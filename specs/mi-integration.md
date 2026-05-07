# Spec: Multiple imputation integration

**Status:** approved 2026-05-07; not yet implemented
**Issue:** FEATURES.md §1
**Branch:** TBD (not on main)
**Future home:** the `mm_*` helpers below are drafted in `modelminer` but
designed to extract cleanly into a separate `micemodel` package. modelminer
itself stays MI-agnostic.

---

## Headline answer

> *Does any of the MI work require package changes, or is it all user-side
> composition on top of the existing hooks?*

Mostly composition. The package gains:

1. **Three named helpers** (`mm_pool_aic`, `mm_pool_mira`, `mm_d1_gate`) —
   one per slot in mine()'s triple. These replace the closure-construction
   boilerplate of TODO Tier 1/2 with named functions, satisfying invariant 6
   ("lambdas are escape hatches, not entry points").
2. **One S3 method** (`mine.mids`) — lets `data` accept a `mids` object so
   the data lives in one place, not closure-baked. mine() itself stays
   MI-agnostic; it only learns one new data shape.

No new arguments on `mine()`, no `pooling = ...` flag, no new search method,
no nested-test mode. The triple stays sacred and pluggable.

---

## Target vignette

### Tier 1 — mean AIC pooling

```r
library(modelminer); library(mice)

imp <- mice(mtcars, m = 10)

mined <- mine(
  data = imp, response_var = mpg,
  model_func = mm_pool_aic(lm)
)
```

Use when: the goal is variable selection that's robust across imputations
under standard AIC. ~m× slower than single-fit `mine()`.

### Tier 2 — D1 gate (nested Wald test)

```r
library(modelminer); library(mice); library(nnet)

imp <- mice(survey_data, m = 10)

mined <- mine(
  data = imp, response_var = vote,
  model_func = mm_pool_mira(nnet::multinom),
  metric = identity,
  metric_comparison = mm_d1_gate
)
```

Use when: a term should only enter the model if it passes the pooled D1
nested Wald test against the current model. More principled than mean-AIC
gating for nested forward selection on imputed data.

---

## New exports

### `mm_pool_aic(model_func)`

**Returns:** a function `function(formula, data)` suitable for mine()'s
`model_func` slot. The returned closure expects `data` to be a `mids` or
`list` of data.frames; it fits `model_func` on each completed dataset and
returns the first fit with `$AIC` overwritten by the mean across the m
fits.

```r
mm_pool_aic <- function(model_func) {
  function(formula, data) {
    datasets <- .as_dataset_list(data)
    fits <- lapply(datasets, \(d) model_func(formula, data = d))
    out <- fits[[1L]]
    out$AIC <- mean(vapply(fits, AIC, numeric(1)))
    out
  }
}
```

Notes:
- `metric = AIC` (the default) reads `$AIC` directly; no metric override
  needed.
- The returned object's other slots come from the *first* imputation's fit.
  This is fine for selection (we only care about AIC) but predictions from
  it would be single-imputation; the user should refit/pool downstream.

### `mm_pool_mira(model_func)`

**Returns:** a function that fits `model_func` on each completed dataset
and returns a `mira` object (via `mice::as.mira(fits)`). Drop into
mine()'s `model_func` slot when paired with `metric = identity` and
`metric_comparison = mm_d1_gate`.

```r
mm_pool_mira <- function(model_func) {
  function(formula, data) {
    datasets <- .as_dataset_list(data)
    fits <- lapply(datasets, \(d) model_func(formula, data = d))
    mice::as.mira(fits)
  }
}
```

### `mm_d1_gate(...)`

**Drops into mine()'s `metric_comparison` slot.** Dispatches on argument
count to handle both call sites in mine()'s search loop:

- **2 args** (`metric_comparison(old, new)` from `.metric_improved`) —
  forward step's nested gate. Runs `mice::D1(new, old)`; returns `new` if
  `P(>F) < 0.05`, else `old`.
- **N args** (`do.call(metric_comparison, candidates)` from
  `.find_best_index`) — non-nested ranking. Returns the candidate with
  lowest mean AIC across imputations.

```r
mm_d1_gate <- function(...) {
  miras <- list(...)
  if (length(miras) == 1L) return(miras[[1L]])
  if (length(miras) == 2L) {
    test <- tryCatch(mice::D1(miras[[2L]], miras[[1L]]),
                     error = function(e) NULL)
    if (!is.null(test) && test$result[1L, "P(>F)"] < 0.05)
      return(miras[[2L]])
    return(miras[[1L]])
  }
  aics <- vapply(miras,
                 \(m) mean(vapply(m$analyses, AIC, numeric(1))),
                 numeric(1))
  miras[[which.min(aics)]]
}
```

The 0.05 threshold is hardcoded in v1. If users want a different alpha,
they wrap `mm_d1_gate` in their own closure or write a custom
`metric_comparison`. (Open question: ship `mm_d1_gate(alpha = 0.05)` as a
factory instead? Defer until someone asks.)

### Internal: `.as_dataset_list(data)`

Coerces accepted shapes into a list of data.frames. Used by both pooling
helpers and by `mine.mids`'s pool-frame extraction.

```r
.as_dataset_list <- function(data) {
  if (inherits(data, "mids")) return(mice::complete(data, action = "all"))
  if (is.list(data) && !is.data.frame(data) &&
      all(vapply(data, is.data.frame, logical(1)))) return(data)
  if (is.data.frame(data)) return(list(data))
  stop("'data' must be a data.frame, mids, or list of data.frames.",
       call. = FALSE)
}
```

---

## mine() shape dispatch

### New S3 method: `mine.mids`

```r
mine.mids <- function(x, response_var, ...) {
  pool_frame <- mice::complete(x, 1L)
  # ... NSE on response_var, then dispatch through a refactored .mine_impl
  # that accepts (data_for_func, pool_frame, response_str, ...)
}
```

### `.mine_impl` refactor

Today: `.mine_impl(data, response_str, ...)` uses `data` for *both* (a)
candidate term pool / NA handling / start-model fitting and (b) passing
through to `model_func`. Those two roles split:

```r
.mine_impl <- function(data, response_str, ..., pool_frame = NULL)
```

- `pool_frame` defaults to `data` for backward compatibility (data.frame
  input).
- `mine.mids` passes `data = imp` and `pool_frame = mice::complete(imp, 1L)`.
- All `is.data.frame(data)` checks become `is.data.frame(pool_frame)`.
- Candidate term pool, numeric-var detection, and the `n / p` AIC warning
  read `pool_frame`.
- `complete.cases()` filtering: skipped when `inherits(data, "mids")`
  (mids has imputed values; no rows to drop).
- Start-model fitting still calls `model_func(formula, data = data)` —
  passes the mids/list straight through to the pooling helper.

This is a contained refactor: one extra parameter, the existing
data.frame path is unchanged.

### Why no `mine.list` (list of data.frames)?

Users with mice imputations have a `mids`. Users with non-mice imputations
(Amelia, jomo, hand-rolled) can wrap their list in a thin shim or, more
simply, build their own pooled model_func and keep `data = first_frame`.
Adding `mine.list` introduces dispatch ambiguity (a data.frame is a list)
without clear payoff. Reconsider if a real Amelia user shows up.

### Formula-path: out of scope for v1

`mine(mpg ~ ., data = imp)` won't work in v1 because dispatch goes through
`mine.formula`, which has `is.data.frame(data)` baked in. Users wanting MI
go through the data-first form: `mine(imp, response_var = mpg, ...)`.
Document this restriction in `?mine` and `?mm_pool_aic`. Lift later if it
matters.

---

## Edge cases & known smells

- **`.find_best_index` uses `identical()` on mira objects.** mira objects
  can be large; pairwise `identical()` to find the winning index is slow.
  Documented in TODO Tier 2 already. Not a blocker — we accept it as a
  cost of mira-as-metric — but flag in the helper docs so users know
  Tier 2 has a runtime smell that Tier 1 doesn't.
- **Memory: m × n_candidates fits per round.** 30 candidates × 10
  imputations = 300 fits per round in Tier 2 (mira retains all m fits).
  Tier 1 is friendlier (each candidate is one merged fit, not m).
- **D1 alpha hardcoded at 0.05** — see `mm_d1_gate` notes above.
- **D1 errors gracefully fall back to keeping `old`.** If `mice::D1`
  errors (singular, missing pkg, etc.), the gate refuses to add the term.
  Logged once per session via `warning()`? Or silent? Default to silent
  in v1; revisit if users report selection stalling mysteriously.
- **`mm_pool_aic` returns a single fit object whose `$AIC` is mean
  across imputations.** Other slots (`$coefficients`, `$residuals`) are
  from the first imputation only. Document this loudly: the returned
  object is for selection, not inference.

---

## Test plan

`tests/testthat/test-mi.R`:

1. **`mm_pool_aic` end-to-end.** Build a small mids from `mtcars` with
   synthetic NAs (`mice::ampute()` or hand-NA). `mine(imp, response_var
   = mpg, model_func = mm_pool_aic(lm))` returns a `mine` object with a
   non-NA formula and finite metric.
2. **`mm_pool_mira` + `mm_d1_gate` end-to-end.** Same setup; assert
   `class(mined$model)` is from the user's model_func family (not mira),
   since `.mine_impl` refits the best formula via `model_func` for the
   final return — *check this assumption against current
   `.mine_impl` refit logic*.
3. **`mm_d1_gate` 1/2/N arg dispatch.** Unit tests for the three branches
   in isolation, using small precomputed mira lists.
4. **`.as_dataset_list` accepts data.frame, mids, list-of-frames; rejects
   other shapes.**
5. **Backward compat: existing `mine(data, response_var)` data.frame path
   unchanged.** Run a representative subset of the existing test suite to
   confirm no behavioural drift after `.mine_impl` refactor.
6. **Pool-frame NA handling.** With a mids, `complete.cases` filtering is
   skipped — assert no warning fires.

`mice` enters Suggests in DESCRIPTION; tests use `skip_if_not_installed`.

---

## Out of scope

- **Tier 3** (first-class `datasets` arg with `pooling = "aic" | "d1"`) —
  rejected; conflicts with "mine() stays MI-agnostic."
- **Tier 4** (dedicated `comparison_mode = "nested_test"` code path) —
  unnecessary; Tier 2 achieves the same via the existing triple.
- **Pooled `predict()` / `coef()` on the returned `mine` object.** The
  selected formula is the deliverable; pooled inference is downstream
  user work.
- **Amelia / jomo / generic list-of-frames as a first-class shape.**
- **Bayesian alternatives to D1** (D2, D3, BLASSO posterior-based gating).
  See FEATURES.md §2 for BLASSO; these may interact later.

---

## Migration path to `micemodel`

The three `mm_*` helpers are the natural extraction unit. When `micemodel`
ships:

1. Move `mm_pool_aic`, `mm_pool_mira`, `mm_d1_gate`, and
   `.as_dataset_list` to `micemodel`.
2. modelminer keeps `mine.mids` (it's a dispatch concern, not a pooling
   concern) and gains `Suggests: micemodel`.
3. Vignette updates to `library(micemodel)`. Helper signatures are
   unchanged.

Designed up front so the v1 helpers don't accumulate modelminer-specific
internals that block extraction.
