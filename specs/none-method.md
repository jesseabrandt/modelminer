# Spec — `method = "none"`: candidate-generation escape hatch

**Status:** approved 2026-06-05 (design via Remote Control). Target: dev.

## Problem

`modelminer`'s value is two-sided: (1) it *generates* an engineered candidate
pool — first-order predictors + `I(x^k)` polynomial expansions + `:`
interactions — and (2) it *searches* that pool for a good model. Today the two
are welded together inside `mine()`: there is no way to get the generated terms
out without running a search.

A user who wants the feature-engineering but **not** our search (they have their
own selection method — penalised regression, domain knowledge, a different
package) currently has to reimplement the polynomial/interaction expansion by
hand. The interaction- and polynomial-formula generation should be available as
an **output**.

## Solution

Add a no-search mode: `mine(..., method = "none")`.

It builds the candidate pool exactly as the search methods do (honouring
`max_degree`, `max_interact_vars`, and formula scoping), assembles the full
generated formula, fits it once with `model_func`, and returns — **without
running any search**. The returned object is a normal `"mine"` object minus the
parts a no-search run cannot have (the search `$trace`).

### Target usage (vignette snippet)

```r
# Generate the expanded candidate formula — first-order + polynomials +
# interactions — without running any search. Use it however you like.
fit <- mine(mpg ~ ., data = mtcars, method = "none",
            max_degree = 3, max_interact_vars = 2)

formula(fit)
#> mpg ~ wt + hp + cyl + ... + I(wt^2) + I(hp^2) + ... + wt:hp + wt:cyl + ...

fit$candidate_terms        # the same pool as a character vector
coef(fit)                  # the full model is fitted, so accessors work
lm(formula(fit), mtcars)   # or plug the formula into anything
```

Formula scoping carries over: `mine(mpg ~ wt + hp, d, method = "none")` only
expands `wt` and `hp`.

### Returned object

A `"mine"` object. Compared to a normal search fit:

| Field | `method = "none"` | normal fit |
|---|---|---|
| `$formula` / `$Formula` | full generated formula | selected formula |
| `$candidate_terms` | the term vector (new field) | not present |
| `$model` | fit of the full generated formula | selected model |
| `$best_metric` | metric of that full model | metric of selected model |
| `$method` | `"none"` | e.g. `"greedy"` |
| `$call` | the matched call | the matched call |
| `$trace` / `$all_models` | `NULL` (no search) | search trace |

`coef`, `predict`, `summary`, `plot`, `formula`, `extract_model` all work
because `$model` is populated. `print`/`summary` degrade gracefully: they show
the generated formula and candidate-term count, and omit the "Models evaluated"
line (there was no search).

## Implementation

1. **`.build_candidate_pool()`** — extract the pool-construction block
   (`R/mine.R:421–454`) into an internal helper taking
   `(data, predictor_vars, numeric_vars, max_degree, max_interact_vars)` and
   returning the character vector. Both the search path and the `none` branch
   call it (DRY).
2. **`match.arg`** — add `"none"` to the accepted `method` values in
   `.mine_impl()`.
3. **Branch in `.mine_impl()`** — after the pool is built, if
   `method == "none"`: build the full formula, fit it once via `model_func`,
   compute `best_metric` (tolerant `tryCatch`), and early-return a result list
   carrying `Formula`, `model`, `best_metric`, `candidate_terms`,
   `all_models = NULL`, `method = "none"`. This short-circuits before the
   small-`n` AIC selection warning and the argument-compatibility warning block
   (neither applies to a no-search run). NA-row handling still applies because
   we fit a model.
4. **`.build_mine()`** — carry `$candidate_terms` onto the object when present.
5. **S3 methods** — `print.mine()` / `summary.mine()` branch on
   `method == "none"` for graceful output.
6. **Docs** — document `method = "none"` under `@param method`, the new
   `$candidate_terms` field under `@returns`, and add an example.

## Invariants preserved

- **Triple is sacred** — `model_func`/`metric`/`metric_comparison` still
  pluggable; `none` just skips the search.
- **NSE at the boundary** — no change; `none` rides the existing `mine()`
  dispatch.
- **Main stays CRAN-ready** — work on dev; `method` already accepts strings, so
  adding a value is additive and does not change `mine()`'s formal signature.
- **Variable selection, not hyperparameter tuning** — `none` answers "which
  candidate terms could enter?", which is the term-generation half of selection.

## Out of scope

- No standalone generator function (`expand_terms()` etc.) — the chosen entry
  point is `method = "none"`.
- No change to `mine()`'s formal arguments.
