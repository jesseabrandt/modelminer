# modelminer — In-flight features

Specs for work targeted at the next release (post-0.1.1). Each section is a
planning sketch, not a final design — vignette-first design happens before
internals.

Order of work (most recent first):

1. **Active:** Arbitrary-slot metric helper (#3)
2. BLASSO search method with standard errors (#2)
3. Multiple imputation integration (#1)
4. Forced ("sticky") variables the search can't drop (#4)

---

## 1. Multiple imputation integration

**Status:** spec approved 2026-05-07 — see
[`specs/mi-integration.md`](specs/mi-integration.md). Headline answer to
"does this require package changes": mostly no. Three named helpers
(`mm_pool_aic`, `mm_pool_mira`, `mm_d1_gate`) plus one S3 method
(`mine.mids`) so `data` accepts a `mids` directly. mine() stays
MI-agnostic; helpers are designed to extract cleanly into a future
`micemodel` package.

---

## 2. BLASSO as a search method, with standard errors

**Intent:** add a Bayesian LASSO search method alongside the existing
`mine_lasso()` family. Unlike frequentist LASSO, BLASSO produces a posterior
over coefficients, which means **per-term standard errors fall out
naturally** — that is the headline feature, not just "another regularised
selector."

**Output shape (sketch):**

- selected formula (as today)
- per-term posterior summary: mean, SE, credible interval
- attached to the returned `mine` object so existing print/summary methods
  keep working

**Open questions:**

- Backend: `monomvn::blasso`? `bayesreg`? something hand-rolled on top of
  `rstanarm`? Cost of MCMC vs. how `mine()` calls the search per round
  matters here.
- How does a posterior-based selector fit the current
  `(candidate_terms, current_formula, current_metric, ...)` search-function
  signature? Probably one shot rather than stepwise — closer to
  `mine_lasso()`'s shape than `mine_greedy()`'s.
- Should standard errors be exposed via a new accessor (`coef_se()`?) or
  baked into a richer `summary.mine` print method?

---

## 3. Arbitrary-slot metric helper *(active)*

**Intent:** sugar for the common case of "use `model$some_slot` as the
metric" — the pattern currently documented in `R/metrics.R` as
`metric = \(m) m$my_loss_slot`.

**Current state in `R/metrics.R`:**

- `extract_metric()` — S3 generic with opinionated per-class defaults
  (`cv.glmnet`, `ranger`, `randomForest`, `rpart`, `tree`, `gbm`).
- `list_metrics()` — interactive inspector for slots on a fitted model.
- Purpose-specific helpers: `lm_loocv`, `make_cv_metric`, `make_cp_metric`.
- **Missing:** a thin factory for "just pull this slot," so users don't have
  to write the lambda by hand.

**Sketch:**

```r
metric = from_slot("cv_error")             # \(m) m$cv_error
metric = from_slot("cvm", reduce = min)    # \(m) min(m$cvm)
metric = from_slot("cptable", col = "xerror", reduce = min)
```

**Open questions:**

- Name: `from_slot()`, `slot_metric()`, `model_slot()`, ...? (UX call.)
- Should it validate the slot exists at first call, or wait until `mine()`
  runs the model and fails naturally?
- How does it interact with `extract_metric()` — is `from_slot()` what users
  reach for *before* writing a custom `extract_metric.<class>()` method?

---

## 4. Forced ("sticky") variables the search can't drop

**Intent:** let the user name predictors that **must** appear in every
candidate model — the search algorithm can add other terms around them but
cannot remove them. Common in applied work: theory-mandated controls
(year/firm fixed effects, treatment indicator), or variables a collaborator
insists on.

**Sketch (API):**

```r
mine(y ~ ., data = d, must_include = c("treatment", "year"))
```

**Implementation scope (small — mostly plumbing):**

- **Stepwise (greedy/forward-backward/backward):** seed locked terms into
  the starting formula, then `setdiff()` them out before the backward step
  iterates over removal candidates. The relevant loops are in
  `mine_forward_backward.R` and `mine_backward.R` — one filter line each.
- **Lasso methods (`mine_lasso.R`, etc.):** `glmnet` has `penalty.factor` —
  a per-coefficient penalty multiplier. Setting `penalty.factor[locked] = 0`
  leaves those vars unpenalised, which both keeps them in the model and
  removes the shrinkage bias for them. Pass-through.
- **Validation:** locked names must exist in `data` and not collide with the
  response.

**Open questions:**

- Argument name: `must_include`, `force_in`, `lock`, `always`, ...? UX call;
  `must_include` reads most clearly at the call site.
- Do locked variables count against `max_interact_vars` / `max_degree`
  budgets, or are they orthogonal? (Probably orthogonal — they're
  user-asserted, not search-discovered.)
- Should locked terms be allowed to be polynomials/interactions
  (`I(x^2)`, `a:b`), or first-order only? First-order is simpler; richer
  forms can come later if needed.
- For BLASSO (#2): same pattern — locked vars get prior variance set to
  effectively unpenalised. Worth designing #2's API so #4 slots in
  cleanly.
