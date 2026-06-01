# modelminer 0.2.0

## Breaking changes

* `mine_lasso()` no longer takes `metric` or `metric_comparison`. Lasso selects
  variables via the cross-validated penalty (lambda), not by optimising a
  metric, so those arguments never drove its decisions (`metric_comparison` was
  inert; `metric` only scored an informational refit). The wrapper now takes
  neither. On a lasso fit, `$best_metric` is `NA` (no metric drives selection) —
  compute any metric yourself from the returned model, e.g.
  `AIC(extract_model(fit))`. `mine(method = "lasso", metric = ...)` is
  unchanged; only the `mine_lasso()` convenience wrapper changed.

## Features

* New `$selector_fit` field on the `"mine"` object holds the underlying
  selection engine when the method has one: a `cv.glmnet` object for `"lasso"`,
  a `glmnet` object for `"lasso_path"` (`NULL` for stepwise methods). The CV
  error curve and full lambda path are recoverable from it.
* New `from_slot()` metric helper for extracting model-embedded fit
  statistics by slot name (composes with `extract_metric()`).
* New `method = "none"` mode builds the engineered candidate pool
  (first-order predictors + polynomial terms + interactions) and fits the
  full generated formula once, running no search. An escape hatch for users
  who want modelminer's feature engineering but their own selection method.
  The returned object carries the new `$candidate_terms` field (the full
  pool); `print()`/`summary()` degrade gracefully for the no-search case.

# modelminer 0.1.1

## Features

* `mine()` is now an S3 generic with `mine.formula(formula, data, ...)` and
  `mine.data.frame(data, response_var, ...)` methods. The formula-first form
  matches the standard R modelling contract (`lm`, `glm`, ...); the data-first
  form preserves the original NSE interface. The pipe form
  `data |> mine(y ~ x)` is auto-routed to the formula method.
* Return value is now an S3 object of class `"mine"` with methods for
  `print()`, `summary()`, `coef()`, `predict()`, `formula()`, and `plot()`.
* New `extract_model()` generic returns the underlying fitted model object.
* `print()`/`summary()` now show informative `Call:` lines -- the outer call
  reflects the user's invocation, and the fitted model's stored call is
  rewritten to reference the selected formula and the user's data (e.g.
  `lm(formula = mpg ~ wt + cyl, data = mtcars)`).
* New vignette `mine-fit-object` walks through the three call forms and the
  S3 methods.
* Requires R (>= 4.1): `mine()`'s pipe form and the documented examples use the
  base pipe `|>`, introduced in R 4.1.

## Deprecations

* The canonical fields on a `"mine"` result are now `$formula` and `$trace`.
  The legacy names `$Formula` and `$all_models` -- from 0.1.0, when `mine()`
  returned a plain `list(Formula, all_models)` -- are still populated this
  release but are **deprecated**: accessing them with `$` emits a one-time
  warning and they will be removed in a future version. Switch to `$formula`
  and `$trace`.

# modelminer 0.1.0

Initial CRAN release.

## Features

* `mine()` dispatcher with configurable stepwise model selection over polynomial
  and interaction terms.
* Search algorithms: greedy forward (`"greedy"`), forward-backward
  (`"forward_backward"`), backward elimination (`"backward"`), exhaustive subset
  (`"exhaustive"`), and L1-regularised selection (`"lasso"`, `"lasso_path"`).
* Convenience wrappers: `mine_greedy()`, `mine_forward_backward()`,
 `mine_backward()`, `mine_exhaustive()`, `mine_lasso()`, `mine_lasso_path()`.
* `compare_methods()` for running multiple configurations side-by-side.
* `formula_wrap()` adapter for matrix-based model functions (e.g., glmnet).
* Metric helpers: `extract_metric()` S3 generic with methods for rpart, tree,
  ranger, randomForest, gbm, and cv.glmnet; `lm_loocv()` for analytical LOOCV;
  `make_cv_metric()` for k-fold CV; `make_cp_metric()` for Mallow's Cp.
* `list_metrics()` for interactive inspection of model-embedded metrics.
* Four vignettes: getting started, algorithm comparison, lasso selection,
  non-linear models.
