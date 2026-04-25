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

## Backward compatibility

Old field names are preserved: `fit$Formula` and `fit$all_models` still work
alongside `fit$formula` and `fit$trace`. Existing code requires no changes.

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
