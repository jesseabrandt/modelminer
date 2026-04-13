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
