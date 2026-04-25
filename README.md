# modelminer

An R package for automated model selection with feature engineering. Starting from an intercept-only (or all-first-order) model, `mine()` searches through polynomial terms and interactions to find the formula that best optimizes a user-supplied metric.

## Installation

```r
# From source (requires the remotes package)
remotes::install_git("https://github.com/jesseabrandt/modelminer.git")
# Codeberg mirror: https://codeberg.org/jesseabrandt/modelminer.git

# Or load locally during development
pkgload::load_all()
```

## Quick Start

```r
library(modelminer)

# Basic usage: greedy forward selection with AIC
result <- mine(mtcars, mpg)

result$Formula      # best formula found
result$best_metric  # AIC of the best model
result$model        # fitted model object, ready for summary(), predict(), etc.
```

> **Planned:** a formula-based wrapper — something like `miner(y ~ x, data)` — that meets R's standard model contract (returns an S3 object with `predict()`, `summary()`, `print()` methods). Not yet implemented.

## How It Works

`mine()` builds a candidate pool from:
- All first-order predictor terms
- Polynomial terms up to `max_degree` (e.g. `I(wt^2)`, `I(wt^3)`)
- Interaction terms up to `max_interact_vars` variables (e.g. `hp:wt`)

It then searches this pool using a configurable strategy -- greedy forward selection, phased variants, forward-backward, pure backward elimination, or exhaustive best-subset -- adding or removing one term per step based on a model selection metric.

## Search Algorithms

Set `method` to choose the search strategy:

```r
# Greedy forward selection (default) -- fast, path-dependent
result <- mine(mtcars, mpg, method = "greedy")

# Greedy forward using * for interactions (bundles main effects with interaction)
result <- mine(mtcars, mpg, method = "greedy_star")

# Phased greedy: first-order -> polynomial -> interactions (selected terms only)
result <- mine(mtcars, mpg, method = "greedy_alt")

# Phased greedy, but phase 3 considers interactions among all predictors
result <- mine(mtcars, mpg, method = "greedy_alt_full")

# greedy_alt with forward-backward within each phase
result <- mine(mtcars, mpg, method = "greedy_alt_fb")

# greedy_alt_full with forward-backward within each phase
result <- mine(mtcars, mpg, method = "greedy_alt_full_fb")

# Forward step then backward step each round over a single pool
result <- mine(mtcars, mpg, method = "forward_backward")

# Pure backward elimination from all first-order predictors
result <- mine(mtcars, mpg, method = "backward")

# Exhaustive best-subset search (slower; guaranteed optimal within max_terms)
result <- mine(mtcars, mpg, method = "exhaustive", max_terms = 5)

# Pass a custom search function for experimental algorithms
my_search <- function(candidate_terms, current_formula, current_metric,
                      results, model_func, metric, metric_comparison, data) {
  # ... return list(Formula = ..., all_models = ...)
}
result <- mine(mtcars, mpg, method = my_search)
```

## Custom Metrics and Models

Any model function that accepts `formula` and `data` arguments works out of the box:

```r
# Binomial GLM
result <- mine(
  data         = mtcars,
  response_var = am,
  model_func   = \(formula, data) glm(formula, data, family = binomial),
  metric       = AIC
)
```

For matrix-based model functions (e.g. `glmnet`), use `formula_wrap()` to add a formula interface:

```r
library(glmnet)

# formula_wrap returns a closure that extracts x and y from a data frame
cv_glmnet <- formula_wrap(glmnet::cv.glmnet)

result <- mine(
  data          = mtcars,
  response_var  = mpg,
  model_func    = cv_glmnet,
  metric        = extract_metric,   # uses min(cvm) for cv.glmnet
  keep_all_vars = TRUE              # cv.glmnet needs at least one predictor
)
```

## Metric Helpers

### Model-embedded metrics

Many model types store their own fit metrics (OOB error, cross-validated loss, etc.) rather than supporting `AIC()`. Use `extract_metric()` as a drop-in `metric` argument:

| Model class     | What `extract_metric()` returns              |
|-----------------|----------------------------------------------|
| `lm`, `glm`, ...| `AIC()` (default fallback)                   |
| `cv.glmnet`     | `min(cvm)` -- minimum mean CV error          |
| `ranger`        | `prediction.error` -- OOB prediction error   |
| `randomForest`  | Last OOB error rate or MSE                   |
| `rpart`         | `min(cptable[,"xerror"])` -- min CV error    |
| `tree`          | Residual deviance                             |
| `GBMFit` (gbm)  | `min(cv.error)`, falls back to train error   |

All methods return lower-is-better values, consistent with `mine()`'s default `metric_comparison = min`.

Use `list_metrics()` to inspect what a model object exposes before deciding whether the default is right for your use case:

```r
m <- rpart::rpart(mpg ~ ., data = mtcars)
list_metrics(m)
```

To extend `extract_metric` to a new class:

```r
extract_metric.my_class <- function(model, ...) model$my_loss_slot
```

### Cross-validation and Cp metrics

```r
# k-fold cross-validated MSE (works with any lm-compatible model)
cv5 <- make_cv_metric(k = 5, seed = 42)
result <- mine(mtcars, mpg, metric = cv5)

# Mallow's Cp (requires a full reference model)
full <- lm(mpg ~ ., data = mtcars)
cp <- make_cp_metric(full)
result <- mine(mtcars, mpg, metric = cp)

# Leave-one-out CV for lm models (fast, closed-form via hat matrix)
result <- mine(mtcars, mpg, metric = lm_loocv)
```

## Comparing Configurations

`compare_methods()` runs multiple `mine()` configurations and returns a tidy summary:

```r
cmp <- compare_methods(
  mtcars, mpg,
  configs = list(
    greedy_d1 = list(method = "greedy",            max_degree = 1),
    greedy_d3 = list(method = "greedy",            max_degree = 3),
    fwd_bwd   = list(method = "forward_backward",  max_degree = 1),
    backward  = list(method = "backward",          max_degree = 1)
  ),
  max_interact_vars = 1
)

cmp$summary     # data frame: Config, Formula, BestMetric, ModelsEvaluated
cmp$details     # named list of full mine() results per config
```

## Key Arguments

| Argument           | Default     | Description                                               |
|--------------------|-------------|-----------------------------------------------------------|
| `model_func`       | `lm`        | Any function accepting `formula` and `data`               |
| `max_degree`       | `3`         | Max polynomial degree for numeric predictors              |
| `max_interact_vars`| `2`         | Max variables per interaction term                        |
| `metric`           | `AIC`       | Function mapping a model to a numeric score               |
| `metric_comparison`| `min`       | `min` for lower-is-better, `max` for higher-is-better     |
| `keep_all_vars`    | `FALSE`     | Start from all first-order terms instead of intercept-only|
| `method`           | `"greedy"`  | Search algorithm (see above) or a custom function         |
| `max_terms`        | `NULL` (5)  | Max subset size for `method = "exhaustive"`               |

## Return Value

`mine()` returns a named list:

- `$Formula` -- best formula found, as a `formula` object
- `$all_models` -- data frame of every formula evaluated and its metric
- `$model` -- fitted model for the best formula (ready for `summary()`, `predict()`, etc.)
- `$best_metric` -- numeric metric value for the best formula
- `$method` -- search algorithm used (`"greedy"`, `"backward"`, `"custom"`, etc.)

## Development

```r
pkgload::load_all()          # load without installing
roxygen2::roxygenise()       # regenerate NAMESPACE and man/
testthat::test_dir("tests/testthat")  # run tests
rcmdcheck::rcmdcheck()       # full R CMD CHECK
```
