# Tests for method-specific wrapper functions (mine_greedy, mine_lasso, etc.)

# Wrappers return classed "mine" objects. Both new and legacy field names are
# populated so existing code that indexed `$Formula` / `$all_models` keeps
# working alongside `$formula` / `$trace`.
legacy_names <- c("Formula", "all_models", "model", "best_metric", "method")

expect_mine_object <- function(result) {
  expect_s3_class(result, "mine")
  expect_true(all(legacy_names %in% names(result)))
}

# -- mine_greedy -----------------------------------------------------------

test_that("mine_greedy returns expected structure and method tag", {
  result <- mine_greedy(mtcars, mpg, verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "greedy")
})

test_that("mine_greedy matches mine(method = 'greedy')", {
  set.seed(1)
  a <- mine(mtcars, mpg, method = "greedy", verbose = FALSE)
  set.seed(1)
  b <- mine_greedy(mtcars, mpg, verbose = FALSE)
  expect_equal(deparse1(a$Formula), deparse1(b$Formula))
  expect_equal(a$best_metric, b$best_metric)
})

test_that("mine_greedy variant argument works", {
  result <- mine_greedy(mtcars, mpg, variant = "greedy_star", verbose = FALSE)
  expect_equal(result$method, "greedy_star")
})

test_that("mine_greedy rejects invalid variant", {
  expect_error(mine_greedy(mtcars, mpg, variant = "bogus"))
})

# -- mine_forward_backward -------------------------------------------------

test_that("mine_forward_backward returns expected structure", {
  result <- mine_forward_backward(mtcars, mpg, max_degree = 1,
                                  max_interact_vars = 1, verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "forward_backward")
})

test_that("mine_forward_backward matches mine(method = 'forward_backward')", {
  set.seed(2)
  a <- mine(mtcars, mpg, method = "forward_backward",
            max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  set.seed(2)
  b <- mine_forward_backward(mtcars, mpg,
                             max_degree = 1, max_interact_vars = 1,
                             verbose = FALSE)
  expect_equal(deparse1(a$Formula), deparse1(b$Formula))
})

# -- mine_backward ---------------------------------------------------------

test_that("mine_backward returns expected structure", {
  result <- mine_backward(mtcars, mpg, verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "backward")
})

test_that("mine_backward does not expose max_degree or keep_all_vars", {
  # Signature should not accept these; calling with them should error
  expect_error(mine_backward(mtcars, mpg, max_degree = 3))
  expect_error(mine_backward(mtcars, mpg, keep_all_vars = TRUE))
})

# -- mine_exhaustive -------------------------------------------------------

test_that("mine_exhaustive returns expected structure", {
  result <- mine_exhaustive(mtcars[, c("mpg", "wt", "cyl", "hp")], mpg,
                            max_terms = 2, max_degree = 1,
                            max_interact_vars = 1, verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "exhaustive")
})

test_that("mine_exhaustive max_terms defaults to 5", {
  # Should not error; default is 5, not NULL
  result <- mine_exhaustive(mtcars[, c("mpg", "wt", "cyl")], mpg,
                            max_degree = 1, max_interact_vars = 1,
                            verbose = FALSE)
  expect_type(result, "list")
})

# -- mine_lasso ------------------------------------------------------------

test_that("mine_lasso returns expected structure", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "lasso")
})

test_that("mine_lasso forwards ... to cv.glmnet", {
  skip_if_not_installed("glmnet")
  # alpha = 0 gives ridge; should still work
  result <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       alpha = 0, verbose = FALSE)
  expect_type(result, "list")
})

test_that("mine_lasso lambda_rule argument works", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       lambda_rule = "lambda.1se", verbose = FALSE)
  expect_type(result, "list")
})

test_that("mine_lasso matches mine(method = 'lasso')", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  a <- mine(mtcars, mpg, method = "lasso", max_degree = 1,
            max_interact_vars = 1, verbose = FALSE)
  set.seed(42)
  b <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                  verbose = FALSE)
  # Selection is lambda-driven, so the chosen formula is identical even though
  # the wrapper supplies no metric.
  expect_equal(deparse1(a$Formula), deparse1(b$Formula))
})

test_that("mine_lasso takes no metric: best_metric is NA, CV error in trace", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       verbose = FALSE)
  # No metric drives lasso, so best_metric is NA (not a faked AIC/CV value).
  expect_true(is.na(result$best_metric))
  # The trace's lambda.min / lambda.1se rows carry glmnet's CV error.
  expect_s3_class(result$trace, "data.frame")
  expect_true(any(is.finite(result$trace$Metric)))
})

test_that("mine_lasso returns the cv.glmnet object as $selector_fit", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       verbose = FALSE)
  expect_s3_class(result$selector_fit, "cv.glmnet")
  # CV error is recoverable from it -- no need to stuff it into best_metric.
  expect_true(is.numeric(result$selector_fit$cvm))
})

test_that("mine_lasso has no metric/metric_comparison arguments", {
  expect_false("metric" %in% names(formals(mine_lasso)))
  expect_false("metric_comparison" %in% names(formals(mine_lasso)))
})

test_that("mine_lasso (metric = NULL) survives the single-predictor fallback", {
  skip_if_not_installed("glmnet")
  # A model_func that rejects the intercept-only starting model forces
  # .mine_impl into its single-predictor fallback. With no metric (the
  # mine_lasso() default) that branch must not call metric() and crash.
  needs_predictor <- function(formula, data, ...) {
    if (length(attr(stats::terms(formula), "term.labels")) == 0L)
      stop("model requires at least one predictor")
    lm(formula, data = data, ...)
  }
  expect_no_error(
    result <- mine_lasso(mtcars, mpg, model_func = needs_predictor,
                         max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  )
  expect_s3_class(result$selector_fit, "cv.glmnet")
  expect_true(is.na(result$best_metric))
})

# -- mine_lasso_path -------------------------------------------------------

test_that("mine_lasso_path returns expected structure", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso_path(mtcars, mpg, max_degree = 1,
                            max_interact_vars = 1, verbose = FALSE)
  expect_mine_object(result)
  expect_equal(result$method, "lasso_path")
})

test_that("mine_lasso_path forwards ... to glmnet", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso_path(mtcars, mpg, max_degree = 1,
                            max_interact_vars = 1, alpha = 0.5,
                            verbose = FALSE)
  expect_type(result, "list")
})

test_that("mine_lasso_path matches mine(method = 'lasso_path')", {
  skip_if_not_installed("glmnet")
  set.seed(99)
  a <- mine(mtcars, mpg, method = "lasso_path", max_degree = 1,
            max_interact_vars = 1, verbose = FALSE)
  set.seed(99)
  b <- mine_lasso_path(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                       verbose = FALSE)
  expect_equal(deparse1(a$Formula), deparse1(b$Formula))
})

test_that("mine_lasso_path returns the glmnet object as $selector_fit", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso_path(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                            verbose = FALSE)
  expect_s3_class(result$selector_fit, "glmnet")
  # metric IS load-bearing here, so best_metric remains a real value.
  expect_true(is.numeric(result$best_metric))
})
