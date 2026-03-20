# Tests for argument-method compatibility warnings

# Helper function to suppress only the AIC small-sample warning
suppress_aic_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("AIC may be unreliable", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

test_that("metric with lasso warns about cv.glmnet-driven selection", {
  skip_if_not_installed("glmnet")
  expect_warning(
    mine(mtcars, mpg, method = "lasso", metric = BIC,
         max_degree = 1, max_interact_vars = 1, verbose = FALSE),
    "cv.glmnet's internal CV error"
  )
})

test_that("metric with lasso_path does NOT warn (metric is used)", {
  skip_if_not_installed("glmnet")
  expect_no_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "lasso_path", metric = BIC,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    )
  )
})

test_that("metric_comparison with lasso warns", {
  skip_if_not_installed("glmnet")
  expect_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "lasso", metric_comparison = max,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    ),
    "does not use metric_comparison"
  )
})

test_that("lambda_rule with non-lasso method warns", {
  expect_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "greedy", lambda_rule = "lambda.1se",
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    ),
    "lambda_rule is only used by"
  )
})

test_that("lambda_rule default with non-lasso does NOT warn", {
  expect_no_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "greedy",
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    )
  )
})

test_that("max_terms with non-exhaustive method warns", {
  expect_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "greedy", max_terms = 3,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    ),
    "max_terms is only used by"
  )
})

test_that("max_terms with exhaustive does NOT warn", {
  expect_no_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "exhaustive", max_terms = 3,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    )
  )
})

test_that("keep_all_vars = FALSE with backward warns", {
  expect_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "backward", keep_all_vars = FALSE,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    ),
    "keep_all_vars = FALSE is overridden"
  )
})

test_that("keep_all_vars = TRUE with backward does NOT warn about override", {
  # Should not get the keep_all_vars override warning (may still get
  # the backward-unused-candidates warning if max_degree/max_interact_vars
  # produce candidates, so we only check the specific message is absent).
  expect_no_error(
    withCallingHandlers(
      mine(mtcars, mpg, method = "backward", keep_all_vars = TRUE,
           max_degree = 1, max_interact_vars = 1, verbose = FALSE),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("keep_all_vars = FALSE is overridden", msg))
          stop("Unexpected keep_all_vars warning fired")
        invokeRestart("muffleWarning")
      }
    )
  )
})

test_that("default args with greedy produce no compatibility warnings", {
  expect_no_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "greedy",
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    )
  )
})

test_that("lasso with default metric/metric_comparison does NOT warn", {
  skip_if_not_installed("glmnet")
  expect_no_warning(
    suppress_aic_warning(
      mine(mtcars, mpg, method = "lasso",
           max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    )
  )
})
