test_that("lm mine doesn't fail", {
  expect_no_error(mine(mtcars, mpg))
})

test_that("mine returns expected structure", {
  result <- mine(mtcars, mpg)
  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models", "model", "best_metric", "method"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
  expect_true("Formula" %in% names(result$all_models))
  expect_true("Metric" %in% names(result$all_models))
})

test_that("null model is included in all_models", {
  result <- mine(mtcars, mpg)
  expect_equal(result$all_models$Formula[1], "mpg ~ 1")
})

test_that("mine selects a better model than intercept-only", {
  result <- mine(mtcars, mpg)
  null_aic <- result$all_models$Metric[[1]]
  best_aic <- AIC(lm(result$Formula, data = mtcars))
  expect_lt(best_aic, null_aic)
})

test_that("keep_all_vars starts with all first-order terms", {
  result <- mine(mtcars, mpg, keep_all_vars = TRUE)
  first_formula <- result$all_models$Formula[1]
  predictor_vars <- setdiff(names(mtcars), "mpg")
  for (v in predictor_vars) {
    expect_true(grepl(v, first_formula))
  }
})

test_that("polynomial terms not generated for factor columns", {
  df <- mtcars
  df$cyl <- as.factor(df$cyl)
  expect_no_error(mine(df, mpg, max_degree = 2, max_interact_vars = 1))
  result <- mine(df, mpg, max_degree = 2, max_interact_vars = 1)
  poly_terms <- grep("I\\(cyl\\^", result$all_models$Formula, value = TRUE)
  expect_length(poly_terms, 0)
})

test_that("mine makes more than one greedy step on a simple dataset", {
  set.seed(42)
  n <- 50
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- 2 * x1 + 3 * x2 + rnorm(n, sd = 0.1)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, max_degree = 1, max_interact_vars = 1)
  final_labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% final_labels)
  expect_true("x2" %in% final_labels)
})

test_that("metric_comparison = max works (e.g. log-likelihood)", {
  log_lik <- function(m) as.numeric(logLik(m))
  result <- mine(mtcars, mpg, metric = log_lik, metric_comparison = max,
                 max_degree = 1, max_interact_vars = 1)
  null_ll <- log_lik(lm(mpg ~ 1, data = mtcars))
  best_ll  <- log_lik(lm(result$Formula, data = mtcars))
  expect_gt(best_ll, null_ll)
})
