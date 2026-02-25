test_that("exhaustive returns expected structure", {
  set.seed(1)
  df <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 2)
  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models", "model", "best_metric", "method"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
  expect_equal(result$method, "exhaustive")
})

test_that("exhaustive finds a better model than intercept-only on mtcars", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 3)
  null_aic <- AIC(lm(mpg ~ 1, data = mtcars))
  expect_lt(result$best_metric, null_aic)
})

test_that("exhaustive with max_terms = 1 evaluates exactly one term per candidate", {
  set.seed(2)
  df <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 1)
  # 2 candidates (x1, x2) + 1 starting model = 3 rows
  expect_equal(nrow(result$all_models), 3)
})

test_that("exhaustive finds the true model on a clear-signal dataset", {
  set.seed(42)
  n  <- 100
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- 3 * x1 - 2 * x2 + rnorm(n, sd = 0.1)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 3)
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
  expect_true("x2" %in% labels)
  expect_false("x3" %in% labels)
})

test_that("exhaustive works with keep_all_vars = TRUE", {
  set.seed(3)
  n  <- 50
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- x1 + x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, max_degree = 2, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 2, keep_all_vars = TRUE)
  expect_type(result, "list")
  # Base terms (x1, x2) should always be present since keep_all_vars = TRUE
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
  expect_true("x2" %in% labels)
})

test_that("exhaustive works with metric_comparison = max", {
  set.seed(4)
  n  <- 50
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- 2 * x1 + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  # R-squared: higher is better
  rsq <- function(model) summary(model)$r.squared
  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "exhaustive", max_terms = 2,
                 metric = rsq, metric_comparison = max)
  expect_type(result, "list")
  # Should find x1 since it's the true predictor
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
})
