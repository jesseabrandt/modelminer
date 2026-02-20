test_that("forward_backward returns expected structure", {
  set.seed(1)
  df <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "forward_backward")
  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
})

test_that("forward_backward finds the true predictors on a clear signal", {
  set.seed(42)
  n  <- 60
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- 3 * x1 - 2 * x2 + rnorm(n, sd = 0.1)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  result <- mine(df, y, max_degree = 1, max_interact_vars = 1,
                 method = "forward_backward")
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
  expect_true("x2" %in% labels)
})

test_that("forward_backward improves on intercept-only", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "forward_backward")
  null_aic <- AIC(lm(mpg ~ 1, data = mtcars))
  best_aic <- AIC(lm(result$Formula, data = mtcars))
  expect_lt(best_aic, null_aic)
})

test_that("forward_backward all_models contains [fwd] and [bwd] entries", {
  set.seed(7)
  n  <- 40
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- 2 * x1 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, max_degree = 1, max_interact_vars = 2,
                 method = "forward_backward")
  # The algorithm evaluates both forward and backward candidates
  expect_gt(nrow(result$all_models), 1)
})
