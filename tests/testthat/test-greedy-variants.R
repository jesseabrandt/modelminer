# ---- greedy_star tests ----

test_that("greedy_star returns expected structure", {
  result <- mine(mtcars, mpg, method = "greedy_star",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("Formula", "all_models", "model", "best_metric", "method") %in% names(result)))
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "greedy_star")
})

test_that("greedy_star improves over intercept-only", {
  result <- mine(mtcars, mpg, method = "greedy_star",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  null_aic <- AIC(lm(mpg ~ 1, data = mtcars))
  expect_lt(result$best_metric, null_aic)
})

test_that("greedy_star with interactions uses * bundling", {
  result <- mine(mtcars, mpg, method = "greedy_star",
                 max_degree = 1, max_interact_vars = 2, verbose = FALSE)
  expect_s3_class(result$Formula, "formula")
  expect_gte(nrow(result$all_models), 2)
})

# ---- greedy_alt tests ----

test_that("greedy_alt returns expected structure", {
  result <- mine(mtcars, mpg, method = "greedy_alt",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("Formula", "all_models", "model", "best_metric", "method") %in% names(result)))
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "greedy_alt")
})

test_that("greedy_alt improves over intercept-only", {
  result <- mine(mtcars, mpg, method = "greedy_alt",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  null_aic <- AIC(lm(mpg ~ 1, data = mtcars))
  expect_lt(result$best_metric, null_aic)
})

test_that("greedy_alt with polynomials finds quadratic terms", {
  set.seed(1)
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- rnorm(n)
  y <- x1 + 3 * x1^2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, method = "greedy_alt",
                 max_degree = 2, max_interact_vars = 1, verbose = FALSE)
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
  expect_true("I(x1^2)" %in% labels)
})

# ---- greedy_alt_full tests ----

test_that("greedy_alt_full returns expected structure", {
  result <- mine(mtcars, mpg, method = "greedy_alt_full",
                 max_degree = 1, max_interact_vars = 2, verbose = FALSE)
  expect_type(result, "list")
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "greedy_alt_full")
})

# ---- greedy_alt_fb tests ----

test_that("greedy_alt_fb returns expected structure", {
  result <- mine(mtcars, mpg, method = "greedy_alt_fb",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  expect_type(result, "list")
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "greedy_alt_fb")
})

test_that("greedy_alt_fb improves over intercept-only", {
  result <- mine(mtcars, mpg, method = "greedy_alt_fb",
                 max_degree = 1, max_interact_vars = 1, verbose = FALSE)
  null_aic <- AIC(lm(mpg ~ 1, data = mtcars))
  expect_lt(result$best_metric, null_aic)
})

# ---- greedy_alt_full_fb tests ----

test_that("greedy_alt_full_fb returns expected structure", {
  result <- mine(mtcars, mpg, method = "greedy_alt_full_fb",
                 max_degree = 1, max_interact_vars = 2, verbose = FALSE)
  expect_type(result, "list")
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "greedy_alt_full_fb")
})

# ---- cross-method consistency ----

test_that("greedy_alt variants all find at least one predictor on mtcars", {
  methods <- c("greedy_alt", "greedy_alt_full",
               "greedy_alt_fb", "greedy_alt_full_fb", "greedy_star")
  for (m in methods) {
    result <- mine(mtcars, mpg, method = m,
                   max_degree = 1, max_interact_vars = 1, verbose = FALSE)
    labels <- attr(terms(result$Formula), "term.labels")
    expect_true(length(labels) >= 1,
                label = paste0(m, " should select at least one predictor"))
  }
})
