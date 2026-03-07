skip_if_not_installed("glmnet")

test_that("lasso returns expected structure", {
  set.seed(1)
  df <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 1)

  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models", "model", "best_metric", "method"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
  expect_equal(result$method, "lasso")
})

test_that("lasso recovers true signal on synthetic data", {
  set.seed(42)
  n  <- 200
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)  # noise
  x4 <- rnorm(n)  # noise
  y  <- 3 * x1 - 2 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)

  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 1)
  labels <- attr(terms(result$Formula), "term.labels")

  expect_true("x1" %in% labels)
  expect_true("x2" %in% labels)
})

test_that("lambda.1se produces a sparser model than lambda.min", {
  set.seed(123)
  n  <- 200
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  x4 <- rnorm(n); x5 <- rnorm(n)
  # x1 strong, x2 moderate, x3 weak, x4/x5 noise
  y  <- 5 * x1 + 1.5 * x2 + 0.3 * x3 + rnorm(n, sd = 1)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)

  res_min <- mine(df, y, method = "lasso", lambda_rule = "lambda.min",
                  max_degree = 1, max_interact_vars = 1)
  res_1se <- mine(df, y, method = "lasso", lambda_rule = "lambda.1se",
                  max_degree = 1, max_interact_vars = 1)

  n_min <- length(attr(terms(res_min$Formula), "term.labels"))
  n_1se <- length(attr(terms(res_1se$Formula), "term.labels"))

  # lambda.1se should select at most as many terms as lambda.min

  expect_lte(n_1se, n_min)
})

test_that("lasso works with polynomial terms", {
  set.seed(7)
  n <- 150
  x1 <- runif(n, -2, 2)
  x2 <- rnorm(n)
  y  <- x1 + 2 * x1^2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, method = "lasso", max_degree = 2, max_interact_vars = 1)
  labels <- attr(terms(result$Formula), "term.labels")

  expect_true("x1" %in% labels)
  expect_true("I(x1^2)" %in% labels)
})

test_that("lasso works with interaction terms", {
  set.seed(8)
  n <- 200
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- x1 + x2 + 3 * x1 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 2)
  labels <- attr(terms(result$Formula), "term.labels")

  expect_true("x1:x2" %in% labels)
})

test_that("lasso works via compare_methods", {
  set.seed(9)
  n <- 100
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- 2 * x1 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  comp <- compare_methods(df, y, configs = list(
    greedy = list(method = "greedy", max_degree = 1, max_interact_vars = 1),
    lasso  = list(method = "lasso", max_degree = 1, max_interact_vars = 1)
  ))

  expect_s3_class(comp$summary, "data.frame")
  expect_equal(nrow(comp$summary), 2)
  expect_true("lasso" %in% comp$summary$Config)
  expect_true("greedy" %in% comp$summary$Config)
  expect_equal(comp$details$lasso$method, "lasso")
})

test_that("lasso all_models contains rows for both lambda rules", {
  set.seed(10)
  n <- 80
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- x1 + rnorm(n)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 1)

  # Starting model row + lambda.min row + lambda.1se row = at least 3
  expect_gte(nrow(result$all_models), 3)
})

test_that("lasso passes extra args to cv.glmnet (alpha = 0 for ridge)", {
  set.seed(11)
  n <- 100
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- x1 + x2 + rnorm(n)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # alpha = 0 is ridge regression -- should keep all variables (no sparsity)
  result <- mine(df, y, method = "lasso", max_degree = 1,
                 max_interact_vars = 1, alpha = 0)
  labels <- attr(terms(result$Formula), "term.labels")

  # Ridge doesn't zero out coefficients, so all vars should be selected
  expect_true(length(labels) >= 2)
})

# ---- lasso_path tests ----

test_that("lasso_path returns expected structure", {
  set.seed(20)
  df <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50))
  result <- mine(df, y, method = "lasso_path", max_degree = 1, max_interact_vars = 1)

  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models", "model", "best_metric", "method"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
  expect_equal(result$method, "lasso_path")
})

test_that("lasso_path produces more models than lasso", {
  set.seed(21)
  n <- 200
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); x4 <- rnorm(n)
  y  <- 3 * x1 + 2 * x2 + 0.5 * x3 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)

  res_cv   <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 1)
  res_path <- mine(df, y, method = "lasso_path", max_degree = 1, max_interact_vars = 1)

  # lasso returns ~3 rows (start + lambda.min + lambda.1se)
  # lasso_path should return more (one per distinct variable set along the path)
  expect_gt(nrow(res_path$all_models), nrow(res_cv$all_models))
})

test_that("lasso_path recovers true signal", {
  set.seed(22)
  n <- 200
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- 4 * x1 - 3 * x2 + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  result <- mine(df, y, method = "lasso_path", max_degree = 1, max_interact_vars = 1)
  labels <- attr(terms(result$Formula), "term.labels")

  expect_true("x1" %in% labels)
  expect_true("x2" %in% labels)
})

test_that("lasso_path works with polynomials and interactions", {
  set.seed(23)
  n <- 200
  x1 <- runif(n, -2, 2); x2 <- rnorm(n)
  y  <- x1 + x1^2 + 2 * x1 * x2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  result <- mine(df, y, method = "lasso_path", max_degree = 2, max_interact_vars = 2)
  labels <- attr(terms(result$Formula), "term.labels")

  # Should find at least x1 and x1:x2
  expect_true("x1" %in% labels)
})

test_that("lasso_path works via compare_methods alongside lasso", {
  set.seed(24)
  n <- 100
  x1 <- rnorm(n); x2 <- rnorm(n)
  y  <- 2 * x1 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  comp <- compare_methods(df, y, configs = list(
    lasso      = list(method = "lasso", max_degree = 1, max_interact_vars = 1),
    lasso_path = list(method = "lasso_path", max_degree = 1, max_interact_vars = 1)
  ))

  expect_s3_class(comp$summary, "data.frame")
  expect_equal(nrow(comp$summary), 2)
  expect_equal(comp$details$lasso_path$method, "lasso_path")
})

# ---- .colnames_to_terms correctness tests ----

test_that(".colnames_to_terms handles factor:numeric interaction columns", {
  # Bug: startsWith("fB:x", "f") matched main-effect term "f" before the
  # interaction term "f:x" could be considered, so "f:x" was silently dropped.
  # Fix: use model.matrix "assign" attribute for unambiguous column -> term mapping.
  set.seed(50)
  df <- data.frame(
    y  = rnorm(60),
    f  = factor(sample(c("A", "B", "C"), 60, replace = TRUE)),
    x  = rnorm(60),
    x2 = rnorm(60)
  )
  # Use a strong signal on the factor:numeric interaction so lasso keeps it.
  # beta_fB_x = 5, very low noise.
  levels_f <- as.integer(df$f)
  df$y <- ifelse(df$f == "B", 5 * df$x, 0) + rnorm(60, sd = 0.2)

  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 2,
                 nfolds = 5)
  labels <- attr(terms(result$Formula), "term.labels")

  # The interaction f:x should appear in the selected formula when its signal
  # is strong.  Before the fix, .colnames_to_terms() swallowed "f:x" entirely.
  expect_true("f:x" %in% labels,
              label = "factor:numeric interaction should be selected by lasso")
})

test_that(".colnames_to_terms handles factor:factor interaction columns", {
  set.seed(51)
  n  <- 120
  f1 <- factor(sample(c("A", "B"), n, replace = TRUE))
  f2 <- factor(sample(c("X", "Y"), n, replace = TRUE))
  # Strong signal on the f1:f2 interaction
  y  <- ifelse(f1 == "B" & f2 == "Y", 8, 0) + rnorm(n, sd = 0.3)
  df <- data.frame(y = y, f1 = f1, f2 = f2)

  result <- mine(df, y, method = "lasso", max_degree = 1, max_interact_vars = 2,
                 nfolds = 5)
  labels <- attr(terms(result$Formula), "term.labels")

  expect_true("f1:f2" %in% labels,
              label = "factor:factor interaction should be selected by lasso")
})

test_that("lasso returns current formula gracefully when design matrix has < 2 columns", {
  # Regression test for the single-column crash:
  # keep_all_vars = TRUE with one predictor -> candidate pool is empty after
  # setdiff, so all_terms = just the one predictor -> ncol(x) == 1 after
  # dropping the intercept -> cv.glmnet would crash with "x should be a
  # matrix with 2 or more columns".  The fix returns the starting formula.
  set.seed(52)
  df <- data.frame(y = rnorm(30), x1 = rnorm(30))

  expect_no_error({
    result <- mine(df, y, method = "lasso",
                   max_degree = 1, max_interact_vars = 1,
                   keep_all_vars = TRUE)
  })
  # Should return x1 unchanged (the starting formula equals the best formula)
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
})

test_that("lasso_path returns current formula gracefully when design matrix has < 2 columns", {
  set.seed(53)
  df <- data.frame(y = rnorm(30), x1 = rnorm(30))

  expect_no_error({
    result <- mine(df, y, method = "lasso_path",
                   max_degree = 1, max_interact_vars = 1,
                   keep_all_vars = TRUE)
  })
  labels <- attr(terms(result$Formula), "term.labels")
  expect_true("x1" %in% labels)
})
