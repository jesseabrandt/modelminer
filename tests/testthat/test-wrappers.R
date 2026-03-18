# Tests for method-specific wrapper functions (mine_greedy, mine_lasso, etc.)

expected_names <- c("Formula", "all_models", "model", "best_metric", "method")

# -- mine_greedy -----------------------------------------------------------

test_that("mine_greedy returns expected structure and method tag", {
  result <- mine_greedy(mtcars, mpg, verbose = FALSE)
  expect_type(result, "list")
  expect_named(result, expected_names)
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
  expect_type(result, "list")
  expect_named(result, expected_names)
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
  expect_type(result, "list")
  expect_named(result, expected_names)
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
  expect_type(result, "list")
  expect_named(result, expected_names)
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
  expect_type(result, "list")
  expect_named(result, expected_names)
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
  expect_equal(deparse1(a$Formula), deparse1(b$Formula))
})

# -- mine_lasso_path -------------------------------------------------------

test_that("mine_lasso_path returns expected structure", {
  skip_if_not_installed("glmnet")
  result <- mine_lasso_path(mtcars, mpg, max_degree = 1,
                            max_interact_vars = 1, verbose = FALSE)
  expect_type(result, "list")
  expect_named(result, expected_names)
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
