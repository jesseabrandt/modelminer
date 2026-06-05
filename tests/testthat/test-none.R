# method = "none": candidate-generation escape hatch -------------------------
#
# `none` builds the engineered candidate pool (first-order + polynomials +
# interactions) and fits the full generated formula, but runs NO search. The
# returned object is a normal "mine" object minus the search trace.

test_that("method = 'none' returns a 'mine' object tagged 'none'", {
  fit <- mine(mpg ~ ., data = mtcars, method = "none", verbose = FALSE)
  expect_s3_class(fit, "mine")
  expect_identical(fit$method, "none")
})

test_that("none's formula carries polynomial and interaction terms", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  labels <- attr(stats::terms(fit$formula), "term.labels")
  expect_true(any(grepl("I\\(.*\\^2\\)", labels)))   # a polynomial term
  expect_true(any(grepl(":", labels)))               # an interaction term
})

test_that("$candidate_terms is the pool as a character vector", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  expect_type(fit$candidate_terms, "character")
  expect_true(length(fit$candidate_terms) > 0)
  # The vector and the formula describe the same pool.
  formula_labels <- attr(stats::terms(fit$formula), "term.labels")
  expect_setequal(fit$candidate_terms, formula_labels)
})

test_that("none fits the full model so accessors work", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  expect_s3_class(fit$model, "lm")
  expect_type(coef(fit), "double")
  expect_true(is.numeric(fit$best_metric) && length(fit$best_metric) == 1L)
  preds <- predict(fit, newdata = mtcars)
  expect_length(preds, nrow(mtcars))
})

test_that("none carries no search trace", {
  fit <- mine(mpg ~ ., data = mtcars, method = "none", verbose = FALSE)
  expect_null(fit$trace)
  expect_null(fit$all_models)
})

test_that("max_degree / max_interact_vars control the generated pool", {
  # Degree 1, no interactions: pool is exactly the first-order predictors.
  flat <- mine(mpg ~ wt + hp + cyl, data = mtcars, method = "none",
               verbose = FALSE, max_degree = 1, max_interact_vars = 1)
  expect_setequal(flat$candidate_terms, c("wt", "hp", "cyl"))

  # Higher settings strictly enlarge the pool.
  rich <- mine(mpg ~ wt + hp + cyl, data = mtcars, method = "none",
               verbose = FALSE, max_degree = 3, max_interact_vars = 3)
  expect_true(length(rich$candidate_terms) > length(flat$candidate_terms))
})

test_that("formula scoping limits which predictors are expanded", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  expect_setequal(all.vars(fit$formula), c("mpg", "wt", "hp"))
})

test_that("data-first (NSE) interface supports method = 'none'", {
  fit <- mine(mtcars, mpg, method = "none", verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_s3_class(fit, "mine")
  expect_identical(fit$method, "none")
})

test_that("formula(fit) round-trips into a model fit", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  refit <- lm(formula(fit), data = mtcars)
  expect_s3_class(refit, "lm")
})

test_that("print() on a none fit is graceful (no trace/metric noise)", {
  fit <- mine(mpg ~ wt + hp, data = mtcars, method = "none", verbose = FALSE,
              max_degree = 2, max_interact_vars = 2)
  expect_output(print(fit), "none")
  expect_no_error(print(fit))
})
