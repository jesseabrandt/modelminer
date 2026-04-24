# S3 dispatch ----------------------------------------------------------------

test_that("mine.formula() returns an object of class 'mine'", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_s3_class(fit, "mine")
  expect_s3_class(fit$formula, "formula")
  expect_s3_class(fit$model, "lm")
  expect_s3_class(fit$trace, "data.frame")
})

test_that("mine.data.frame() (NSE response) returns the same class", {
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_s3_class(fit, "mine")
  expect_s3_class(fit$model, "lm")
})

test_that("pipe with formula in slot 2 routes to the formula method", {
  fit <- mtcars |> mine(mpg ~ wt + cyl, verbose = FALSE,
                        max_degree = 1, max_interact_vars = 1)
  expect_s3_class(fit, "mine")
  used <- all.vars(fit$formula)
  expect_true(all(used %in% c("mpg", "wt", "cyl")))
})

test_that("all three call forms agree on the selected formula", {
  f1 <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
             max_degree = 1, max_interact_vars = 1)$formula
  f2 <- mine(mtcars, mpg, verbose = FALSE,
             max_degree = 1, max_interact_vars = 1)$formula
  f3 <- (mtcars |> mine(mpg ~ ., verbose = FALSE,
                        max_degree = 1, max_interact_vars = 1))$formula
  expect_equal(deparse1(f1), deparse1(f2))
  expect_equal(deparse1(f1), deparse1(f3))
})

# Back-compat: legacy $Formula / $all_models fields are populated ------------

test_that("mine() object exposes both new and legacy field names", {
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  # new names
  expect_s3_class(fit$formula, "formula")
  expect_s3_class(fit$trace, "data.frame")
  # legacy names preserved for existing $-accessors
  expect_identical(fit$Formula,    fit$formula)
  expect_identical(fit$all_models, fit$trace)
})

# Constructor behaviour ------------------------------------------------------

test_that("mine() default model_func is lm", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_s3_class(fit$model, "lm")
})

test_that("mine() respects an explicit predictor list from the formula", {
  fit <- mine(mpg ~ wt + cyl, data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  used <- all.vars(fit$formula)
  expect_true(all(used %in% c("mpg", "wt", "cyl")))
})

test_that("mine() forwards method and search options", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              method = "forward_backward",
              max_degree = 1, max_interact_vars = 1)
  expect_equal(fit$method, "forward_backward")
})

# Input validation -----------------------------------------------------------

test_that("mine.formula() errors on a one-sided formula", {
  expect_error(mine(~ wt, data = mtcars),
               "two-sided")
})

test_that("mine.formula() errors when data is not a data frame", {
  expect_error(mine(mpg ~ wt, data = as.matrix(mtcars)),
               "'data' must be a data frame")
})

test_that("mine.formula() errors when response is missing from data", {
  expect_error(mine(zzz ~ wt, data = mtcars),
               "Response variable 'zzz' not found")
})

test_that("mine.formula() errors when a named predictor is missing", {
  expect_error(mine(mpg ~ zzz, data = mtcars),
               "Predictor")
})

# S3 methods -----------------------------------------------------------------

test_that("print.mine() prints the call, method, formula, metric", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  out <- capture.output(print(fit))
  expect_true(any(grepl("Method",  out)))
  expect_true(any(grepl("Formula", out)))
  expect_true(any(grepl("Metric",  out)))
})

test_that("summary.mine() returns a summary.mine object", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  s <- summary(fit)
  expect_s3_class(s, "summary.mine")
  expect_true(!is.null(s$model_summary))
  expect_equal(s$method, fit$method)
  expect_equal(s$formula, fit$formula)
})

test_that("print.summary.mine() runs without error", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_no_error(capture.output(print(summary(fit))))
})

test_that("coef.mine() delegates to the underlying model", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_equal(coef(fit), coef(fit$model))
})

test_that("predict.mine() works with and without newdata", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  p1 <- predict(fit)
  p2 <- predict(fit, newdata = mtcars[1:5, ])
  expect_length(p1, nrow(mtcars))
  expect_length(p2, 5L)
})

test_that("formula.mine() returns the selected formula", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_identical(formula(fit), fit$formula)
})

test_that("plot.mine() delegates to the underlying model", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(fit))
})

# extract_model() ------------------------------------------------------------

test_that("extract_model() returns the fitted model from a mine fit", {
  fit <- mine(mpg ~ ., data = mtcars, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  m <- extract_model(fit)
  expect_s3_class(m, "lm")
  expect_identical(m, fit$model)
})

test_that("extract_model() errors clearly on unsupported input", {
  expect_error(extract_model(1:5),
               "Cannot extract a model")
})
