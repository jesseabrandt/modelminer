test_that("formula_wrap returns a callable function", {
  wrapped <- formula_wrap(lm)
  expect_type(wrapped, "closure")
})

test_that("formula_wrap result can be called and produces a model", {
  xy_lm <- function(x, y) lm(y ~ x)
  wrapped <- formula_wrap(xy_lm)
  result <- wrapped(mpg ~ cyl, mtcars[, c("mpg", "cyl")])
  expect_s3_class(result, "lm")
})

test_that("formula_wrap errors clearly when formula and data are swapped", {
  wrapped <- formula_wrap(lm)
  expect_error(
    wrapped(mtcars, mpg ~ cyl),          # data where formula belongs
    "must be a formula object"
  )
})

test_that("formula_wrap honours custom x_name / y_name argument names", {
  # A matrix-based function whose predictor/response args aren't x/y.
  capture <- function(pred, resp) list(x = pred, y = resp)
  wrapped <- formula_wrap(capture, x_name = "pred", y_name = "resp")
  out <- wrapped(mpg ~ wt + hp, mtcars)
  expect_equal(ncol(out$x), 2L)                 # wt + hp, intercept dropped
  expect_equal(colnames(out$x), c("wt", "hp"))
  expect_equal(out$y, mtcars$mpg)
})

test_that("formula_wrap forwards ... extras to the wrapped function", {
  capture <- function(x, y, scaled = FALSE) scaled
  wrapped <- formula_wrap(capture)
  expect_true(wrapped(mpg ~ wt, mtcars, scaled = TRUE))
  expect_false(wrapped(mpg ~ wt, mtcars))       # default preserved
})
