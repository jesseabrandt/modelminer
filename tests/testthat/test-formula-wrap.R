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
