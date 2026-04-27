test_that("to_xy extracts correct response and predictors from formula", {
  xy <- modelminer:::to_xy(mtcars, mpg ~ cyl + hp)
  expect_equal(xy$y, mtcars$mpg)
  expect_equal(ncol(xy$x), 2)
  expect_true(all(c("cyl", "hp") %in% colnames(xy$x)))
  expect_false("wt" %in% colnames(xy$x))
})

test_that(".build_formula handles intercept-only, single, and multi-term cases", {
  f0 <- modelminer:::.build_formula("mpg", character(0))
  expect_s3_class(f0, "formula")
  expect_identical(attr(stats::terms(f0), "term.labels"), character(0))
  expect_identical(all.vars(f0[[2]]), "mpg")

  f1 <- modelminer:::.build_formula("mpg", "hp")
  expect_identical(attr(stats::terms(f1), "term.labels"), "hp")

  fn <- modelminer:::.build_formula("mpg", c("hp", "wt"))
  expect_identical(attr(stats::terms(fn), "term.labels"), c("hp", "wt"))
})

test_that(".build_formula preserves polynomial and interaction terms verbatim", {
  f <- modelminer:::.build_formula("mpg", c("hp", "wt", "I(hp^2)", "hp:wt"))
  expect_identical(
    attr(stats::terms(f), "term.labels"),
    c("hp", "wt", "I(hp^2)", "hp:wt")
  )
  # Confirm it actually fits — guards against any quoting regression
  fit <- stats::lm(f, data = mtcars)
  expect_s3_class(fit, "lm")
})
