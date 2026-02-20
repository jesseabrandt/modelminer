test_that("to_xy extracts correct response and predictors from formula", {
  xy <- modelminer:::to_xy(mtcars, mpg ~ cyl + hp)
  expect_equal(xy$y, mtcars$mpg)
  expect_equal(ncol(xy$x), 2)
  expect_true(all(c("cyl", "hp") %in% colnames(xy$x)))
  expect_false("wt" %in% colnames(xy$x))
})
