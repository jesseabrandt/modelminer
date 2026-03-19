# .metric_improved ----------------------------------------------------------
test_that(".metric_improved detects numeric improvement", {
  expect_true(.metric_improved(5, 10, min))
  expect_false(.metric_improved(10, 5, min))
})

test_that(".metric_improved handles near-equal floats as no improvement", {
  expect_false(.metric_improved(5 + 1e-15, 5, min))
})

test_that(".metric_improved works with max", {
  expect_true(.metric_improved(10, 5, max))
  expect_false(.metric_improved(5, 10, max))
})

# .find_best_index ----------------------------------------------------------
test_that(".find_best_index returns correct index", {
  expect_equal(.find_best_index(list(10, 5, 8), min), 2L)
  expect_equal(.find_best_index(list(10, 5, 8), max), 1L)
})

test_that(".find_best_index handles near-equal ties", {
  expect_equal(.find_best_index(list(5 + 1e-15, 5, 8), min), 1L)
})

# .try_fit_metric -----------------------------------------------------------
test_that(".try_fit_metric returns metric on success", {
  f <- mpg ~ wt
  res <- .try_fit_metric(f, lm, AIC, mtcars, "wt", "", verbose = FALSE)
  expect_true(is.list(res))
  expect_equal(res$metric, AIC(lm(f, mtcars)))
})

test_that(".try_fit_metric returns NULL and warns on model failure", {
  bad_func <- function(formula, data) stop("nope")
  expect_warning(
    res <- .try_fit_metric(mpg ~ wt, bad_func, AIC, mtcars, "wt", "", verbose = FALSE),
    "model fitting failed"
  )
  expect_null(res)
})

test_that(".try_fit_metric returns NULL and warns on metric failure", {
  bad_metric <- function(model) stop("nope")
  expect_warning(
    res <- .try_fit_metric(mpg ~ wt, lm, bad_metric, mtcars, "wt", "", verbose = FALSE),
    "metric computation failed"
  )
  expect_null(res)
})

# .results_collector --------------------------------------------------------
test_that(".results_collector accumulates and finalizes", {
  rc <- .results_collector(data.frame(Formula = "y ~ 1", Metric = I(list(100))))
  rc$collect("y ~ x", 90)
  rc$collect("y ~ x + z", 80)
  out <- rc$finalize()
  expect_equal(nrow(out), 3)
  expect_equal(out$Formula[3], "y ~ x + z")
})
