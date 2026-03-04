test_that("backward returns expected structure", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "backward")
  expect_type(result, "list")
  expect_named(result, c("Formula", "all_models", "model", "best_metric", "method"))
  expect_s3_class(result$Formula, "formula")
  expect_s3_class(result$all_models, "data.frame")
  expect_equal(result$method, "backward")
})

test_that("backward improves on the full model (AIC on mtcars)", {
  full_aic <- AIC(lm(mpg ~ ., data = mtcars))
  result   <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                   method = "backward")
  expect_lte(result$best_metric, full_aic)
})

test_that("backward removes at least one predictor", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "backward")
  all_preds   <- setdiff(names(mtcars), "mpg")
  kept_labels <- attr(terms(result$Formula), "term.labels")
  expect_lt(length(kept_labels), length(all_preds))
})

test_that("backward works with keep_all_vars = TRUE", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "backward", keep_all_vars = TRUE)
  expect_s3_class(result$Formula, "formula")
  expect_equal(result$method, "backward")
})

test_that("backward works with keep_all_vars = FALSE (auto-override)", {
  result <- mine(mtcars, mpg, max_degree = 1, max_interact_vars = 1,
                 method = "backward", keep_all_vars = FALSE)
  expect_s3_class(result$Formula, "formula")
  # Should still have started from all predictors and pruned
  expect_gt(nrow(result$all_models), 1)
})

test_that("backward works via compare_methods", {
  configs <- list(
    greedy   = list(method = "greedy"),
    backward = list(method = "backward")
  )
  comp <- compare_methods(mtcars, mpg, configs,
                          max_degree = 1, max_interact_vars = 1)
  expect_true("backward" %in% comp$summary$Config)
  expect_true("greedy"   %in% comp$summary$Config)
})
