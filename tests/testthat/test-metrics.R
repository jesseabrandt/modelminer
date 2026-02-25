# Helpers used across tests ---------------------------------------------------

# Mock objects let us test S3 dispatch and slot logic without requiring the
# modeling packages to be installed. The class attribute is all that S3 needs.

mock_cv_glmnet <- structure(
  list(cvm    = c(1.5, 1.2, 1.1, 1.3, 1.8),
       lambda = c(0.1, 0.05, 0.01, 0.005, 0.001),
       lambda.min = 0.01,
       name   = "Mean-Squared Error"),
  class = "cv.glmnet"
)

mock_ranger <- structure(
  list(prediction.error = 0.234, treetype = "Regression"),
  class = "ranger"
)

mock_ranger_no_oob <- structure(
  list(prediction.error = NULL),
  class = "ranger"
)

mock_rf_class <- structure(
  list(err.rate = matrix(
    c(0.50, 0.30, 0.20, 0.18, 0.15,
      0.60, 0.40, 0.30, 0.25, 0.20,
      0.40, 0.20, 0.10, 0.10, 0.10),
    ncol = 3,
    dimnames = list(NULL, c("OOB", "setosa", "virginica"))
  )),
  class = "randomForest"
)

mock_rf_reg <- structure(
  list(mse = c(10.5, 8.2, 7.1, 6.8, 6.5),
       rsq = c(0.1, 0.3, 0.4, 0.45, 0.48)),
  class = "randomForest"
)

mock_rf_empty <- structure(list(), class = "randomForest")

mock_tree <- structure(list(dev = 45.2), class = "tree")

mock_gbm_cv <- structure(
  list(cv.error    = c(0.80, 0.60, 0.50, 0.45, 0.50),
       train.error = c(0.70, 0.50, 0.40, 0.30, 0.20)),
  class = "GBMFit"
)

mock_gbm_no_cv <- structure(
  list(cv.error    = NULL,
       train.error = c(0.70, 0.50, 0.40, 0.30, 0.20)),
  class = "GBMFit"
)


# extract_metric default (AIC fallback) ---------------------------------------

test_that("default method returns AIC for lm", {
  m <- lm(mpg ~ wt, data = mtcars)
  expect_equal(extract_metric(m), AIC(m))
})

test_that("default method returns AIC for glm", {
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  expect_equal(extract_metric(m), AIC(m))
})

test_that("default method errors with helpful message for unknown class", {
  bad <- structure(list(), class = "unknown_model_xyz")
  expect_error(
    extract_metric(bad),
    regexp = "unknown_model_xyz"
  )
  expect_error(
    extract_metric(bad),
    regexp = "extract_metric\\.unknown_model_xyz"
  )
})


# extract_metric.cv.glmnet ----------------------------------------------------

test_that("cv.glmnet returns min cvm", {
  expect_equal(extract_metric(mock_cv_glmnet), min(mock_cv_glmnet$cvm))
})

test_that("cv.glmnet result is a single numeric", {
  result <- extract_metric(mock_cv_glmnet)
  expect_length(result, 1)
  expect_type(result, "double")
})


# extract_metric.ranger -------------------------------------------------------

test_that("ranger returns prediction.error", {
  expect_equal(extract_metric(mock_ranger), 0.234)
})

test_that("ranger errors when prediction.error is NULL", {
  expect_error(extract_metric(mock_ranger_no_oob), regexp = "prediction.error")
})


# extract_metric.randomForest -------------------------------------------------

test_that("randomForest classification returns last OOB error rate", {
  expect_equal(extract_metric(mock_rf_class), 0.15)
})

test_that("randomForest regression returns last OOB MSE", {
  expect_equal(extract_metric(mock_rf_reg), 6.5)
})

test_that("randomForest prefers err.rate over mse when both present", {
  # err.rate signals classification; should use OOB column not mse
  mock_both <- structure(
    list(err.rate = mock_rf_class$err.rate, mse = mock_rf_reg$mse),
    class = "randomForest"
  )
  expect_equal(extract_metric(mock_both), 0.15)
})

test_that("randomForest errors when neither err.rate nor mse present", {
  expect_error(extract_metric(mock_rf_empty), regexp = "OOB")
})


# extract_metric.rpart --------------------------------------------------------

test_that("rpart returns min xerror from cptable", {
  m <- rpart::rpart(mpg ~ ., data = mtcars)
  expect_equal(extract_metric(m), min(m$cptable[, "xerror"]))
})

test_that("rpart result is a single numeric", {
  m <- rpart::rpart(mpg ~ ., data = mtcars)
  result <- extract_metric(m)
  expect_length(result, 1)
  expect_type(result, "double")
})

test_that("rpart works for classification trees", {
  m <- rpart::rpart(Species ~ ., data = iris)
  expect_equal(extract_metric(m), min(m$cptable[, "xerror"]))
})


# extract_metric.tree ---------------------------------------------------------

test_that("tree returns dev slot", {
  expect_equal(extract_metric(mock_tree), 45.2)
})


# extract_metric.GBMFit -------------------------------------------------------

test_that("gbm returns min cv.error when available", {
  expect_equal(extract_metric(mock_gbm_cv), 0.45)
})

test_that("gbm falls back to train.error and warns when cv.error is NULL", {
  expect_warning(
    result <- extract_metric(mock_gbm_no_cv),
    regexp = "cv.folds"
  )
  expect_equal(result, min(mock_gbm_no_cv$train.error))
})


# lower-is-better convention --------------------------------------------------

test_that("all methods return numeric scalars", {
  results <- list(
    extract_metric(mock_cv_glmnet),
    extract_metric(mock_ranger),
    extract_metric(mock_rf_class),
    extract_metric(mock_rf_reg),
    extract_metric(mock_tree),
    extract_metric(mock_gbm_cv)
  )
  for (r in results) {
    expect_length(r, 1)
    expect_true(is.numeric(r))
  }
})


# list_metrics ----------------------------------------------------------------

test_that("list_metrics returns model invisibly", {
  m <- lm(mpg ~ wt, data = mtcars)
  result <- expect_invisible(list_metrics(m))
  expect_identical(result, m)
})

test_that("list_metrics works without error on lm", {
  m <- lm(mpg ~ wt, data = mtcars)
  expect_no_error(list_metrics(m))
})

test_that("list_metrics works without error on rpart", {
  m <- rpart::rpart(mpg ~ ., data = mtcars)
  expect_no_error(list_metrics(m))
})

test_that("list_metrics works on unknown class without erroring", {
  bad <- structure(list(x = 1.5), class = "unknown_xyz")
  expect_no_error(list_metrics(bad))
})

test_that("list_metrics output mentions extract_metric default for known class", {
  m <- lm(mpg ~ wt, data = mtcars)
  output <- capture.output(list_metrics(m))
  expect_true(any(grepl("extract_metric", output)))
  expect_true(any(grepl(round(AIC(m), 4), output, fixed = TRUE)))
})


# integration with mine() -----------------------------------------------------

test_that("extract_metric works as drop-in metric= argument for mine()", {
  result <- mine(mtcars, mpg,
                 metric            = extract_metric,
                 max_degree        = 1,
                 max_interact_vars = 1)
  expect_s3_class(result$Formula, "formula")
  # Should select same model as AIC since default falls back to AIC
  result_aic <- mine(mtcars, mpg,
                     metric            = AIC,
                     max_degree        = 1,
                     max_interact_vars = 1)
  expect_equal(deparse1(result$Formula), deparse1(result_aic$Formula))
})
