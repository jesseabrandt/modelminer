# Extracted from test-metrics.R:218

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
m <- lm(mpg ~ wt, data = mtcars)
output <- capture.output(list_metrics(m))
expect_true(any(grepl("extract_metric", output)))
expect_true(any(grepl(round(AIC(m), 2), output, fixed = TRUE)))
