# Helpers for extracting model-embedded metrics
#
# These helpers are specifically for metrics that are stored ON the model
# object itself -- OOB error, cross-validated loss, residual deviance, etc.
# They are NOT needed for externally-computed metrics like AIC or BIC:
# those already work as-is because lm, glm, gam, coxph, lmer, and friends
# all respond to stats::AIC(), which is mine()'s default metric.
#
# The S3 generic extract_metric() makes one opinionated default choice per
# model class. When a model type exposes multiple metrics (e.g. rpart's CP
# table has both resubstitution error and cross-validated error), the default
# picks the most generally useful one. To use a different metric, write an
# anonymous function instead:
#
#   metric = \(m) min(m$cptable[, "rel error"])   # training error instead of CV
#   metric = \(m) m$rsq[length(m$rsq)]            # OOB R^2 from randomForest
#
# Use list_metrics() interactively to see what slots a model exposes before
# deciding whether the default is appropriate.


# ---- extract_metric generic --------------------------------------------------

#' Extract a model-embedded metric for use with mine()
#'
#' Extracts a scalar metric from a fitted model object, suitable for passing
#' as \code{mine()}'s \code{metric} argument. Dispatches on the model's class.
#'
#' \strong{All methods return lower-is-better values}, so
#' \code{metric_comparison = min} (mine()'s default) is always correct.
#'
#' \strong{Default method:} calls \code{AIC()}, covering \code{lm},
#' \code{glm}, \code{gam}, \code{coxph}, \code{lmer}, and other standard
#' R model classes. For those you do not need \code{extract_metric} at all --
#' \code{metric = AIC} (the default) already handles them.
#'
#' \strong{Adding support for a new class:}
#' \preformatted{
#'   extract_metric.my_class <- function(model, ...) model$my_loss_slot
#' }
#' Use \code{list_metrics()} to inspect what slots are available.
#'
#' @param model A fitted model object.
#' @param ... Reserved for future method arguments.
#' @returns A single numeric value, lower-is-better.
#' @seealso \code{\link{list_metrics}}
#' @importFrom stats coef lm.fit nobs
#' @importFrom utils tail
#' @export
extract_metric <- function(model, ...) UseMethod("extract_metric")

#' @export
extract_metric.default <- function(model, ...) {
  tryCatch(
    AIC(model),
    error = function(e) {
      stop(
        "No extract_metric method for class '", paste(class(model), collapse = "/"), "'. ",
        "Define one with:\n",
        "  extract_metric.", class(model)[1], " <- function(model, ...) <your metric>\n",
        "Use list_metrics(model) to see available slots.",
        call. = FALSE
      )
    }
  )
}


# ---- Methods -----------------------------------------------------------------

# cv.glmnet (glmnet::cv.glmnet) -------------------------------------------
#
# Default: minimum mean CV error across the lambda path (at lambda.min).
# The metric type depends on what type.measure was passed at fit time;
# model$name reports it (e.g. "Mean-Squared Error", "AUC", "Deviance").
#
# Alternatives:
#   \(m) m$cvm[m$lambda == m$lambda.1se]   # CV error at the 1-SE lambda
#   \(m) min(m$cvup)                        # upper CV error band
#
# Note: keep_all_vars = TRUE is required when using this with mine() because
# cv.glmnet cannot accept a zero-column predictor matrix, which formula_wrap
# produces for the intercept-only starting formula.
#' @export
extract_metric.cv.glmnet <- function(model, ...) min(model$cvm)


# ranger (ranger::ranger) -------------------------------------------------
#
# Default: out-of-bag prediction error. The scale depends on the task:
#   regression:    OOB mean squared error
#   classification: OOB misclassification rate
#   probability:   OOB Brier score
# All are lower-is-better. OOB is computed during training at no extra cost.
#
# Alternatives:
#   \(m) 1 - m$r.squared   # OOB 1 - R^2 (regression only; negated so lower=better)
#
#' @export
extract_metric.ranger <- function(model, ...) {
  if (is.null(model$prediction.error)) {
    stop(
      "ranger model has no OOB prediction error. ",
      "This should not happen unless the forest object is malformed.",
      call. = FALSE
    )
  }
  model$prediction.error
}


# randomForest (randomForest::randomForest) --------------------------------
#
# Default: overall OOB error at the final tree count.
#   classification: OOB misclassification rate (last row of err.rate[,"OOB"])
#   regression:    OOB MSE (last element of mse)
#
# Alternatives (classification):
#   \(m) tail(m$err.rate[, "class_name"], 1)   # per-class OOB error rate
#
# Alternatives (regression):
#   \(m) 1 - tail(m$rsq, 1)   # OOB 1 - R^2 (negated so lower = better)
#
#' @export
extract_metric.randomForest <- function(model, ...) {
  if (!is.null(model$err.rate)) {
    tail(model$err.rate[, "OOB"], 1)
  } else if (!is.null(model$mse)) {
    tail(model$mse, 1)
  } else {
    stop("Could not extract OOB error from randomForest model.", call. = FALSE)
  }
}


# rpart (rpart::rpart) -----------------------------------------------------
#
# Default: minimum cross-validated relative error (xerror) from the CP table.
# xerror is the CV estimate; rel error is the resubstitution (training) error.
# Both are relative to the root node error, so they are dimensionless.
# xerror is preferred for model selection because it estimates generalisation.
#
# The full CP table has columns: CP, nsplit, rel error, xerror, xstd.
# xstd is the standard error of xerror -- useful for the "1-SE rule" (pick the
# simplest tree within 1 SE of the minimum xerror).
#
# Alternatives:
#   \(m) min(m$cptable[, "rel error"])              # training error (biased)
#   \(m) {
#     best <- which.min(m$cptable[, "xerror"])
#     m$cptable[best, "xerror"] + m$cptable[best, "xstd"]  # 1-SE threshold
#   }
#
#' @export
extract_metric.rpart <- function(model, ...) {
  min(model$cptable[, "xerror"])
}


# tree (tree::tree) --------------------------------------------------------
#
# Default: residual deviance stored on the fitted object.
# This is the training deviance -- not cross-validated -- so it will favour
# larger trees. For proper selection, fit with tree::cv.tree() and wrap
# that instead.
#
# Alternatives:
#   \(m) m$dev / m$frame$dev[1]   # relative deviance (normalised to root)
#
#' @export
extract_metric.tree <- function(model, ...) model$dev


# gbm (gbm::gbm) -----------------------------------------------------------
#
# Default: minimum cross-validated loss (requires cv.folds > 0 at fit time).
# The loss type depends on the distribution argument: gaussian -> MSE,
# bernoulli -> log-loss, adaboost -> exponential loss, etc.
#
# Falls back to training loss with a warning if no CV was run, but training
# loss always decreases with more trees and is unreliable for selection.
#
# Alternatives:
#   \(m) m$cv.error[gbm::gbm.perf(m, method="cv", plot.it=FALSE)]
#   # error at the optimal number of trees by CV
#
#' @export
extract_metric.GBMFit <- function(model, ...) {
  if (!is.null(model$cv.error) && length(model$cv.error) > 0) {
    min(model$cv.error)
  } else {
    warning(
      "gbm model has no cross-validated error (cv.folds was not set). ",
      "Using training loss, which favours more trees and is not reliable for selection. ",
      "Refit with cv.folds >= 3.",
      call. = FALSE
    )
    min(model$train.error)
  }
}


# ---- lm_loocv ---------------------------------------------------------------

#' Analytical leave-one-out cross-validation for linear models
#'
#' Computes the mean squared leave-one-out prediction error for a fitted
#' \code{lm} object using the hat-matrix shortcut (PRESS / n). No additional
#' model fits are required, making this as fast as \code{AIC}.
#'
#' Only valid for ordinary least-squares (\code{lm}) models. For other model
#' types, or when k-fold CV is preferred, use \code{\link{make_cv_metric}}.
#'
#' @param model A fitted \code{lm} object.
#' @returns A single numeric value (mean squared LOOCV error), lower-is-better.
#' @seealso \code{\link{make_cv_metric}}
#' @export
#'
#' @examples
#' result <- mine(mtcars, mpg, metric = lm_loocv)
#' result$Formula
lm_loocv <- function(model) {
  mean((residuals(model) / (1 - hatvalues(model)))^2)
}


# ---- make_cv_metric ---------------------------------------------------------

#' K-fold cross-validation metric function
#'
#' Returns a metric function that computes mean squared k-fold cross-validated
#' prediction error. The returned function can be passed directly to
#' \code{\link{mine}} as the \code{metric} argument.
#'
#' The data and formula are extracted from the fitted model via
#' \code{model.frame()} and \code{formula()}, so no separate data argument is
#' required. This works correctly when every variable referenced inside
#' \code{I()} expressions also appears as a direct term in the formula -- which
#' is the normal case when \code{modelminer} builds formulas incrementally.
#'
#' \strong{Performance note:} each call to the returned function refits the
#' model \code{k} times on training folds. For a typical \code{mine()} run
#' evaluating hundreds of candidate models, this is substantially slower than
#' \code{\link{lm_loocv}} or \code{AIC}. Prefer \code{lm_loocv} for
#' \code{lm}-based searches unless you specifically need k-fold CV.
#'
#' @param k Number of folds. Defaults to 10.
#' @param seed Integer seed for reproducible fold assignment. Defaults to 1.
#' @returns A function with signature \code{function(model)} returning a single
#'   numeric value (mean squared CV error), lower-is-better. Compatible with
#'   \code{mine()}'s default \code{metric_comparison = min}.
#' @seealso \code{\link{lm_loocv}}
#' @export
#'
#' @examples
#' cv10 <- make_cv_metric(k = 10)
#' result <- mine(mtcars, mpg, metric = cv10)
#' result$Formula
make_cv_metric <- function(k = 10, seed = 1L) {
  force(k)
  force(seed)

  function(model) {
    # Use model.matrix() rather than re-evaluating the formula on fold subsets.
    # Formula re-evaluation fails when a predictor appears only inside an
    # interaction (e.g. cyl:wt with no standalone wt term) because the
    # model frame subset won't contain the raw variable. Slicing the
    # pre-computed design matrix sidesteps that entirely.
    X <- model.matrix(model)
    y <- model.response(model.frame(model))
    n <- length(y)

    # Save and restore RNG state so callers' randomness is not affected.
    old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
          rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
    folds <- sample(rep_len(seq_len(k), n))

    fold_mse <- vapply(seq_len(k), function(i) {
      X_train <- X[folds != i, , drop = FALSE]
      X_test  <- X[folds == i, , drop = FALSE]
      y_train <- y[folds != i]
      y_test  <- y[folds == i]

      fit <- tryCatch(lm.fit(X_train, y_train), error = function(e) NULL)
      if (is.null(fit)) return(NA_real_)

      coefs <- fit$coefficients
      if (anyNA(coefs)) return(NA_real_)  # rank-deficient fold

      pred <- drop(X_test %*% coefs)
      mean((y_test - pred)^2)
    }, numeric(1))

    mean(fold_mse, na.rm = TRUE)
  }
}


# ---- make_cp_metric ---------------------------------------------------------

#' Mallow's Cp metric function
#'
#' Returns a metric function that computes Mallow's Cp statistic for a
#' candidate model, using the residual variance from a user-supplied full
#' model as the reference. The returned function can be passed directly to
#' \code{\link{mine}} as the \code{metric} argument.
#'
#' \strong{Formula:} \code{Cp = RSS_p / sigma2_full - n + 2p} where
#' \code{RSS_p} is the candidate model's residual sum of squares,
#' \code{sigma2_full} is the full model's residual variance
#' (\code{summary(full_model)$sigma^2}, i.e. RSS_full / (n - p_full)),
#' \code{n} is the number of observations, and \code{p} is the number of
#' estimated coefficients (including intercept) in the candidate model.
#' A well-fitting model has Cp approximately equal to p.
#'
#' \strong{Only valid for \code{lm} models.} Both the full model and every
#' candidate model evaluated by \code{mine()} must be ordinary least-squares
#' fits. The full model should include all predictors that could plausibly
#' matter -- typically \code{lm(y ~ ., data)}.
#'
#' @param full_model A fitted \code{lm} object used as the reference
#'   (its \code{sigma^2} estimates the error variance).
#' @returns A function with signature \code{function(model)} returning a single
#'   numeric Cp value (lower is better). Compatible with \code{mine()}'s
#'   default \code{metric_comparison = min}.
#' @seealso \code{\link{lm_loocv}}, \code{\link{make_cv_metric}}
#' @export
#'
#' @examples
#' cp <- make_cp_metric(lm(mpg ~ ., data = mtcars))
#' result <- mine(mtcars, mpg, metric = cp, max_degree = 1)
#' result$Formula
#'
# TODO(jesse): triple-check the Cp formula against a textbook reference.
# Verified numerically: Cp(full model) == p_full (exact) on mtcars, and
# the formula matches the leaps package definition:
#   Cp = RSS_p / sigma2_full - n + 2*p
# where sigma2_full = summary(full)$sigma^2 = RSS_full / (n - p_full).
make_cp_metric <- function(full_model) {
  mse_full <- summary(full_model)$sigma^2
  if (mse_full == 0) {
    stop("Full model has zero residual variance (perfect fit); ",
         "Cp is undefined.", call. = FALSE)
  }

  function(model) {
    rss <- sum(residuals(model)^2)
    n   <- nobs(model)
    p   <- sum(!is.na(coef(model)))  # exclude aliased (NA) coefficients
    rss / mse_full - n + 2 * p
  }
}


# ---- list_metrics -----------------------------------------------------------

#' Inspect available metrics on a fitted model object
#'
#' Prints the numeric slots available on a model, what \code{extract_metric()}
#' returns by default, and column names of any matrix slots. Intended as an
#' interactive discovery tool before deciding whether the default metric is
#' appropriate or whether to write a custom anonymous function instead.
#'
#' @param model A fitted model object.
#' @returns \code{model}, invisibly.
#' @seealso \code{\link{extract_metric}}
#' @export
#'
#' @examples
#' \dontrun{
#' m <- rpart::rpart(mpg ~ ., data = mtcars)
#' list_metrics(m)
#' }
list_metrics <- function(model) {
  cat("Class:", paste(class(model), collapse = " / "), "\n")

  default <- tryCatch(extract_metric(model), error = function(e) NULL)
  if (!is.null(default)) {
    cat("extract_metric() default:", round(default, 6), "\n")
  } else {
    cat("extract_metric() default: no method defined for this class\n")
  }

  cat("\nNumeric slots (use as \\(m) m$<name> or \\(m) min(m$<name>)):\n")
  scalar_printed <- FALSE
  for (nm in names(model)) {
    val <- model[[nm]]
    if (is.numeric(val) && !is.matrix(val)) {
      if (length(val) == 1) {
        cat("  $", nm, ": ", round(val, 6), "\n", sep = "")
      } else {
        cat("  $", nm, ": [length ", length(val), "]",
            "  range [", round(min(val), 4), ", ", round(max(val), 4), "]\n", sep = "")
      }
      scalar_printed <- TRUE
    }
  }
  if (!scalar_printed) cat("  (none at top level)\n")

  mat_slots <- Filter(is.matrix, model)
  if (length(mat_slots) > 0) {
    cat("\nMatrix slots (use \\(m) min(m$<name>[, \"<col>\"])):\n")
    for (nm in names(mat_slots)) {
      cat("  $", nm, ": columns [", paste(colnames(model[[nm]]), collapse = ", "), "]\n", sep = "")
    }
  }

  invisible(model)
}
