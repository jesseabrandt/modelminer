# Helpers for extracting model-embedded metrics
#
# These helpers are specifically for metrics that are stored ON the model
# object itself — OOB error, cross-validated loss, residual deviance, etc.
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
#   metric = \(m) m$rsq[length(m$rsq)]            # OOB R² from randomForest
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
#' R model classes. For those you do not need \code{extract_metric} at all —
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
#   \(m) 1 - m$r.squared   # OOB 1 - R² (regression only; negated so lower=better)
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
#   \(m) 1 - tail(m$rsq, 1)   # OOB 1 - R² (negated so lower = better)
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
# xstd is the standard error of xerror — useful for the "1-SE rule" (pick the
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
# This is the training deviance — not cross-validated — so it will favour
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
# The loss type depends on the distribution argument: gaussian → MSE,
# bernoulli → log-loss, adaboost → exponential loss, etc.
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
