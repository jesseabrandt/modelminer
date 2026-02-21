#' Extract a comparable metric from a fitted model
#'
#' A generic function that extracts a scalar metric suitable for use as
#' \code{mine()}'s \code{metric} argument. It dispatches on the class of the
#' fitted model, so the same call works regardless of which package produced
#' the model.
#'
#' \strong{Convention:} all methods return a value where \emph{lower is better},
#' so \code{metric_comparison = min} (the default in \code{mine()}) is always
#' correct. For metrics that are naturally higher-is-better (R², accuracy),
#' methods return their negation.
#'
#' \strong{For standard R model objects} (\code{lm}, \code{glm}, \code{gam},
#' \code{coxph}, \code{lmer}, etc.) the default method calls \code{AIC()},
#' so you do not need \code{extract_metric} for those — the default
#' \code{metric = AIC} in \code{mine()} already handles them.
#'
#' \strong{Extending to new classes:} define a method and it is immediately
#' available as \code{metric = extract_metric} in any \code{mine()} call:
#' \preformatted{
#'   extract_metric.my_class <- function(model, ...) model$my_loss
#' }
#'
#' @param model A fitted model object.
#' @param ... Reserved for future method arguments.
#'
#' @returns A single numeric value, lower-is-better.
#' @export
#'
#' @examples
#' \dontrun{
#' # cv.glmnet
#' formula_cv_glmnet <- formula_wrap(\(x, y, ...) glmnet::cv.glmnet(x, y, ...))
#' mine(mtcars, mpg,
#'      model_func        = formula_cv_glmnet,
#'      metric            = extract_metric,
#'      keep_all_vars     = TRUE,
#'      max_degree        = 1,
#'      max_interact_vars = 1)
#'
#' # ranger
#' mine(mtcars, mpg,
#'      model_func = \(formula, data) ranger::ranger(formula, data),
#'      metric     = extract_metric)
#' }
extract_metric <- function(model, ...) UseMethod("extract_metric")

# Default: fall back to AIC for any model class that supports it.
# This covers lm, glm, gam (mgcv), coxph (survival), lmer/glmer (lme4),
# and anything else in the standard R modelling ecosystem.
#' @export
extract_metric.default <- function(model, ...) {
  tryCatch(
    AIC(model),
    error = function(e) {
      stop(
        "No extract_metric method for class '", paste(class(model), collapse = "/"), "'. ",
        "Define one with:\n",
        "  extract_metric.", class(model)[1], " <- function(model, ...) <your metric>",
        call. = FALSE
      )
    }
  )
}

# ---- glmnet ------------------------------------------------------------------

# cv.glmnet (glmnet::cv.glmnet)
# Returns the minimum mean cross-validated error across the lambda path.
# The optimal lambda (model$lambda.min) is what cv.glmnet would select;
# taking min(cvm) is equivalent and gives a scalar for comparison.
#
# Use with: formula_wrap(\(x, y, ...) glmnet::cv.glmnet(x, y, ...))
# Note: keep_all_vars = TRUE is required — cv.glmnet cannot accept a
# zero-column predictor matrix, which formula_wrap produces for ~ 1.
#' @export
extract_metric.cv.glmnet <- function(model, ...) min(model$cvm)

# ---- ranger ------------------------------------------------------------------

# ranger (ranger::ranger)
# Returns out-of-bag prediction error. The meaning depends on the task:
#   regression:    OOB MSE
#   classification: OOB misclassification rate
#   probability:   OOB Brier score
# All are lower-is-better.
#
# OOB error is free — ranger computes it during training with no extra cost.
# Set num.threads if parallel speed matters; ranger defaults to all cores.
#' @export
extract_metric.ranger <- function(model, ...) {
  if (is.null(model$prediction.error)) {
    stop(
      "ranger model has no OOB prediction error. ",
      "Ensure keep.inbag = TRUE is not set to FALSE and the forest is not empty.",
      call. = FALSE
    )
  }
  model$prediction.error
}

# ---- randomForest ------------------------------------------------------------

# randomForest (randomForest::randomForest)
# Returns OOB error: misclassification rate for classification forests,
# OOB MSE for regression forests.
# err.rate is a matrix (ntree × nclass+1); the "OOB" column tracks the
# aggregate OOB error across trees. We take the final row (all trees grown).
#' @export
extract_metric.randomForest <- function(model, ...) {
  if (!is.null(model$err.rate)) {
    tail(model$err.rate[, "OOB"], 1)    # classification
  } else if (!is.null(model$mse)) {
    tail(model$mse, 1)                  # regression OOB MSE
  } else {
    stop("Could not extract OOB error from randomForest model.", call. = FALSE)
  }
}

# ---- rpart -------------------------------------------------------------------

# rpart (rpart::rpart)
# Returns the minimum cross-validated relative error from the CP table.
# xerror is relative to the root node error, so it is dimensionless and
# comparable across the search path (all models are fit on the same data).
# For classification this is relative misclassification; for regression,
# relative MSE.
#
# TODO: consider returning xerror * root_node_error for an absolute scale,
# which would make the metric comparable across datasets, not just within
# a single mine() run.
#' @export
extract_metric.rpart <- function(model, ...) {
  min(model$cptable[, "xerror"])
}

# ---- tree --------------------------------------------------------------------

# tree (tree::tree)
# Returns the residual deviance. This is not penalised for complexity, so it
# will tend to favour larger trees. Consider pruning with tree::cv.tree()
# and wrapping that instead if overfitting is a concern.
#' @export
extract_metric.tree <- function(model, ...) model$dev

# ---- gbm ---------------------------------------------------------------------

# gbm (gbm::gbm) — gradient boosted machines
# Returns the minimum cross-validated loss if cv.folds > 0 was set when
# fitting, otherwise falls back to training loss (less reliable for selection).
# The metric meaning depends on distribution: "bernoulli" gives log-loss,
# "gaussian" gives MSE, etc. All are lower-is-better.
#' @export
extract_metric.GBMFit <- function(model, ...) {
  if (!is.null(model$cv.error) && length(model$cv.error) > 0) {
    min(model$cv.error)
  } else {
    # No CV was run — use training loss as a fallback, but warn because this
    # will always favour more trees and is not a reliable selection criterion.
    warning(
      "gbm model has no cross-validated error (cv.folds was not set). ",
      "Using training loss, which may overfit. Set cv.folds >= 3 when fitting.",
      call. = FALSE
    )
    min(model$train.error)
  }
}
