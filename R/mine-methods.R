#' @export
print.mine <- function(x, ...) {
  cat("modelminer fit\n",
      "Call:    ", deparse1(x$call),  "\n",
      "Method:  ", x$method,           "\n",
      "Formula: ", deparse1(x$formula), "\n",
      "Metric:  ", format(x$best_metric), "\n",
      "Models evaluated: ", nrow(x$trace), "\n",
      sep = "")
  invisible(x)
}

#' @export
summary.mine <- function(object, ...) {
  model_summary <- tryCatch(summary(object$model, ...),
                            error = function(e) {
                              warning("summary() failed on underlying model: ",
                                      conditionMessage(e), call. = FALSE)
                              NULL
                            })
  structure(
    list(
      call          = object$call,
      formula       = object$formula,
      method        = object$method,
      best_metric   = object$best_metric,
      n_models      = nrow(object$trace),
      model_summary = model_summary,
      trace         = object$trace
    ),
    class = "summary.mine"
  )
}

#' @export
print.summary.mine <- function(x, ...) {
  cat("modelminer fit summary\n",
      "Call:    ", deparse1(x$call),    "\n",
      "Method:  ", x$method,             "\n",
      "Formula: ", deparse1(x$formula),  "\n",
      "Metric:  ", format(x$best_metric), "\n",
      "Models evaluated: ", x$n_models,  "\n\n",
      "-- Final model summary -----------------------------------\n",
      sep = "")
  if (!is.null(x$model_summary)) print(x$model_summary, ...)
  invisible(x)
}

#' @export
coef.mine <- function(object, ...) stats::coef(object$model, ...)

#' @export
predict.mine <- function(object, newdata, ...) {
  if (missing(newdata))
    stats::predict(object$model, ...)
  else
    stats::predict(object$model, newdata = newdata, ...)
}

#' @export
formula.mine <- function(x, ...) x$formula

#' Plot method for mine fits (stub)
#'
#' Currently delegates to the underlying model's \code{plot} method. Future
#' versions will add modelminer-specific plots (e.g. metric trace across the
#' search, coefficient paths). The signature is stable; the output may change.
#'
#' @param x A \code{"mine"} object.
#' @param ... Passed through to the underlying model's \code{plot} method.
#' @export
plot.mine <- function(x, ...) {
  if (is.null(x$model))
    stop("No fitted model to plot.", call. = FALSE)
  plot(x$model, ...)
}

#' Extract the fitted model from a search result
#'
#' Returns the underlying fitted model object (e.g. an \code{lm}, \code{glm},
#' \code{ranger}, ...) from a \code{\link{mine}} fit. Use this when you want
#' to hand the model to a downstream function that expects a "real" model
#' object, without depending on the internal list layout.
#'
#' @param x A \code{"mine"} object (or any list carrying a \code{$model}
#'   element, for forward-compatibility with custom search results).
#' @param ... Unused; reserved for future methods.
#'
#' @returns The fitted model object.
#'
#' @export
#' @examples
#' fit <- mine(mpg ~ ., data = mtcars, max_degree = 1, max_interact_vars = 1)
#' m <- extract_model(fit)
#' class(m)
extract_model <- function(x, ...) UseMethod("extract_model")

#' @export
extract_model.mine <- function(x, ...) x$model

#' @export
extract_model.default <- function(x, ...) {
  if (is.list(x) && !is.null(x$model)) return(x$model)
  stop("Cannot extract a model from object of class '",
       paste(class(x), collapse = "/"), "'.", call. = FALSE)
}
