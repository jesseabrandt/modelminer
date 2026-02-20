#' Wrap a model function to accept a formula interface
#'
#' Adapts functions that take a predictor matrix \code{x} and response vector
#' \code{y} (e.g. \code{glmnet}) into functions that accept a \code{formula}
#' and \code{data} argument, making them compatible with \code{mine()}.
#'
#' @param model_func A model function that accepts a predictor matrix and
#'   response vector as named arguments.
#' @param x_name Name of the predictor matrix argument in \code{model_func}.
#'   Defaults to \code{"x"}.
#' @param y_name Name of the response vector argument in \code{model_func}.
#'   Defaults to \code{"y"}.
#'
#' @returns A new function with the signature \code{function(data, formula, ...)}
#'   that extracts \code{x} and \code{y} from \code{data} using the formula and
#'   calls the original \code{model_func}.
#'
#' @export
#' @examples
#' \dontrun{
#' formula_glmnet <- formula_wrap(glmnet::glmnet)
#' result <- mine(mtcars, mpg, model_func = formula_glmnet)
#' }
#'
formula_wrap <- function(model_func, x_name = "x", y_name = "y") {

  # x_name / y_name default to "x" and "y" because that is the glmnet
  # convention, which is the primary motivating case. Other matrix-based
  # functions may differ. If a function uses different argument names, pass
  # them explicitly rather than relying on positional matching, since
  # do.call() uses named arguments.
  #
  # TODO: It may be worth inspecting formals(model_func) to detect the
  # correct argument names automatically, though this is unreliable for
  # functions that use ... or have non-standard signatures.

  new_func <- function(data, formula, ...) {
    xy   <- to_xy(data, formula)
    args <- list(...)
    args[[x_name]] <- xy$x
    args[[y_name]] <- xy$y
    do.call(model_func, args)
  }

  return(new_func)
}
