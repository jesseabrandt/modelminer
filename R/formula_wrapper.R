

#' Wrap a model function to accept a formula interface
#'
#' @param model_func is the model function to wrap. It should take a predictor matrix and response vector as arguments.
#' @param x_name is the name of the predictor matrix in the model function.
#' @param y_name is the name of the response vector in the model function.
#'
#' @returns a new function that accepts a data frame and a formula, and calls the original model function with the appropriate arguments.
#'
#' @export
#' @examples
#' formula_glmnet <- formula_wrap(glmnet)
#' formula_glmnet(mtcars, mpg ~ cyl + hp)
#'
formula_wrap <- function(model_func, x_name = "x", y_name = "y"){

  # there is probably a better way to determine what the varnames are
  # I do not know how consistent they are
  # for now this should at least work with glmnet

  new_func <- function(data, formula, ...){
    xy <- to_xy(data, formula)
    args <- list(...)
    args[[x_name]] <- xy$x
    args[[y_name]] <- xy$y
    do.call(model_func, args)
  }

  return(new_func)
}
