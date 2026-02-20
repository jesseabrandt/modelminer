#' @importFrom stats model.matrix
# this function is used inside formula_wrap
to_xy <- function(data, formula) {
  # Get the response variable from the formula
  response_var <- all.vars(formula)[1]

  # Extract response vector
  y <- data[[response_var]]

  # Use model.matrix to respect the formula's RHS (including interactions,
  # polynomials, and factor encoding). Drop the intercept column.
  x <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  return(list(y = y, x = x))
}
