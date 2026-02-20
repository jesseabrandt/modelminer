# Build a formula from a response variable name and a character vector of terms.
# An empty terms vector produces response ~ 1 (intercept only).
.build_formula <- function(response_str, terms) {
  if (length(terms) == 0) {
    as.formula(paste(response_str, "~ 1"))
  } else {
    as.formula(paste(response_str, "~", paste(terms, collapse = " + ")))
  }
}

#' @importFrom stats model.matrix
# Extracts the response vector y and predictor matrix x from a data frame
# using a formula. Used inside formula_wrap() to convert formula+data calls
# into the x/y interface expected by functions like glmnet.
to_xy <- function(data, formula) {
  response_var <- all.vars(formula)[1]
  y <- data[[response_var]]

  # model.matrix handles interactions, polynomials, and factor encoding.
  # The intercept column is dropped because model_func will add its own.
  x <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  return(list(y = y, x = x))
}
