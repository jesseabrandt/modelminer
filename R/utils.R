# this function is used inside formula_wrap
to_xy <- function(data, formula) {
  # print("test")
  # Get the response variable and predictor variables from the formula
  response_var <- all.vars(formula)[1]
  # print("ok")
  predictors <- setdiff(names(data), as_string(enexpr(response_var)))

  # Extract the response variable (y) and predictor variables (X) from the data
  y <- data[[response_var]]
  x <- data[predictors]

  # convert x to matrix
  x <- as.matrix(x)
  return(list(y = y, x = x))
}

