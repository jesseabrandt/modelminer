# Build a formula from a response variable name and a character vector of terms.
# An empty terms vector produces response ~ 1 (intercept only).
.build_formula <- function(response_str, terms) {
  if (length(terms) == 0) {
    as.formula(paste(response_str, "~ 1"))
  } else {
    as.formula(paste(response_str, "~", paste(terms, collapse = " + ")))
  }
}

# Extract the base variable from a polynomial term like "I(x^2)".
# Returns NA_character_ for non-polynomial terms.
.poly_base_var <- function(term) {
  m <- regmatches(term, regexec("^I\\((.+)\\^[0-9]+\\)$", term))[[1]]
  if (length(m) == 2L) m[2L] else NA_character_
}

# Filter candidate terms so that polynomial terms I(var^k) are only included
# when var is already a main-effect term in the current formula (marginality
# principle).  Non-polynomial candidates pass through unchanged.
.eligible_candidates <- function(candidate_terms, current_formula) {
  current_labels <- attr(stats::terms(current_formula), "term.labels")
  Filter(function(t) {
    bv <- .poly_base_var(t)
    is.na(bv) || bv %in% current_labels
  }, candidate_terms)
}

# Extracts the response vector y and predictor matrix x from a data frame
# using a formula. Used inside formula_wrap() to convert formula+data calls
# into the x/y interface expected by functions like glmnet.
to_xy <- function(data, formula) {
  response_var <- all.vars(formula)[1]
  y <- data[[response_var]]

  # model.matrix handles interactions, polynomials, and factor encoding.
  # The intercept column is dropped because model_func will add its own.
  x <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  if (ncol(x) == 0L) {
    stop("formula_wrap: the formula '", deparse1(formula),
         "' produces no predictor columns after dropping the intercept.",
         call. = FALSE)
  }

  return(list(y = y, x = x))
}
