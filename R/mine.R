#' Automated model selection with feature engineering
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response_var The name of the response variable (unquoted).
#' @param model_func A model function accepting \code{formula} and \code{data} arguments.
#'   Defaults to \code{lm}.
#' @param max_degree Maximum degree for polynomial terms. Defaults to 3.
#' @param max_interact_vars Maximum number of variables in interaction terms. Defaults to 2.
#' @param metric A function to compute the model selection metric. Defaults to \code{AIC}.
#' @param metric_comparison A function that compares metric values and returns the preferable one.
#'   Defaults to \code{min}.
#' @param keep_all_vars If \code{TRUE}, starts with all first-order terms in the formula.
#'   Defaults to \code{FALSE}.
#' @param method Search algorithm to use. One of \code{"greedy"} (default),
#'   \code{"forward_backward"}, or \code{"exhaustive"}. May also be a custom search
#'   function — see Details.
#'
#' @details
#' When \code{method} is a function it must have the signature:
#' \preformatted{
#'   function(candidate_terms, current_formula, current_metric,
#'            results, model_func, metric, metric_comparison, data)
#' }
#' and return \code{list(Formula, all_models)} matching the standard contract.
#' This lets you pass experimental ("draft") search implementations for comparison
#' via \code{\link{compare_methods}}.
#'
#' @returns A list with two elements: \code{Formula} (the best formula found) and
#'   \code{all_models} (a data frame of all evaluated formulas and their metric values).
#' @export
#'
#' @examples
#' result <- mine(mtcars, mpg)
#' result$Formula
#' @importFrom rlang enexpr as_string
#' @importFrom stats AIC as.formula lm
#' @importFrom utils combn
mine <- function(data, response_var, model_func = lm,
                 max_degree = 3, max_interact_vars = 2, metric = AIC,
                 metric_comparison = min, keep_all_vars = FALSE,
                 method = "greedy") {
  response_str <- as_string(enexpr(response_var))
  .mine_impl(data, response_str,
             model_func        = model_func,
             max_degree        = max_degree,
             max_interact_vars = max_interact_vars,
             metric            = metric,
             metric_comparison = metric_comparison,
             keep_all_vars     = keep_all_vars,
             method            = method)
}

# Internal workhorse — accepts response_var as a plain string so it can be
# called programmatically (e.g. from compare_methods()) without NSE friction.
#
# All arguments mirror mine() exactly, except response_str replaces response_var.
.mine_impl <- function(data, response_str, model_func = lm,
                       max_degree = 3, max_interact_vars = 2, metric = AIC,
                       metric_comparison = min, keep_all_vars = FALSE,
                       method = "greedy") {

  if (!is.function(method)) {
    method <- match.arg(method, c("greedy", "forward_backward", "exhaustive"))
  }

  # ---- Shared setup: candidate term pool ----

  predictor_vars  <- setdiff(names(data), response_str)
  candidate_terms <- predictor_vars

  # Polynomial terms (numeric columns only)
  numeric_vars <- predictor_vars[
    sapply(predictor_vars, function(v) is.numeric(data[[v]]))
  ]
  if (max_degree >= 2) {
    for (var in numeric_vars) {
      for (degree in 2:max_degree) {
        candidate_terms <- c(candidate_terms, paste0("I(", var, "^", degree, ")"))
      }
    }
  }

  # Interaction terms — * rather than : so adding one term also brings in main
  # effects. See CLAUDE.md for the design trade-off.
  if (max_interact_vars > 1 && length(predictor_vars) >= 2) {
    max_k <- min(max_interact_vars, length(predictor_vars))
    for (i in seq_len(max_k - 1)) {
      interact_terms <- combn(predictor_vars, i + 1, function(vars) {
        paste(vars, collapse = "*")
      })
      candidate_terms <- c(candidate_terms, interact_terms)
    }
  }

  # ---- Starting formula ----

  if (keep_all_vars) {
    start_formula   <- as.formula(paste(response_str, "~",
                                        paste(predictor_vars, collapse = " + ")))
    initial_terms   <- predictor_vars   # tracked for forward_backward bookkeeping
  } else {
    start_formula   <- as.formula(paste(response_str, "~ 1"))
    initial_terms   <- character(0)
  }

  start_model <- tryCatch(
    model_func(start_formula, data = data),
    error = function(e) {
      stop("Failed to fit starting model: ", conditionMessage(e), call. = FALSE)
    }
  )

  start_metric <- tryCatch(
    metric(start_model),
    error = function(e) {
      stop("Failed to compute metric for starting model: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  cat("Formula:", deparse(start_formula), "Metric:", start_metric, "\n")

  start_results <- data.frame(
    Formula = deparse1(start_formula),
    Metric  = I(list(start_metric))
  )

  # ---- Dispatch to search algorithm ----

  common_args <- list(
    candidate_terms   = candidate_terms,
    current_formula   = start_formula,
    current_metric    = start_metric,
    results           = start_results,
    model_func        = model_func,
    metric            = metric,
    metric_comparison = metric_comparison,
    data              = data
  )

  if (is.function(method)) {
    do.call(method, common_args)
  } else if (method == "greedy") {
    do.call(.mine_greedy, common_args)
  } else if (method == "forward_backward") {
    do.call(.mine_forward_backward,
            c(common_args, list(response_str = response_str,
                                initial_terms = initial_terms)))
  } else {
    do.call(.mine_exhaustive,
            c(common_args, list(response_str = response_str)))
  }
}
