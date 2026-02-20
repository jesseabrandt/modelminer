#' Title
#'
#' @param data
#' @param response_var
#' @param model_func
#' @param max_degree
#' @param max_interact_vars
#' @param metric
#' @param metric_comparison is a function that compares the two metrics and returns the preferable one. Defaults to min, can be max or some other comparison function.
#' @param keep_all_vars if set to TRUE, will keep all of the first-order terms in the model, just testing interactions and higher-order effects.
#'
#' @returns
#' @export
#'
#' @examples
#' @importFrom rlang enexpr as_string
mine <- function(data, response_var, model_func = lm,
  max_degree = 3, max_interact_vars = 2, metric = AIC, metric_comparison = min,
  keep_all_vars = FALSE) {

  # Create vector of possible predictor terms
  response_str <- as_string(enexpr(response_var))
  predictor_vars <- setdiff(names(data), response_str) # get predictor variables
  candidate_terms <- predictor_vars # renamed from `terms` to avoid shadowing base::terms()

  # Add polynomial terms for numeric variables only
  # (non-numeric columns such as factors cannot be raised to a power)
  # Guard against max_degree < 2: 2:1 in R produces c(2,1), not empty
  numeric_vars <- predictor_vars[sapply(predictor_vars, function(v) is.numeric(data[[v]]))]
  if (max_degree >= 2) {
    for (var in numeric_vars) {
      for (degree in 2:max_degree) {
        candidate_terms <- c(candidate_terms, paste0("I(", var, "^", degree, ")"))
      }
    }
  }

  # Add interaction terms
  # Using * rather than : so that adding an interaction also pulls in its main
  # effects if they are not already in the formula. This can find better models
  # in fewer greedy steps, at the cost of adding multiple terms at once.
  # Trade-off: : is cleaner for strict one-term-at-a-time greedy search.
  if (max_interact_vars > 1) {
    max_k <- min(max_interact_vars, length(predictor_vars))
    for (i in 1:(max_k - 1)) {
      if (i + 1 <= length(predictor_vars)) {
        interact_terms <- combn(predictor_vars, i + 1, function(vars) {
          paste(vars, collapse = "*") # trying with * instead of :
        })
        candidate_terms <- c(candidate_terms, interact_terms)
      }
    }
  }

  # create a starting formula
  if (keep_all_vars) {
    current_formula <- as.formula(paste(response_str, "~", paste(predictor_vars, collapse = " + ")))
  } else {
    current_formula <- as.formula(paste(response_str, "~ 1"))
  }

  # fit a model
  current_model <- model_func(current_formula, data = data)

  # get metric of model
  current_metric <- metric(current_model)

  cat("Formula:", deparse(current_formula), "Metric:", current_metric, "\n")

  # Metric column is list() to accommodate non-numeric metric objects
  results <- data.frame(Formula = deparse1(current_formula), Metric = I(list(current_metric)))

  keep_going <- TRUE
  while (keep_going) {
    if (length(candidate_terms) == 0) break

    # reset each iteration so formula recovery only looks at this round's candidates
    # Track as parallel lists so metric_comparison can be any variadic function
    round_formulas <- character(0)
    round_metrics  <- list()

    for (term in candidate_terms) {
      # create a formula with new term
      next_formula <- as.formula(paste(deparse1(current_formula), "+", term))

      next_model <- model_func(formula = next_formula, data = data)
      next_metric <- metric(next_model)

      cat("Formula:", deparse1(next_formula), "Metric:", next_metric, "\n")

      results <- rbind(results, data.frame(Formula = deparse1(next_formula), Metric = I(list(next_metric))))
      round_formulas <- c(round_formulas, deparse1(next_formula))
      round_metrics  <- c(round_metrics, list(next_metric))
    }

    # do.call passes each metric as a separate arg, so min/max/custom all work
    best_round_metric  <- do.call(metric_comparison, round_metrics)
    best_global_metric <- do.call(metric_comparison, c(list(current_metric), round_metrics))

    # Use identical() rather than == so non-numeric metrics work correctly
    if (identical(best_global_metric, current_metric)) {
      keep_going <- FALSE
    } else {
      current_metric <- best_round_metric
      best_idx <- which(sapply(round_metrics, identical, best_round_metric))[1]
      current_formula <- as.formula(round_formulas[best_idx])

      # remove term from list and add it to used terms list
      # attr(terms()) returns : notation; also strip * versions to handle interaction candidates
      used_terms <- attr(stats::terms(current_formula), "term.labels")
      used_terms_star <- gsub(":", "*", used_terms)
      candidate_terms <- setdiff(candidate_terms, c(used_terms, used_terms_star))
    }
  }

  return(list(Formula = current_formula, all_models = results)) # placeholder
}
