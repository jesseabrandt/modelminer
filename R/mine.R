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
  predictor_vars <- setdiff(names(data), as_string(enexpr(response_var)))# get predictor variables
  terms <- predictor_vars


  # Add polynomial terms
  for (var in predictor_vars) {
    for (degree in 2:max_degree) {
      terms <- c(terms, paste0("I(", var, "^", degree, ")"))
    }
  }

  # Add interaction terms
  # May need to update to include higher order terms
  if (max_interact_vars > 1) {
    max_k <- min(max_interact_vars, length(predictor_vars))
    for (i in 1:(max_k - 1)) {
      if (i + 1 <= length(predictor_vars)) {
        interact_terms <- combn(predictor_vars, i + 1, function(vars) {
          paste(vars, collapse = ":")
        })
        terms <- c(terms, interact_terms)
      }
    }
  }

  # create a starting formula

  if (keep_all_vars) {
    current_formula <- as.formula(paste(enexpr(response_var), "~", paste(predictor_vars, collapse = " + ")))
  } else {
    current_formula <- as.formula(paste(enexpr(response_var), "~ 1"))
  }

  # fit a model
  current_model <- (model_func(current_formula, data = data))

  # get metric of model
  current_metric <- metric(current_model)

  cat("Formula:", deparse(current_formula), "Metric:", current_metric, "\n")

  results <- data.frame(Formula = deparse1(current_formula), Metric = current_metric)


  {
    next_models <- data.frame(Formula = deparse1(current_formula), Metric = current_metric)
    for(term in terms) {
      # create a formula with new term
      next_formula <- as.formula(paste(deparse1(current_formula), " + ", term))#PLACEHOLDER

      next_model <- model_func(formula = next_formula, data = data)
      next_metric <- metric(next_model)

      cat("Formula:", deparse1(next_formula), "Metric:", next_metric, "\n")

      results <- rbind(results, data.frame(Formula = deparse(next_formula), Metric = next_metric))
      next_models <- rbind(next_models, data.frame(Formula = deparse1(next_formula), Metric = next_metric))

      # replace current model if new one is better
      # if (metric_comparison(next_metric, current_metric) == next_metric & next_metric != current_metric) {
      #
      #   current_model <- next_model
      #   current_metric <- metric(current_model)
      #   current_formula <- next_formula
      # }

    }
    current_metric <- metric_comparison(results$Metric)
    # print(current_metric)
    current_formula <- next_models$Formula[results$Metric == current_metric][1]
    # print(current_formula)
  }
  return(list(Formula = current_formula, all_models = results))# placeholder
  
}
