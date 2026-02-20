# Internal: greedy forward stepwise model selection.
#
# Called by .mine_impl() after shared setup. Iteratively adds the single
# candidate term that most improves the metric until no improvement is found.
#
# Terms that cause model-fitting or metric errors are skipped with a warning.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_greedy <- function(candidate_terms, current_formula, current_metric,
                         results, model_func, metric, metric_comparison, data) {

  keep_going <- TRUE
  while (keep_going) {
    if (length(candidate_terms) == 0) break

    round_formulas <- character(0)
    round_metrics  <- list()

    for (term in candidate_terms) {
      next_formula <- as.formula(paste(deparse1(current_formula), "+", term))

      next_model <- tryCatch(
        model_func(formula = next_formula, data = data),
        error = function(e) {
          warning("Skipping term '", term, "': model fitting failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(next_model)) next

      next_metric <- tryCatch(
        metric(next_model),
        error = function(e) {
          warning("Skipping term '", term, "': metric computation failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(next_metric)) next

      cat("Formula:", deparse1(next_formula), "Metric:", next_metric, "\n")

      results        <- rbind(results,
                              data.frame(Formula = deparse1(next_formula),
                                         Metric  = I(list(next_metric))))
      round_formulas <- c(round_formulas, deparse1(next_formula))
      round_metrics  <- c(round_metrics, list(next_metric))
    }

    # All candidates in this round failed to fit
    if (length(round_formulas) == 0) break

    best_round_metric  <- do.call(metric_comparison, round_metrics)
    best_global_metric <- do.call(metric_comparison,
                                  c(list(current_metric), round_metrics))

    if (identical(best_global_metric, current_metric)) {
      keep_going <- FALSE
    } else {
      current_metric  <- best_round_metric
      best_idx        <- which(sapply(round_metrics, identical, best_round_metric))[1]
      current_formula <- as.formula(round_formulas[best_idx])

      used_terms      <- attr(stats::terms(current_formula), "term.labels")
      candidate_terms <- setdiff(candidate_terms, used_terms)
    }
  }

  list(Formula = current_formula, all_models = results)
}
