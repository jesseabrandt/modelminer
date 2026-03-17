# Pure backward elimination model selection.
#
# Starts from a model containing all initial_terms and iteratively removes the
# single term whose removal most improves the metric. Stops when no removal
# improves the current model.
#
# This is the complement of greedy forward selection: instead of building up
# from an intercept-only model, it prunes down from a full model. The two
# approaches can find different local optima because the search paths through
# model space differ.
#
# candidate_terms is accepted for interface consistency but unused -- backward
# elimination only removes terms, never adds them.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_backward <- function(candidate_terms, current_formula, current_metric,
                           results, model_func, metric, metric_comparison,
                           data, verbose = TRUE, response_str,
                           initial_terms = character(0)) {

  current_terms <- initial_terms

  while (length(current_terms) > 0) {
    bwd_formulas <- character(0)
    bwd_metrics  <- list()
    bwd_removed  <- character(0)

    for (term in current_terms) {
      try_formula <- .build_formula(response_str, setdiff(current_terms, term))

      try_model <- tryCatch(
        model_func(formula = try_formula, data = data),
        error = function(e) {
          warning("Skipping removal of '", term, "' (backward): model fitting failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(try_model)) next

      try_metric <- tryCatch(
        metric(try_model),
        error = function(e) {
          warning("Skipping removal of '", term, "' (backward): metric computation failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(try_metric)) next

      if (verbose) message("[bwd] Formula: ", deparse1(try_formula), " Metric: ", try_metric)

      results      <- rbind(results,
                            data.frame(Formula = deparse1(try_formula),
                                       Metric  = I(list(try_metric))))
      bwd_formulas <- c(bwd_formulas, deparse1(try_formula))
      bwd_metrics  <- c(bwd_metrics, list(try_metric))
      bwd_removed  <- c(bwd_removed, term)
    }

    if (length(bwd_formulas) == 0) break

    best_bwd    <- do.call(metric_comparison, bwd_metrics)
    best_global <- do.call(metric_comparison,
                           c(list(current_metric), bwd_metrics))

    if (identical(best_global, current_metric)) {
      break
    } else {
      best_idx        <- which(sapply(bwd_metrics, identical, best_bwd))[1]
      dropped_term    <- bwd_removed[best_idx]
      current_terms   <- setdiff(current_terms, dropped_term)
      current_formula <- as.formula(bwd_formulas[best_idx])
      current_metric  <- best_bwd
    }
  }

  list(Formula = current_formula, all_models = results)
}
