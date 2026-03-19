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
  rc            <- .results_collector(results)

  while (length(current_terms) > 0) {
    bwd_formulas <- character(0)
    bwd_metrics  <- list()
    bwd_removed  <- character(0)

    for (term in current_terms) {
      try_formula <- .build_formula(response_str, setdiff(current_terms, term))

      fit_result <- .try_fit_metric(try_formula, model_func, metric, data,
                                    term_label = term, direction = "bwd",
                                    verbose = verbose)
      if (is.null(fit_result)) next

      rc$collect(deparse1(try_formula), fit_result$metric)
      bwd_formulas <- c(bwd_formulas, deparse1(try_formula))
      bwd_metrics  <- c(bwd_metrics, list(fit_result$metric))
      bwd_removed  <- c(bwd_removed, term)
    }

    if (length(bwd_formulas) == 0) break

    best_bwd <- do.call(metric_comparison, bwd_metrics)

    if (!.metric_improved(best_bwd, current_metric, metric_comparison)) {
      break
    } else {
      best_idx        <- .find_best_index(bwd_metrics, metric_comparison)
      dropped_term    <- bwd_removed[best_idx]
      current_terms   <- setdiff(current_terms, dropped_term)
      current_formula <- as.formula(bwd_formulas[best_idx])
      current_metric  <- best_bwd
    }
  }

  list(Formula = current_formula, all_models = rc$finalize())
}
