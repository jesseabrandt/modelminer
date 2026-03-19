# Greedy forward stepwise model selection.
#
# Each round, every remaining candidate term is tried in turn. The single
# term that most improves the metric is added, and terms now covered by the
# formula are pruned from the candidate pool. Repeats until no candidate
# improves on the current model.
#
# This is a local search: the first term added influences which terms look
# useful later, so the path through candidate space depends on the order
# metric improvements happen. It will not always find the globally best
# subset -- that is what the (unimplemented) exhaustive method is for.
#
# Terms that cause model-fitting or metric errors are skipped with a warning
# so that one bad candidate (e.g., a singular column combination) does not
# abort the entire search.
.mine_greedy <- function(candidate_terms, current_formula, current_metric,
                         results, model_func, metric, metric_comparison, data,
                         verbose = TRUE) {

  # Track current terms explicitly so we can rebuild formulas via
  # .build_formula() rather than string-pasting onto deparse1() output.
  current_terms   <- attr(stats::terms(current_formula), "term.labels")
  response_str    <- as.character(current_formula[[2]])
  rc              <- .results_collector(results)

  keep_going <- TRUE
  while (keep_going) {
    if (length(candidate_terms) == 0) break

    # Only offer I(var^k) when var is already in the formula (marginality).
    round_candidates <- .eligible_candidates(candidate_terms, current_formula)
    if (length(round_candidates) == 0) break

    # Parallel vectors rather than a data frame because metric_comparison is
    # called with do.call(), which needs the metrics as a plain list.
    round_formulas <- list()
    round_metrics  <- list()
    round_added    <- character(0)  # which candidate term each trial added

    for (term in round_candidates) {
      next_formula <- .build_formula(response_str, c(current_terms, term))

      fit_result <- .try_fit_metric(next_formula, model_func, metric, data,
                                    term_label = term, verbose = verbose)
      if (is.null(fit_result)) next

      rc$collect(deparse1(next_formula), fit_result$metric)
      round_formulas <- c(round_formulas, list(next_formula))
      round_metrics  <- c(round_metrics, list(fit_result$metric))
      round_added    <- c(round_added, term)
    }

    if (length(round_formulas) == 0) break  # every candidate failed to fit

    if (!.metric_improved(do.call(metric_comparison, round_metrics),
                          current_metric, metric_comparison)) {
      keep_going <- FALSE
    } else {
      best_idx        <- .find_best_index(round_metrics, metric_comparison)
      current_metric  <- round_metrics[[best_idx]]
      current_formula <- round_formulas[[best_idx]]
      current_terms   <- c(current_terms, round_added[best_idx])

      # Remove terms that are now in the formula from the candidate pool.
      candidate_terms <- setdiff(candidate_terms, current_terms)
    }
  }

  list(Formula = current_formula, all_models = rc$finalize())
}
