# Forward-backward stepwise model selection.
#
# Each iteration runs two steps in sequence:
#   Forward  -- try adding each candidate term; keep the best improvement.
#   Backward -- try removing each term currently in the model; keep the best
#              improvement (or no change if none helps).
# The iteration repeats until neither step changes the model.
#
# Running backward after every forward step (rather than running all forward
# steps to convergence before any backward steps) lets the algorithm correct
# early additions that become redundant once better terms are found. The
# trade-off is more model evaluations per iteration.
#
# added_terms tracks terms in : notation, matching what attr(terms(), "term.labels")
# returns. This makes the formula rebuilding for backward steps straightforward:
# remove a term from the vector and pass the rest to .build_formula().
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_forward_backward <- function(candidate_terms, current_formula, current_metric,
                                   results, model_func, metric, metric_comparison,
                                   data, verbose = TRUE, response_str,
                                   initial_terms = character(0)) {

  # Terms currently in the model, tracked in : notation. Starts populated
  # when keep_all_vars = TRUE so those terms are eligible for backward removal.
  added_terms <- initial_terms
  rc          <- .results_collector(results)

  keep_going <- TRUE
  while (keep_going) {
    changed <- FALSE

    # ---- Forward step ----

    # Only offer I(var^k) when var is already in the formula (marginality).
    fwd_candidates <- .eligible_candidates(candidate_terms, current_formula)

    if (length(fwd_candidates) > 0) {
      fwd_formulas <- character(0)
      fwd_metrics  <- list()
      fwd_terms    <- character(0)  # which candidate was trialled for each entry

      for (term in fwd_candidates) {
        try_formula <- .build_formula(response_str, c(added_terms, term))

        fit_result <- .try_fit_metric(try_formula, model_func, metric, data,
                                      term_label = term, direction = "fwd",
                                      verbose = verbose)
        if (is.null(fit_result)) next

        rc$collect(deparse1(try_formula), fit_result$metric)
        fwd_formulas <- c(fwd_formulas, deparse1(try_formula))
        fwd_metrics  <- c(fwd_metrics, list(fit_result$metric))
        fwd_terms    <- c(fwd_terms, term)
      }

      if (length(fwd_formulas) > 0) {
        best_fwd <- do.call(metric_comparison, fwd_metrics)

        if (.metric_improved(best_fwd, current_metric, metric_comparison)) {
          best_idx        <- .find_best_index(fwd_metrics, metric_comparison)
          new_term        <- fwd_terms[best_idx]
          added_terms     <- c(added_terms, new_term)
          current_formula <- as.formula(fwd_formulas[best_idx])
          current_metric  <- best_fwd

          # Prune candidates that are now covered by the expanded formula.
          used            <- attr(stats::terms(current_formula), "term.labels")
          candidate_terms <- setdiff(candidate_terms, used)
          changed         <- TRUE
        }
      }
    }

    # ---- Backward step ----

    if (length(added_terms) > 0) {
      bwd_formulas <- character(0)
      bwd_metrics  <- list()
      bwd_removed  <- character(0)  # which term was omitted for each trial

      for (term in added_terms) {
        try_formula <- .build_formula(response_str, setdiff(added_terms, term))

        fit_result <- .try_fit_metric(try_formula, model_func, metric, data,
                                      term_label = term, direction = "bwd",
                                      verbose = verbose)
        if (is.null(fit_result)) next

        rc$collect(deparse1(try_formula), fit_result$metric)
        bwd_formulas <- c(bwd_formulas, deparse1(try_formula))
        bwd_metrics  <- c(bwd_metrics, list(fit_result$metric))
        bwd_removed  <- c(bwd_removed, term)
      }

      if (length(bwd_formulas) > 0) {
        best_bwd <- do.call(metric_comparison, bwd_metrics)

        if (.metric_improved(best_bwd, current_metric, metric_comparison)) {
          best_idx        <- .find_best_index(bwd_metrics, metric_comparison)
          dropped_term    <- bwd_removed[best_idx]
          added_terms     <- setdiff(added_terms, dropped_term)
          # The dropped term goes back into the candidate pool; a later forward
          # step could re-add it in a different context.
          candidate_terms <- c(candidate_terms, dropped_term)
          current_formula <- as.formula(bwd_formulas[best_idx])
          current_metric  <- best_bwd
          changed         <- TRUE
        }
      }
    }

    if (!changed) keep_going <- FALSE
  }

  list(Formula = current_formula, all_models = rc$finalize())
}
