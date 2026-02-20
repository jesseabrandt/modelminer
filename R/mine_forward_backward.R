# Internal: forward-backward stepwise model selection.
#
# Called by .mine_impl() after shared setup. Each iteration consists of:
#   Forward step  — try adding each candidate term; keep the best improvement.
#   Backward step — try removing each term currently in the model; keep the
#                   best improvement.
# Repeats until neither step improves the metric.
#
# Terms are tracked in *-notation (as passed in candidate_terms / initial_terms)
# rather than as expanded term labels, so the formula can be rebuilt cleanly
# for backward steps without : vs * bookkeeping.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_forward_backward <- function(candidate_terms, current_formula, current_metric,
                                   results, model_func, metric, metric_comparison,
                                   data, response_str, initial_terms = character(0)) {

  # added_terms: terms currently contributing to current_formula, in *-notation
  added_terms <- initial_terms

  keep_going <- TRUE
  while (keep_going) {
    changed <- FALSE

    # ---- Forward step ----

    if (length(candidate_terms) > 0) {
      fwd_formulas <- character(0)
      fwd_metrics  <- list()
      fwd_terms    <- character(0)   # which candidate was added for each trial

      for (term in candidate_terms) {
        try_formula <- .build_formula(response_str, c(added_terms, term))

        try_model <- tryCatch(
          model_func(formula = try_formula, data = data),
          error = function(e) {
            warning("Skipping term '", term, "' (forward): model fitting failed: ",
                    conditionMessage(e), call. = FALSE)
            NULL
          }
        )
        if (is.null(try_model)) next

        try_metric <- tryCatch(
          metric(try_model),
          error = function(e) {
            warning("Skipping term '", term, "' (forward): metric computation failed: ",
                    conditionMessage(e), call. = FALSE)
            NULL
          }
        )
        if (is.null(try_metric)) next

        cat("[fwd] Formula:", deparse1(try_formula), "Metric:", try_metric, "\n")

        results      <- rbind(results,
                              data.frame(Formula = deparse1(try_formula),
                                         Metric  = I(list(try_metric))))
        fwd_formulas <- c(fwd_formulas, deparse1(try_formula))
        fwd_metrics  <- c(fwd_metrics, list(try_metric))
        fwd_terms    <- c(fwd_terms, term)
      }

      if (length(fwd_formulas) > 0) {
        best_fwd    <- do.call(metric_comparison, fwd_metrics)
        best_global <- do.call(metric_comparison,
                               c(list(current_metric), fwd_metrics))

        if (!identical(best_global, current_metric)) {
          best_idx        <- which(sapply(fwd_metrics, identical, best_fwd))[1]
          new_term        <- fwd_terms[best_idx]
          added_terms     <- c(added_terms, new_term)
          current_formula <- as.formula(fwd_formulas[best_idx])
          current_metric  <- best_fwd

          # Prune candidates: remove anything now covered by the formula
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
      bwd_removed  <- character(0)  # which term was dropped for each trial

      for (term in added_terms) {
        try_formula <- .build_formula(response_str, setdiff(added_terms, term))

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

        cat("[bwd] Formula:", deparse1(try_formula), "Metric:", try_metric, "\n")

        results      <- rbind(results,
                              data.frame(Formula = deparse1(try_formula),
                                         Metric  = I(list(try_metric))))
        bwd_formulas <- c(bwd_formulas, deparse1(try_formula))
        bwd_metrics  <- c(bwd_metrics, list(try_metric))
        bwd_removed  <- c(bwd_removed, term)
      }

      if (length(bwd_formulas) > 0) {
        best_bwd    <- do.call(metric_comparison, bwd_metrics)
        best_global <- do.call(metric_comparison,
                               c(list(current_metric), bwd_metrics))

        if (!identical(best_global, current_metric)) {
          best_idx        <- which(sapply(bwd_metrics, identical, best_bwd))[1]
          dropped_term    <- bwd_removed[best_idx]
          added_terms     <- setdiff(added_terms, dropped_term)
          candidate_terms <- c(candidate_terms, dropped_term)  # can be re-added later
          current_formula <- as.formula(bwd_formulas[best_idx])
          current_metric  <- best_bwd
          changed         <- TRUE
        }
      }
    }

    if (!changed) keep_going <- FALSE
  }

  list(Formula = current_formula, all_models = results)
}
