# Greedy forward stepwise model selection using * interaction candidates.
#
# Identical to .mine_greedy in structure, but the interaction part of the
# candidate pool uses * instead of :. The difference matters:
#
#   a:b  -- adds only the interaction term. If a and b are not already in the
#           model, the result is an interaction without its main effects.
#
#   a*b  -- adds a + b + a:b in one step. Main effects are always present
#           alongside their interaction, so the model stays hierarchically
#           correct regardless of what was selected before.
#
# The tradeoff: a * candidate adds up to three terms at once, which is a
# coarser step than :. If only one main effect or only the interaction is
# useful, the : approach can find that more precisely; * forces the bundle.
#
# Candidate pruning note: after adding a*b, attr(terms(), "term.labels")
# returns ["a", "b", "a:b"] -- not "a*b". The *-string itself must be removed
# from the candidate pool explicitly by tracking which candidate was selected.
# That is what round_terms does below.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_greedy_star <- function(candidate_terms, current_formula, current_metric,
                               results, model_func, metric, metric_comparison,
                               data, verbose = TRUE, predictor_vars, numeric_vars,
                               max_degree, max_interact_vars) {

  # Rebuild the candidate pool using * for interactions.
  # candidate_terms (the : pool from mine()) is intentionally ignored.
  star_candidates <- predictor_vars

  if (max_degree >= 2) {
    for (var in numeric_vars) {
      for (degree in 2:max_degree) {
        star_candidates <- c(star_candidates, paste0("I(", var, "^", degree, ")"))
      }
    }
  }

  if (max_interact_vars > 1 && length(predictor_vars) >= 2) {
    max_k <- min(max_interact_vars, length(predictor_vars))
    for (i in seq_len(max_k - 1)) {
      star_terms <- combn(predictor_vars, i + 1, function(vars) {
        paste(vars, collapse = "*")
      })
      star_candidates <- c(star_candidates, star_terms)
    }
  }

  # Track current terms explicitly for .build_formula() usage.
  current_terms <- attr(stats::terms(current_formula), "term.labels")
  response_str  <- as.character(current_formula[[2]])
  rc            <- .results_collector(results)

  keep_going <- TRUE
  while (keep_going) {
    if (length(star_candidates) == 0) break

    round_formulas <- list()
    round_metrics  <- list()
    round_terms    <- character(0)  # original candidate string for each trial

    for (term in star_candidates) {
      # Use .build_formula with the * term appended; R's formula parser
      # expands a*b into a + b + a:b automatically.
      next_formula <- .build_formula(response_str, c(current_terms, term))

      fit_result <- .try_fit_metric(next_formula, model_func, metric, data,
                                    term_label = term, verbose = verbose)
      if (is.null(fit_result)) next

      rc$collect(deparse1(next_formula), fit_result$metric)
      round_formulas <- c(round_formulas, list(next_formula))
      round_metrics  <- c(round_metrics, list(fit_result$metric))
      round_terms    <- c(round_terms, term)
    }

    if (length(round_formulas) == 0) break

    if (!.metric_improved(do.call(metric_comparison, round_metrics),
                          current_metric, metric_comparison)) {
      keep_going <- FALSE
    } else {
      best_idx        <- .find_best_index(round_metrics, metric_comparison)
      current_metric  <- round_metrics[[best_idx]]
      current_formula <- round_formulas[[best_idx]]

      # Prune by expanded term labels (covers first-order and : components of
      # any * term that was selected) AND by the original candidate string
      # itself (since "a*b" does not appear in term.labels -- only "a", "b",
      # "a:b" do).
      used_terms      <- c(attr(stats::terms(current_formula), "term.labels"),
                           round_terms[best_idx])
      star_candidates <- setdiff(star_candidates, used_terms)
    }
  }

  list(Formula = current_formula, all_models = rc$finalize())
}
