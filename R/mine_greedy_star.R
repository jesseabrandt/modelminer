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
                               data, predictor_vars, numeric_vars,
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

  keep_going <- TRUE
  while (keep_going) {
    if (length(star_candidates) == 0) break

    round_formulas <- character(0)
    round_metrics  <- list()
    round_terms    <- character(0)  # original candidate string for each trial

    for (term in star_candidates) {
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

      message("Formula: ", deparse1(next_formula), " Metric: ", next_metric)

      results        <- rbind(results,
                              data.frame(Formula = deparse1(next_formula),
                                         Metric  = I(list(next_metric))))
      round_formulas <- c(round_formulas, deparse1(next_formula))
      round_metrics  <- c(round_metrics, list(next_metric))
      round_terms    <- c(round_terms, term)
    }

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

      # Prune by expanded term labels (covers first-order and : components of
      # any * term that was selected) AND by the original candidate string
      # itself (since "a*b" does not appear in term.labels -- only "a", "b",
      # "a:b" do).
      used_terms      <- c(attr(stats::terms(current_formula), "term.labels"),
                           round_terms[best_idx])
      star_candidates <- setdiff(star_candidates, used_terms)
    }
  }

  list(Formula = current_formula, all_models = results)
}
