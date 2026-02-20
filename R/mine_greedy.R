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
# subset — that is what the (unimplemented) exhaustive method is for.
#
# Terms that cause model-fitting or metric errors are skipped with a warning
# so that one bad candidate (e.g., a singular column combination) does not
# abort the entire search.
.mine_greedy <- function(candidate_terms, current_formula, current_metric,
                         results, model_func, metric, metric_comparison, data) {

  keep_going <- TRUE
  while (keep_going) {
    if (length(candidate_terms) == 0) break

    # Parallel vectors rather than a data frame because metric_comparison is
    # called with do.call(), which needs the metrics as a plain list.
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

    if (length(round_formulas) == 0) break  # every candidate failed to fit

    best_round_metric  <- do.call(metric_comparison, round_metrics)
    # Compare the round's best against the current model to decide whether to
    # continue. Using identical() rather than == so non-numeric metrics work
    # and floating-point equality is tested by value, not by ==.
    best_global_metric <- do.call(metric_comparison,
                                  c(list(current_metric), round_metrics))

    if (identical(best_global_metric, current_metric)) {
      keep_going <- FALSE
    } else {
      current_metric  <- best_round_metric
      # If multiple terms tie on the metric, [1] picks the first one.
      # Ties are rare in practice; the choice is deterministic but arbitrary.
      best_idx        <- which(sapply(round_metrics, identical, best_round_metric))[1]
      current_formula <- as.formula(round_formulas[best_idx])

      # Remove terms that are now in the formula from the candidate pool.
      # attr(terms(), "term.labels") expands interactions (e.g. a:b stays a:b)
      # so with : notation these match candidate strings directly.
      used_terms      <- attr(stats::terms(current_formula), "term.labels")
      candidate_terms <- setdiff(candidate_terms, used_terms)
    }
  }

  list(Formula = current_formula, all_models = results)
}
