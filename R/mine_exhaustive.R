# Best-subset exhaustive model selection.
#
# Evaluates all subsets of candidate_terms from size 1 up to max_terms and
# returns the globally best model by metric. Unlike the greedy and
# forward-backward methods, this is not path-dependent: it finds the true
# optimum within the candidate pool (up to the size cap).
#
# The max_terms cap avoids the full 2^n combinatorial explosion. With p
# candidates and max_terms = k, the number of subsets evaluated is
# sum(choose(p, 1:k)), which is manageable for moderate p and small k.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_exhaustive <- function(candidate_terms, current_formula, current_metric,
                             results, model_func, metric, metric_comparison,
                             data, response_str, max_terms = NULL) {

  if (is.null(max_terms)) max_terms <- 5L
  max_terms <- min(max_terms, length(candidate_terms))

  if (max_terms < 1) {
    return(list(Formula = current_formula, all_models = results))
  }

  # Extract base terms already in the formula (non-empty when keep_all_vars = TRUE).
  base_terms <- attr(stats::terms(current_formula), "term.labels")

  # Only search over candidates not already in the model.
  candidate_terms <- setdiff(candidate_terms, base_terms)
  max_terms <- min(max_terms, length(candidate_terms))

  if (max_terms < 1) {
    return(list(Formula = current_formula, all_models = results))
  }

  # Warn if the search space is large.
  total_combns <- sum(vapply(seq_len(max_terms), function(k) {
    choose(length(candidate_terms), k)
  }, numeric(1)))
  if (total_combns > 50000) {
    warning("Exhaustive search will evaluate ", total_combns,
            " subsets. This may take a while.", call. = FALSE)
  }

  best_formula <- current_formula
  best_metric  <- current_metric

  for (k in seq_len(max_terms)) {
    n_subsets <- choose(length(candidate_terms), k)
    cat("Size ", k, ": evaluating ", n_subsets, " subsets...\n", sep = "")

    subsets <- combn(candidate_terms, k, simplify = FALSE)

    for (subset in subsets) {
      try_formula <- .build_formula(response_str, c(base_terms, subset))

      try_model <- tryCatch(
        model_func(formula = try_formula, data = data),
        error = function(e) {
          warning("Skipping subset {", paste(subset, collapse = ", "),
                  "}: model fitting failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(try_model)) next

      try_metric <- tryCatch(
        metric(try_model),
        error = function(e) {
          warning("Skipping subset {", paste(subset, collapse = ", "),
                  "}: metric computation failed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (is.null(try_metric)) next

      results <- rbind(results,
                       data.frame(Formula = deparse1(try_formula),
                                  Metric  = I(list(try_metric))))

      comparison <- do.call(metric_comparison, list(best_metric, try_metric))
      if (!identical(comparison, best_metric)) {
        best_metric  <- try_metric
        best_formula <- try_formula
      }
    }
  }

  cat("Exhaustive search complete: evaluated ", nrow(results), " models.\n", sep = "")

  list(Formula = best_formula, all_models = results)
}
