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
                             data, verbose = TRUE, response_str,
                             max_terms = NULL) {

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
  rc           <- .results_collector(results)

  for (k in seq_len(max_terms)) {
    n_subsets <- choose(length(candidate_terms), k)
    if (verbose) message("Size ", k, ": evaluating ", n_subsets, " subsets...")

    subsets <- combn(candidate_terms, k, simplify = FALSE)

    for (subset in subsets) {
      try_formula <- .build_formula(response_str, c(base_terms, subset))

      fit_result <- .try_fit_metric(try_formula, model_func, metric, data,
                                    term_label = paste(subset, collapse = ", "),
                                    verbose = verbose)
      if (is.null(fit_result)) next

      rc$collect(deparse1(try_formula), fit_result$metric)

      if (.metric_improved(fit_result$metric, best_metric, metric_comparison)) {
        best_metric  <- fit_result$metric
        best_formula <- try_formula
      }
    }
  }

  if (verbose) message("Exhaustive search complete: evaluated ",
                       nrow(rc$finalize()), " models.")

  list(Formula = best_formula, all_models = rc$finalize())
}
