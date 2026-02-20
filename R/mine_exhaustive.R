# Best-subset exhaustive model selection.
#
# Evaluates all 2^n subsets of candidate_terms and returns the globally best
# model by metric. Unlike the greedy and forward-backward methods, this is
# not path-dependent: it finds the true optimum within the candidate pool.
#
# The practical problem is that the candidate pool can be large. With p
# predictors, max_degree polynomial terms, and pairwise interactions you can
# easily have 30-50 candidates, making 2^50 subsets infeasible. Before
# implementing this, consider:
#   - Restricting the candidate pool before calling (e.g., only first-order
#     terms, or only variables that pass an initial screening).
#   - Branch-and-bound pruning (if metric is monotone under nesting, which
#     AIC is not in general).
#   - Capping the subset size and running it as a best-k-term search.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_exhaustive <- function(candidate_terms, current_formula, current_metric,
                             results, model_func, metric, metric_comparison,
                             data, response_str) {
  stop("Exhaustive search is not yet implemented.")
}
