# Phased greedy model selection.
#
# Works in three sequential phases, each building on what the previous phase
# selected rather than searching a single pre-built candidate pool:
#
#   Phase 1 — first-order:
#     Greedy forward selection over plain predictor variables. Establishes
#     which variables have any marginal signal at all.
#
#   Phase 2 — polynomial:
#     Greedy forward selection over polynomial terms, but ONLY for variables
#     that were selected in phase 1. No point offering I(x^k) if x itself
#     did not improve the model.
#
#   Phase 3 — interaction:
#     Greedy forward selection over interactions built from ALL terms currently
#     in the model after phase 2 — including polynomial terms. Interactions
#     therefore involve only variables that showed a marginal benefit, and
#     higher-order terms can participate (e.g. hp:I(wt^2) is offered if both
#     hp and I(wt^2) were selected).
#
# Compared to the standard greedy, this enforces main-effects-before-
# interactions and avoids polynomial expansion of variables that had no
# first-order signal. The tradeoff is that the phases are independent: a
# variable that only matters in an interaction — with no main effect — will
# be missed by phase 1 and never offered in phase 3. The standard greedy
# can sometimes find those cases because it considers the full pool at once.
#
# Returns list(Formula, all_models) matching the mine() contract.
#
# full_interactions: if FALSE (default, method = "greedy_alt"), phase 3 builds
# interaction candidates only from terms already in the model after phase 2.
# This means variables with no main-effect signal are excluded from interactions.
# If TRUE (method = "greedy_alt_full"), phase 3 pools ALL predictor variables
# together with any phase-2 polynomial terms, so a variable that only matters
# inside an interaction can still be found.
.mine_greedy_alt <- function(candidate_terms, current_formula, current_metric,
                              results, model_func, metric, metric_comparison,
                              data, predictor_vars, numeric_vars,
                              max_degree, max_interact_vars,
                              full_interactions = FALSE) {

  # candidate_terms is the pre-built global pool passed by mine() for all
  # other algorithms. It is not used here; each phase builds its own pool.

  # ── Phase 1: first-order terms ─────────────────────────────────────────────

  cat("\n── Phase 1: first-order terms ──\n")
  phase1 <- .greedy_phase(
    candidates        = predictor_vars,
    current_formula   = current_formula,
    current_metric    = current_metric,
    results           = results,
    model_func        = model_func,
    metric            = metric,
    metric_comparison = metric_comparison,
    data              = data
  )
  current_formula <- phase1$formula
  current_metric  <- phase1$metric
  results         <- phase1$results

  # ── Phase 2: polynomial terms for selected variables only ──────────────────

  # Inspect the formula after phase 1 to find which base variables were chosen.
  selected_base_vars <- intersect(
    attr(stats::terms(current_formula), "term.labels"),
    predictor_vars
  )
  selected_numeric <- intersect(selected_base_vars, numeric_vars)

  poly_candidates <- character(0)
  if (max_degree >= 2 && length(selected_numeric) > 0) {
    for (var in selected_numeric) {
      for (degree in 2:max_degree) {
        poly_candidates <- c(poly_candidates, paste0("I(", var, "^", degree, ")"))
      }
    }
  }

  if (length(poly_candidates) > 0) {
    cat("\n── Phase 2: polynomial terms ──\n")
    phase2 <- .greedy_phase(
      candidates        = poly_candidates,
      current_formula   = current_formula,
      current_metric    = current_metric,
      results           = results,
      model_func        = model_func,
      metric            = metric,
      metric_comparison = metric_comparison,
      data              = data
    )
    current_formula <- phase2$formula
    current_metric  <- phase2$metric
    results         <- phase2$results
  } else {
    cat("\n── Phase 2: skipped (no numeric variables selected in phase 1) ──\n")
  }

  # ── Phase 3: interactions ──────────────────────────────────────────────────

  # The pool of terms to combine into interactions differs by mode:
  #
  #   greedy_alt (full_interactions = FALSE):
  #     Interactions are built from terms already in the model. A variable
  #     that had no main-effect signal in phase 1 is absent from the model
  #     and therefore absent from all interaction candidates. Cleaner pool,
  #     but pure-interaction predictors are missed.
  #
  #   greedy_alt_full (full_interactions = TRUE):
  #     Interactions are built from ALL predictor variables (regardless of
  #     whether phase 1 selected them) plus any polynomial terms added in
  #     phase 2. This lets pure-interaction predictors participate, at the
  #     cost of a larger candidate pool that may include noisy interactions.
  current_term_labels <- attr(stats::terms(current_formula), "term.labels")
  poly_in_model       <- grep("^I\\(", current_term_labels, value = TRUE)

  interact_pool <- if (full_interactions) {
    c(predictor_vars, poly_in_model)
  } else {
    current_term_labels
  }

  interact_candidates <- character(0)
  if (max_interact_vars > 1 && length(interact_pool) >= 2) {
    max_k <- min(max_interact_vars, length(interact_pool))
    for (i in seq_len(max_k - 1)) {
      new_terms <- combn(interact_pool, i + 1, function(vars) {
        paste(vars, collapse = ":")
      })
      interact_candidates <- c(interact_candidates, new_terms)
    }
    # Drop interactions that are already in the formula
    interact_candidates <- setdiff(interact_candidates, current_term_labels)
  }

  if (length(interact_candidates) > 0) {
    cat("\n── Phase 3: interaction terms ──\n")
    phase3 <- .greedy_phase(
      candidates        = interact_candidates,
      current_formula   = current_formula,
      current_metric    = current_metric,
      results           = results,
      model_func        = model_func,
      metric            = metric,
      metric_comparison = metric_comparison,
      data              = data
    )
    current_formula <- phase3$formula
    results         <- phase3$results
  } else {
    cat("\n── Phase 3: skipped (fewer than 2 terms selected) ──\n")
  }

  list(Formula = current_formula, all_models = results)
}


# One greedy forward selection pass over a fixed candidate set.
#
# Shared by all phases of .mine_greedy_alt. Does not know which phase it is
# running; the caller is responsible for supplying the right candidate set.
#
# Returns list(formula, metric, results) so the caller can chain phases.
.greedy_phase <- function(candidates, current_formula, current_metric,
                           results, model_func, metric, metric_comparison, data) {

  keep_going <- TRUE
  while (keep_going) {
    if (length(candidates) == 0) break

    round_formulas <- character(0)
    round_metrics  <- list()

    for (term in candidates) {
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

      used_terms <- attr(stats::terms(current_formula), "term.labels")
      candidates <- setdiff(candidates, used_terms)
    }
  }

  list(formula = current_formula, metric = current_metric, results = results)
}
