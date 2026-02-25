# Phased greedy (and phased forward-backward) model selection.
#
# Works in three sequential phases, each building its candidate pool from what
# the previous phase produced rather than searching a single pre-built pool:
#
#   Phase 1 — first-order:
#     Selection over plain predictor variables. Establishes which variables
#     have any marginal signal at all.
#
#   Phase 2 — polynomial:
#     Selection over polynomial terms, but ONLY for variables selected in
#     phase 1. No point offering I(x^k) if x itself did not improve the model.
#
#   Phase 3 — interaction:
#     Selection over interactions built from terms in the model after phase 2.
#     With full_interactions = FALSE (default), only already-selected terms
#     participate, so the pool is small and clean but pure-interaction
#     predictors are missed. With full_interactions = TRUE, ALL predictor
#     variables are eligible in phase 3, catching variables that only matter
#     inside an interaction.
#
# do_backward controls whether each phase uses forward-only (.greedy_phase) or
# forward-backward (.greedy_phase_fb) search. The backward step within a phase
# can only remove terms that phase added — terms from earlier phases are locked
# in. This keeps phases independent and avoids phase 2 undoing phase 1.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_greedy_alt <- function(candidate_terms, current_formula, current_metric,
                              results, model_func, metric, metric_comparison,
                              data, predictor_vars, numeric_vars,
                              max_degree, max_interact_vars,
                              full_interactions = FALSE,
                              do_backward = FALSE,
                              response_str = NULL) {

  # candidate_terms is the pre-built global pool passed by mine() for all
  # other algorithms. It is not used here; each phase builds its own pool.
  #
  # response_str is only needed for do_backward = TRUE (the forward-backward
  # helper uses .build_formula(), which requires the response name as a string).

  # ── Phase runner: dispatch to forward-only or forward-backward ──────────────

  run_phase <- if (do_backward) {
    function(candidates, prior_terms, ...) {
      .greedy_phase_fb(candidates  = candidates,
                       prior_terms = prior_terms,
                       response_str = response_str, ...)
    }
  } else {
    function(candidates, prior_terms, ...) {
      .greedy_phase(candidates = candidates, ...)
    }
  }

  # ── Phase 1: first-order terms ─────────────────────────────────────────────

  cat("\n── Phase 1: first-order terms ──\n")
  phase1 <- run_phase(
    candidates        = predictor_vars,
    prior_terms       = character(0),
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

  # prior_terms for phase 2 = everything phase 1 left in the model
  prior_after_1 <- attr(stats::terms(current_formula), "term.labels")

  if (length(poly_candidates) > 0) {
    cat("\n── Phase 2: polynomial terms ──\n")
    phase2 <- run_phase(
      candidates        = poly_candidates,
      prior_terms       = prior_after_1,
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
  #   full_interactions = FALSE (greedy_alt / greedy_alt_fb):
  #     Interactions are built from terms already in the model after phase 2.
  #     A variable with no main-effect signal never reaches phase 3.
  #     Cleaner, smaller pool; pure-interaction predictors are missed.
  #
  #   full_interactions = TRUE (greedy_alt_full / greedy_alt_full_fb):
  #     Interactions are built from ALL predictor variables plus any polynomial
  #     terms added in phase 2. Variables that only matter inside an interaction
  #     can still be found. Larger pool, more models evaluated.
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
    interact_candidates <- setdiff(interact_candidates, current_term_labels)
  }

  prior_after_2 <- current_term_labels

  if (length(interact_candidates) > 0) {
    cat("\n── Phase 3: interaction terms ──\n")
    phase3 <- run_phase(
      candidates        = interact_candidates,
      prior_terms       = prior_after_2,
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


# ── .greedy_phase: forward-only pass ─────────────────────────────────────────
#
# One greedy forward selection pass over a fixed candidate set.
# Shared by all forward-only phases of .mine_greedy_alt.
#
# Returns list(formula, metric, results).
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


# ── .greedy_phase_fb: forward-backward pass ───────────────────────────────────
#
# Forward-backward selection within a single phase's candidate set.
#
# prior_terms: terms in the model from earlier phases — these are LOCKED and
#   cannot be removed by this phase's backward step. Only terms this phase
#   adds are eligible for removal. This keeps phases independent: phase 2
#   cannot undo phase 1 decisions, which would make the sequencing meaningless.
#
# phase_terms: tracks terms added during this phase (starts empty).
#   The backward step iterates over phase_terms only.
#   A dropped term returns to the candidate pool; a later forward step can
#   re-add it in a different context.
#
# Returns list(formula, metric, results, phase_terms).
.greedy_phase_fb <- function(candidates, prior_terms, response_str,
                              current_formula, current_metric,
                              results, model_func, metric, metric_comparison, data) {

  phase_terms <- character(0)

  keep_going <- TRUE
  while (keep_going) {
    changed <- FALSE

    # ---- Forward step -------------------------------------------------------

    if (length(candidates) > 0) {
      fwd_formulas <- character(0)
      fwd_metrics  <- list()
      fwd_terms    <- character(0)

      for (term in candidates) {
        try_formula <- .build_formula(response_str, c(prior_terms, phase_terms, term))

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
          phase_terms     <- c(phase_terms, new_term)
          candidates      <- setdiff(candidates,
                                     attr(stats::terms(as.formula(fwd_formulas[best_idx])),
                                          "term.labels"))
          current_formula <- as.formula(fwd_formulas[best_idx])
          current_metric  <- best_fwd
          changed         <- TRUE
        }
      }
    }

    # ---- Backward step (phase_terms only) -----------------------------------
    #
    # Only terms this phase added are eligible for removal. prior_terms (from
    # earlier phases) are never touched — removing them would invalidate the
    # phased structure (e.g. phase 2 dropping a phase-1 main effect mid-search).

    if (length(phase_terms) > 0) {
      bwd_formulas <- character(0)
      bwd_metrics  <- list()
      bwd_removed  <- character(0)

      for (term in phase_terms) {
        remaining   <- setdiff(phase_terms, term)
        try_formula <- .build_formula(response_str, c(prior_terms, remaining))

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
          phase_terms     <- setdiff(phase_terms, dropped_term)
          candidates      <- c(candidates, dropped_term)
          current_formula <- as.formula(bwd_formulas[best_idx])
          current_metric  <- best_bwd
          changed         <- TRUE
        }
      }
    }

    if (!changed) keep_going <- FALSE
  }

  list(formula     = current_formula,
       metric      = current_metric,
       results     = results,
       phase_terms = phase_terms)
}
