# Lasso / elastic-net model selection via glmnet.
#
# Instead of iteratively adding or removing terms, lasso uses L1
# regularisation to select variables in a single pass over the full
# candidate term pool.  cv.glmnet picks the regularisation strength
# (lambda) by cross-validation, and we read off the non-zero
# coefficients at two standard rules:
#   lambda.min -- the lambda that minimises cross-validated error
#   lambda.1se -- the largest lambda within 1 SE of the minimum
#                 (sparser, more conservative)
#
# Both models are recorded in all_models.  The `lambda_rule` argument
# controls which one becomes the returned Formula.
#
# Factor variables: model.matrix() expands a factor f with levels
# A, B, C into dummy columns fB, fC.  If *any* dummy for a factor has
# a non-zero coefficient, the original term `f` is selected.
#
# After lasso selects terms, mine()'s post-dispatch code refits using
# whatever model_func the user passed (default lm), giving the standard
# "lasso-select, OLS-refit" workflow automatically.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_lasso <- function(candidate_terms, current_formula, current_metric,
                        results, model_func, metric, metric_comparison,
                        data, verbose = TRUE, response_str,
                        lambda_rule = "lambda.min", ...) {

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for method = 'lasso'. ",
         "Install it with install.packages('glmnet').", call. = FALSE)
  }

  lambda_rule <- match.arg(lambda_rule, c("lambda.min", "lambda.1se"))

  # ---- Build full design matrix from all candidates ----

  # Combine any terms already in the starting formula with the candidate pool.
  # Note: lasso bypasses the marginality filtering (.eligible_candidates) used
  # by stepwise methods.  Regularisation may validly select a quadratic term
  # without its linear term (L1 can shrink the linear coefficient to zero while
  # keeping the quadratic).  Users who require strict marginality should use a
  # stepwise method.
  base_terms <- attr(stats::terms(current_formula), "term.labels")
  all_terms  <- unique(c(base_terms, candidate_terms))

  if (length(all_terms) == 0) {
    return(list(Formula = current_formula, all_models = results))
  }

  full_formula <- .build_formula(response_str, all_terms)

  # model.matrix() expands factors, polynomials, and interactions.
  # Drop the intercept column -- glmnet adds its own by default.
  mf <- tryCatch(
    stats::model.frame(full_formula, data = data, na.action = stats::na.omit),
    error = function(e) {
      stop("Failed to build model frame for lasso: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  x <- stats::model.matrix(full_formula, data = mf)[, -1, drop = FALSE]
  y <- stats::model.response(mf)

  # cv.glmnet requires at least 2 predictor columns (ncol >= 2).
  # Return the starting formula unchanged if there are 0 or 1 columns so we
  # get a clean no-op rather than a cryptic "x should be a matrix with 2 or
  # more columns" error from glmnet.
  if (ncol(x) < 2L) {
    warning("Lasso requires at least 2 predictor columns in the design matrix. ",
            "Only ", ncol(x), " column(s) found after building candidate pool. ",
            "Returning the starting formula unchanged.", call. = FALSE)
    return(list(Formula = current_formula, all_models = results))
  }

  # ---- Fit cv.glmnet ----

  # Save and restore RNG state so callers' randomness is not affected
  # (cv.glmnet uses random fold assignments internally).
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  dots <- list(...)
  # Remove arguments that are mine()-specific, not cv.glmnet args
  mine_args <- c("candidate_terms", "current_formula", "current_metric",
                 "results", "model_func", "metric", "metric_comparison",
                 "data", "response_str", "lambda_rule")
  dots <- dots[!names(dots) %in% mine_args]

  cv_fit <- tryCatch(
    do.call(glmnet::cv.glmnet, c(list(x = x, y = y), dots)),
    error = function(e) {
      stop("cv.glmnet failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  # ---- Extract selected terms for both lambda rules ----

  formulas_out <- list()
  for (rule in c("lambda.min", "lambda.1se")) {
    lam <- cv_fit[[rule]]
    coefs <- stats::coef(cv_fit, s = lam)
    nonzero_cols <- .extract_nonzero_cols(coefs)

    # Map model.matrix column names back to formula terms.
    selected_terms <- .colnames_to_terms(nonzero_cols, all_terms, data)

    if (length(selected_terms) == 0) {
      f <- stats::as.formula(paste(response_str, "~ 1"))
    } else {
      f <- .build_formula(response_str, selected_terms)
    }

    # Evaluate with the user's model_func and metric for comparable all_models rows.
    fit <- tryCatch(model_func(f, data = data), error = function(e) NULL)
    met <- if (!is.null(fit)) {
      tryCatch(metric(fit), error = function(e) NA_real_)
    } else {
      NA_real_
    }

    # Also record the CV metric from glmnet for informational purposes.
    cv_idx <- which(cv_fit$lambda == lam)
    cv_mse <- if (length(cv_idx) == 1) cv_fit$cvm[cv_idx] else NA_real_

    if (verbose) message("[lasso:", rule, "] Formula: ", deparse1(f),
            " Metric: ", met, " (CV: ", round(cv_mse, 4), ")")

    results <- rbind(results,
                     data.frame(Formula = deparse1(f),
                                Metric  = I(list(met))))

    formulas_out[[rule]] <- f
  }

  best_formula <- formulas_out[[lambda_rule]]

  list(Formula = best_formula, all_models = results)
}


# Lasso path model selection via glmnet.
#
# Walks the full regularization path and records every model at which the
# set of selected variables changes (i.e. each variable entry/exit point).
# Each distinct variable set is refit with model_func and evaluated with
# the user's metric, giving a rich all_models table that shows how model
# quality evolves as regularization loosens.
#
# Unlike .mine_lasso (which uses cv.glmnet and returns 2 models), this
# uses the non-CV glmnet() path directly and can return 5-15+ models.
# The "best" formula is whichever optimizes the user's metric.
#
# Returns list(Formula, all_models) matching the mine() contract.
.mine_lasso_path <- function(candidate_terms, current_formula, current_metric,
                             results, model_func, metric, metric_comparison,
                             data, verbose = TRUE, response_str,
                             lambda_rule = NULL, ...) {

  if (!is.null(lambda_rule)) {
    warning("'lambda_rule' is ignored for method = 'lasso_path'. ",
            "The best formula is determined by the user's metric.",
            call. = FALSE)
  }

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for method = 'lasso_path'. ",
         "Install it with install.packages('glmnet').", call. = FALSE)
  }

  # ---- Build full design matrix from all candidates ----

  base_terms <- attr(stats::terms(current_formula), "term.labels")
  all_terms  <- unique(c(base_terms, candidate_terms))

  if (length(all_terms) == 0) {
    return(list(Formula = current_formula, all_models = results))
  }

  full_formula <- .build_formula(response_str, all_terms)

  mf <- tryCatch(
    stats::model.frame(full_formula, data = data, na.action = stats::na.omit),
    error = function(e) {
      stop("Failed to build model frame for lasso_path: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  x <- stats::model.matrix(full_formula, data = mf)[, -1, drop = FALSE]
  y <- stats::model.response(mf)

  # glmnet() (non-CV) accepts ncol >= 1, but with a single column the path is

  # trivial and the refit step may fail for model_func's that need >= 2 columns.
  # Use the same >= 2 guard as cv.glmnet for consistency.
  if (ncol(x) < 2L) {
    warning("Lasso path requires at least 2 predictor columns in the design matrix. ",
            "Returning the starting formula unchanged.", call. = FALSE)
    return(list(Formula = current_formula, all_models = results))
  }

  # ---- Fit glmnet (non-CV) ----

  dots <- list(...)
  mine_args <- c("candidate_terms", "current_formula", "current_metric",
                 "results", "model_func", "metric", "metric_comparison",
                 "data", "response_str", "lambda_rule")
  dots <- dots[!names(dots) %in% mine_args]

  fit <- tryCatch(
    do.call(glmnet::glmnet, c(list(x = x, y = y), dots)),
    error = function(e) {
      stop("glmnet failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  # ---- Walk the lambda path, record each distinct variable set ----

  is_multinomial <- is.list(fit$beta)
  if (!is_multinomial) {
    beta <- as.matrix(fit$beta)  # p x nlambda
  }
  best_formula    <- current_formula
  best_metric     <- current_metric
  prev_key        <- ""  # track previous variable set to skip duplicates
  n_before        <- nrow(results)

  for (i in seq_along(fit$lambda)) {
    if (is_multinomial) {
      # Union nonzero rows across all class-specific beta matrices at column i
      nonzero_cols <- unique(unlist(lapply(fit$beta, function(b) {
        bm <- as.matrix(b)
        rownames(bm)[bm[, i] != 0]
      })))
    } else {
      nonzero_cols <- rownames(beta)[beta[, i] != 0]
    }
    selected_terms <- .colnames_to_terms(nonzero_cols, all_terms, data)

    # Deduplicate: skip if same variable set as previous lambda
    key <- paste(sort(selected_terms), collapse = "+")
    if (key == prev_key) next
    prev_key <- key

    if (length(selected_terms) == 0) {
      f <- stats::as.formula(paste(response_str, "~ 1"))
    } else {
      f <- .build_formula(response_str, selected_terms)
    }

    # Refit with user's model_func and evaluate with user's metric
    m <- tryCatch(model_func(f, data = data), error = function(e) NULL)
    met <- if (!is.null(m)) {
      tryCatch(metric(m), error = function(e) NA_real_)
    } else {
      NA_real_
    }

    n_terms <- length(selected_terms)
    if (verbose) message("[lasso_path] lambda=", signif(fit$lambda[i], 4),
            " terms=", n_terms,
            " Formula: ", deparse1(f), " Metric: ", met)

    results <- rbind(results,
                     data.frame(Formula = deparse1(f),
                                Metric  = I(list(met))))

    # Track best model.  Guard against best_metric being NA (e.g. when the
    # intercept-only starting model failed and current_metric was set to a
    # fallback value): min(NA, x) returns NA, so the identical() check would
    # never trigger an update.
    if (!is.na(met)) {
      if (is.na(best_metric)) {
        best_metric  <- met
        best_formula <- f
      } else {
        comparison <- do.call(metric_comparison, list(best_metric, met))
        if (!identical(comparison, best_metric)) {
          best_metric  <- met
          best_formula <- f
        }
      }
    }
  }

  if (verbose) message("Lasso path complete: ", nrow(results) - n_before, " distinct models evaluated.")

  list(Formula = best_formula, all_models = results)
}


# Extract nonzero coefficient column names from glmnet coef() output.
# For gaussian/binomial family, coef() returns a single sparse matrix.
# For multinomial family, coef() returns a list of sparse matrices (one per
# class).  This helper unions nonzero rows across all classes so that a
# predictor selected for any class is included.
.extract_nonzero_cols <- function(coefs) {
  if (is.list(coefs) && !is.matrix(coefs)) {
    # Multinomial: list of sparse matrices, one per class
    all_nonzero <- unique(unlist(lapply(coefs, function(cm) {
      cm <- as.matrix(cm)
      # Drop intercept row
      cm <- cm[-1, , drop = FALSE]
      rownames(cm)[cm[, 1] != 0]
    })))
    return(all_nonzero)
  }
  # Single matrix (gaussian, binomial, etc.)
  coefs <- as.matrix(coefs)
  coefs <- coefs[-1, , drop = FALSE]
  rownames(coefs)[coefs[, 1] != 0]
}


# Map model.matrix column names (which may include factor dummies like fB, fC,
# or polynomial columns) back to the original formula terms.
#
# Uses the "assign" attribute of model.matrix, which maps every column index
# to a 1-based index into term.labels (0 = intercept).  This is the only
# reliable way to handle all cases correctly:
#
#   - Numeric term "x"    -> column "x"       (assign = k)
#   - Factor term "f"     -> columns "fB","fC" (assign = k, k)
#   - Poly term "I(x^2)"  -> column "I(x^2)"  (assign = k)
#   - Interaction "f:x"   -> columns "fB:x","fC:x" (assign = k, k)
#   - Factor:factor "f1:f2" -> columns "f1B:f2Y",... (assign = k, k)
#
# The previous implementation tried to recover this mapping via string prefix
# matching (startsWith), which failed for factor interaction columns like
# "fB:x": startsWith("fB:x", "f") matched the main-effect term "f" before
# the interaction term "f:x" could be considered, so interaction terms were
# silently dropped.
.colnames_to_terms <- function(nonzero_cols, all_terms, data) {
  if (length(nonzero_cols) == 0) return(character(0))

  # Build the full design matrix for all candidate terms so we can read its
  # "assign" attribute.  We use a one-sided formula so no response is needed.
  full_formula <- stats::as.formula(paste("~", paste(all_terms, collapse = " + ")))

  mf <- tryCatch(
    stats::model.frame(full_formula, data = data, na.action = stats::na.omit),
    error = function(e) NULL
  )
  if (is.null(mf)) {
    # Fallback: exact-match only (handles pure-numeric cases without factors)
    return(unique(nonzero_cols[nonzero_cols %in% all_terms]))
  }

  mm        <- stats::model.matrix(full_formula, data = mf)
  tl        <- attr(stats::terms(full_formula), "term.labels")
  assign_v  <- attr(mm, "assign")     # length == ncol(mm), value 0 = intercept

  # Build a named character vector: col_name -> source term (empty for intercept)
  col_to_term          <- character(ncol(mm))
  names(col_to_term)   <- colnames(mm)
  for (i in seq_along(assign_v)) {
    idx <- assign_v[i]
    if (idx > 0L) col_to_term[i] <- tl[idx]
  }

  # Look up each nonzero column name; skip any that don't appear in the map
  # (shouldn't happen, but guards against mismatches if data changed shape).
  matched <- col_to_term[nonzero_cols]
  matched <- matched[!is.na(matched) & nchar(matched) > 0L]

  unique(matched)
}
