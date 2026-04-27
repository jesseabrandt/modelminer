# Build a formula from a response variable name and a character vector of terms.
# An empty terms vector produces response ~ 1 (intercept only).
.build_formula <- function(response_str, terms) {
  if (length(terms) == 0) {
    stats::reformulate("1", response = response_str)
  } else {
    stats::reformulate(terms, response = response_str)
  }
}

# Extract the base variable from a polynomial term like "I(x^2)".
# Returns NA_character_ for non-polynomial terms.
.poly_base_var <- function(term) {
  m <- regmatches(term, regexec("^I\\((.+)\\^[0-9]+\\)$", term))[[1]]
  if (length(m) == 2L) m[2L] else NA_character_
}

# Filter candidate terms so that polynomial terms I(var^k) are only included
# when var is already a main-effect term in the current formula (marginality
# principle).  Non-polynomial candidates pass through unchanged.
.eligible_candidates <- function(candidate_terms, current_formula) {
  current_labels <- attr(stats::terms(current_formula), "term.labels")
  Filter(function(t) {
    # Polynomial marginality: I(var^k) requires var in model
    bv <- .poly_base_var(t)
    if (!is.na(bv)) return(bv %in% current_labels)

    # Interaction marginality: a:b requires both a and b in model
    parts <- strsplit(t, ":", fixed = TRUE)[[1]]
    if (length(parts) > 1L) return(all(parts %in% current_labels))

    # First-order terms always eligible
    TRUE
  }, candidate_terms)
}

# Extracts the response vector y and predictor matrix x from a data frame
# using a formula. Used inside formula_wrap() to convert formula+data calls
# into the x/y interface expected by functions like glmnet.
to_xy <- function(data, formula) {
  response_var <- all.vars(formula)[1]
  y <- data[[response_var]]

  # model.matrix handles interactions, polynomials, and factor encoding.
  # The intercept column is dropped because model_func will add its own.
  x <- model.matrix(formula, data = data)[, -1, drop = FALSE]

  if (ncol(x) == 0L) {
    stop("formula_wrap: the formula '", deparse1(formula),
         "' produces no predictor columns after dropping the intercept.",
         call. = FALSE)
  }

  return(list(y = y, x = x))
}

# Tolerance-aware check: did new_metric improve over old_metric?
.metric_improved <- function(new_metric, old_metric, metric_comparison) {
  best <- do.call(metric_comparison, list(old_metric, new_metric))
  if (is.numeric(best) && length(best) == 1L &&
      is.numeric(old_metric) && length(old_metric) == 1L) {
    !isTRUE(all.equal(best, old_metric))
  } else {
    !identical(best, old_metric)
  }
}

# Find index of best metric in a list, using tolerance for numerics.
.find_best_index <- function(metrics, metric_comparison) {
  best <- do.call(metric_comparison, metrics)
  if (is.numeric(best) && length(best) == 1L) {
    which(vapply(metrics, function(m) {
      is.numeric(m) && length(m) == 1L && isTRUE(all.equal(m, best))
    }, logical(1)))[1L]
  } else {
    which(vapply(metrics, identical, logical(1), best))[1L]
  }
}

# Try fitting a model and computing its metric. Returns list(metric=) or NULL.
.try_fit_metric <- function(formula, model_func, metric, data,
                            term_label = "", direction = "", verbose = TRUE) {
  model <- tryCatch(
    model_func(formula = formula, data = data),
    error = function(e) {
      warning("Skipping ", if (nzchar(direction)) paste0("(", direction, ") "),
              "term '", term_label, "': model fitting failed: ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(model)) return(NULL)

  met <- tryCatch(
    metric(model),
    error = function(e) {
      warning("Skipping ", if (nzchar(direction)) paste0("(", direction, ") "),
              "term '", term_label, "': metric computation failed: ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(met)) return(NULL)

  if (verbose) {
    prefix <- if (nzchar(direction)) paste0("[", direction, "] ") else ""
    message(prefix, "Formula: ", deparse1(formula), " Metric: ", met)
  }
  list(metric = met)
}

# List-based result accumulator to avoid O(n^2) rbind.
.results_collector <- function(initial_results) {
  chunks <- list(initial_results)
  list(
    collect = function(formula_str, metric_val) {
      chunks[[length(chunks) + 1L]] <<- data.frame(
        Formula = formula_str, Metric = I(list(metric_val))
      )
    },
    finalize = function() {
      do.call(rbind, chunks)
    }
  )
}
