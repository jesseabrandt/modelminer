#' Automated model selection with feature engineering
#'
#' Starting from an intercept-only (or all-first-order) model, iteratively adds
#' and optionally removes terms -- polynomial expansions, interactions, and
#' first-order predictors -- that most improve a user-supplied metric. Multiple
#' search strategies are available, from greedy forward selection to
#' L1-regularised lasso.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response_var The name of the response variable (unquoted).
#' @param model_func A model function accepting \code{formula} and \code{data} arguments.
#'   Defaults to \code{lm}.
#' @param max_degree Maximum degree for polynomial terms. Defaults to 3.
#' @param max_interact_vars Maximum number of variables in interaction terms. Defaults to 2.
#' @param metric A function to compute the model selection metric. Defaults to \code{AIC}.
#' @param metric_comparison A function that compares metric values and returns the preferable one.
#'   Defaults to \code{min}.
#' @param keep_all_vars If \code{TRUE}, starts with all first-order terms in the formula.
#'   Defaults to \code{FALSE}.
#' @param method Search algorithm to use. One of:
#'   \itemize{
#'     \item \code{"greedy"} (default) -- forward selection, interaction candidates use \code{:}
#'     \item \code{"greedy_star"} -- same but interaction candidates use \code{*}, so main effects are always included with their interaction
#'     \item \code{"greedy_alt"} -- phased: first-order, polynomial (selected vars), interactions (selected terms)
#'     \item \code{"greedy_alt_full"} -- same phases, but phase 3 considers interactions among all predictors
#'     \item \code{"greedy_alt_fb"} -- \code{greedy_alt} with forward-backward within each phase
#'     \item \code{"greedy_alt_full_fb"} -- \code{greedy_alt_full} with forward-backward within each phase
#'     \item \code{"forward_backward"} -- forward-backward over a single pre-built pool
#'     \item \code{"backward"} -- pure backward elimination from all predictors
#'     \item \code{"exhaustive"} -- best-subset selection up to \code{max_terms} terms
#'     \item \code{"lasso"} -- L1-regularised selection via \code{glmnet::cv.glmnet()}
#'       over the full candidate pool. Requires the \pkg{glmnet} package.
#'       \code{keep_all_vars} affects the starting formula but lasso considers
#'       the full candidate pool regardless.
#'       Extra arguments in \code{...} are passed to \code{cv.glmnet()}, e.g.
#'       \code{alpha} (0 = ridge, 0.5 = elastic net, 1 = lasso), \code{nfolds},
#'       \code{family}.
#'     \item \code{"lasso_path"} -- walks the full \code{glmnet()} regularization
#'       path and records every model at which the selected variable set changes
#'       (each variable entry/exit point). Produces a richer \code{all_models}
#'       table than \code{"lasso"}. The best formula is whichever optimizes
#'       the user's \code{metric}. Requires the \pkg{glmnet} package.
#'   }
#'   May also be a custom search function -- see Details.
#' @param max_terms Maximum number of terms to include in a subset for the
#'   exhaustive method. Defaults to 5 if \code{NULL}. Ignored by other methods.
#' @param lambda_rule For \code{method = "lasso"}: which lambda to use for the
#'   selected formula. One of \code{"lambda.min"} (default, best CV performance)
#'   or \code{"lambda.1se"} (sparser, within 1 SE of the minimum). Both models
#'   appear in \code{all_models} regardless of this choice. Ignored by other
#'   methods.
#' @param verbose If \code{TRUE} (the default), print progress messages showing
#'   each model evaluated during the search. Set to \code{FALSE} to suppress
#'   all iteration output.
#' @param ... Additional arguments forwarded to \code{\link[glmnet]{cv.glmnet}}
#'   (for \code{method = "lasso"}) or \code{\link[glmnet]{glmnet}} (for
#'   \code{method = "lasso_path"}). Common options include \code{alpha},
#'   \code{nfolds}, and \code{family}. Ignored for all other built-in methods.
#'
#' @details
#' When \code{method} is a function it must accept at least these positional
#' arguments:
#' \preformatted{
#'   function(candidate_terms, current_formula, current_metric,
#'            results, model_func, metric, metric_comparison, data, ...)
#' }
#' Built-in methods may also receive \code{response_str} and other named
#' arguments via \code{...}.  A custom function can accept and ignore these
#' with a \code{...} catch-all.
#'
#' The function must return \code{list(Formula, all_models)} matching the
#' standard contract.  This lets you pass experimental ("draft") search
#' implementations for comparison via \code{\link{compare_methods}}.
#'
#' @returns A list with five elements:
#'   \describe{
#'     \item{\code{Formula}}{The best formula found, as a \code{formula} object.}
#'     \item{\code{all_models}}{A data frame of every formula evaluated and its
#'       metric value (list column, to support non-numeric metrics).}
#'     \item{\code{model}}{The fitted model object for the best formula, ready
#'       for \code{summary()}, \code{predict()}, \code{coef()}, etc.}
#'     \item{\code{best_metric}}{The metric value for the best formula as a
#'       plain numeric scalar.}
#'     \item{\code{method}}{The search algorithm used (\code{"greedy"},
#'       \code{"greedy_star"}, \code{"greedy_alt"}, \code{"greedy_alt_full"},
#'       \code{"greedy_alt_fb"}, \code{"greedy_alt_full_fb"},
#'       \code{"forward_backward"}, \code{"backward"}, \code{"exhaustive"},
#'       \code{"lasso"}, \code{"lasso_path"}, or \code{"custom"}).}
#'   }
#' @export
#'
#' @examples
#' result <- mine(mtcars, mpg)
#' result$Formula
#' @importFrom rlang enexpr as_string
#' @importFrom stats AIC as.formula lm hatvalues model.frame model.matrix model.response residuals
#' @importFrom utils combn
mine <- function(data, response_var, model_func = lm,
                 max_degree = 3, max_interact_vars = 2, metric = AIC,
                 metric_comparison = min, keep_all_vars = FALSE,
                 method = "greedy", max_terms = NULL,
                 lambda_rule = "lambda.min", verbose = TRUE, ...) {
  response_str <- as_string(enexpr(response_var))
  .mine_impl(data, response_str,
             model_func        = model_func,
             max_degree        = max_degree,
             max_interact_vars = max_interact_vars,
             metric            = metric,
             metric_comparison = metric_comparison,
             keep_all_vars     = keep_all_vars,
             method            = method,
             max_terms         = max_terms,
             lambda_rule       = lambda_rule,
             verbose           = verbose,
             ...)
}

# Internal workhorse. Accepts response_var as a plain string rather than an
# unquoted symbol so it can be called via do.call() from compare_methods()
# without fighting R's NSE rules. All other arguments are identical to mine().
.mine_impl <- function(data, response_str, model_func = lm,
                       max_degree = 3, max_interact_vars = 2, metric = AIC,
                       metric_comparison = min, keep_all_vars = FALSE,
                       method = "greedy", max_terms = NULL,
                       lambda_rule = "lambda.min", verbose = TRUE, ...) {

  # Input validation
  if (!is.data.frame(data))
    stop("'data' must be a data frame.", call. = FALSE)
  if (nrow(data) == 0L)
    stop("'data' has no rows.", call. = FALSE)
  if (!response_str %in% names(data))
    stop("Response variable '", response_str, "' not found in data.", call. = FALSE)
  if (ncol(data) < 2L)
    stop("'data' must have at least 2 columns (response + 1 predictor).", call. = FALSE)

  if (!is.function(method)) {
    method <- match.arg(method, c("greedy", "greedy_star",
                                   "greedy_alt", "greedy_alt_full",
                                   "greedy_alt_fb", "greedy_alt_full_fb",
                                   "forward_backward", "backward",
                                   "exhaustive", "lasso", "lasso_path"))
  }

  # ---- Shared setup: candidate term pool ----

  predictor_vars  <- setdiff(names(data), response_str)

  # NA handling: drop incomplete rows and warn
  used_cols <- c(response_str, predictor_vars)
  complete <- stats::complete.cases(data[, used_cols, drop = FALSE])
  if (!all(complete)) {
    n_drop <- sum(!complete)
    warning("Dropped ", n_drop, " row(s) with NA values in response/predictor columns. ",
            "All candidate models will be fit on the same ", sum(complete), " complete rows.",
            call. = FALSE)
    data <- data[complete, , drop = FALSE]
  }

  # Small-n AIC warning
  if (identical(metric, AIC)) {
    n <- nrow(data)
    p <- length(predictor_vars)
    if (p > 0 && n < 10 * p) {
      warning("AIC may be unreliable with only ", n, " observations and ", p,
              " predictors (ratio ", round(n / p, 1), ":1). ",
              "Consider using make_cv_metric() or lm_loocv for small samples.",
              call. = FALSE)
    }
  }

  candidate_terms <- predictor_vars

  # Only numeric variables can be raised to a power; factors are excluded.
  numeric_vars <- predictor_vars[
    sapply(predictor_vars, function(v) is.numeric(data[[v]]))
  ]
  if (max_degree >= 2) {
    for (var in numeric_vars) {
      for (degree in 2:max_degree) {
        candidate_terms <- c(candidate_terms, paste0("I(", var, "^", degree, ")"))
      }
    }
  }

  # Interaction terms are generated with : rather than *, so each candidate
  # represents only the interaction itself -- no implicit main effects.
  # This keeps the search strict: one term added or removed per step, and
  # added_terms bookkeeping in forward_backward stays unambiguous.
  #
  # The downside is that a:b without a and b already in the model is
  # statistically awkward (interaction without main effects). The current
  # design relies on the search finding a and b first if they improve the
  # metric, but there's no enforcement. A future improvement might be to
  # require main effects as prerequisites before their interaction is offered
  # as a candidate, or to group them and treat the whole family as one step.
  if (max_interact_vars > 1 && length(predictor_vars) >= 2) {
    max_k <- min(max_interact_vars, length(predictor_vars))
    for (i in seq_len(max_k - 1)) {
      interact_terms <- combn(predictor_vars, i + 1, function(vars) {
        paste(vars, collapse = ":")
      })
      candidate_terms <- c(candidate_terms, interact_terms)
    }
  }

  # ---- Starting formula ----

  if (keep_all_vars) {
    start_formula <- as.formula(paste(response_str, "~",
                                      paste(predictor_vars, collapse = " + ")))
    # forward_backward needs to know which terms are already in the model so
    # it can consider dropping them during backward steps.
    initial_terms <- predictor_vars
  } else {
    start_formula <- as.formula(paste(response_str, "~ 1"))
    initial_terms <- character(0)
  }

  # Backward elimination must start from all predictors.
  if (identical(method, "backward") && !keep_all_vars) {
    start_formula <- as.formula(paste(response_str, "~",
                                      paste(predictor_vars, collapse = " + ")))
    initial_terms <- predictor_vars
  }

  # Try the starting model. If keep_all_vars = FALSE and the intercept-only
  # model fails (e.g. randomForest needs >= 1 predictor), fall back to the
  # best single-predictor model.  When keep_all_vars = TRUE the failure stays
  # fatal because the user explicitly asked for all vars -- silently dropping
  # them would be surprising.
  start_model <- tryCatch(
    model_func(start_formula, data = data),
    error = function(e) e
  )

  start_metric <- if (!inherits(start_model, "error")) {
    tryCatch(metric(start_model), error = function(e) e)
  } else {
    start_model  # propagate the error
  }

  intercept_ok <- !inherits(start_model, "error") &&
                  !inherits(start_metric, "error")

  if (!intercept_ok && keep_all_vars) {
    # Fatal -- user asked for all vars, no silent fallback.
    msg <- if (inherits(start_model, "error"))
      conditionMessage(start_model) else conditionMessage(start_metric)
    stop("Failed to fit starting model: ", msg, call. = FALSE)
  }

  start_results <- data.frame(Formula = character(0),
                               Metric  = I(list()))

  if (intercept_ok) {
    if (verbose) message("Formula: ", deparse1(start_formula), " Metric: ", start_metric)
    start_results <- data.frame(
      Formula = deparse1(start_formula),
      Metric  = I(list(start_metric))
    )
  } else {
    # ---- Single-predictor fallback ----
    if (verbose) message("Intercept-only model failed; trying each predictor individually...")
    fallback_metrics  <- list()
    fallback_formulas <- list()

    for (var in predictor_vars) {
      f <- as.formula(paste(response_str, "~", var))
      m <- tryCatch(model_func(f, data = data), error = function(e) {
        warning("Fallback: could not fit ", deparse1(f), ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      })
      if (is.null(m)) next

      mv <- tryCatch(metric(m), error = function(e) {
        warning("Fallback: could not compute metric for ", deparse1(f), ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      })
      if (is.null(mv)) next

      if (verbose) message("Formula: ", deparse1(f), " Metric: ", mv)
      fallback_formulas[[length(fallback_formulas) + 1L]] <- f
      fallback_metrics[[length(fallback_metrics) + 1L]]   <- mv
      start_results <- rbind(start_results,
                              data.frame(Formula = deparse1(f),
                                         Metric  = I(list(mv))))
    }

    if (length(fallback_metrics) == 0L)
      stop("No single-predictor model could be fit. Cannot proceed.",
           call. = FALSE)

    best_idx      <- which(vapply(fallback_metrics, identical,
                                   logical(1),
                                   do.call(metric_comparison, fallback_metrics)))
    best_idx      <- best_idx[1L]
    start_formula <- fallback_formulas[[best_idx]]
    start_metric  <- fallback_metrics[[best_idx]]
    initial_terms <- all.vars(start_formula[[3]])

    if (verbose) message("Selected fallback starting model: ", deparse1(start_formula),
            " (metric: ", start_metric, ")")
  }

  # ---- Dispatch to search algorithm ----

  # ---- Argument-method compatibility warnings ----
  # Warn when user-supplied arguments are ignored or underutilised by the
  # chosen method.  Each check fires only when the argument differs from its
  # default, so users who rely on defaults see no noise.

  if (!is.function(method)) {

    # ... args: only lasso/lasso_path forward them to glmnet
    dots <- list(...)
    if (length(dots) > 0 && !method %in% c("lasso", "lasso_path")) {
      dot_names <- if (is.null(names(dots))) "(unnamed)" else paste(names(dots), collapse = ", ")
      warning("Extra arguments (", dot_names,
              ") are ignored for method = '", method, "'. ",
              "Only 'lasso' and 'lasso_path' forward ... to glmnet.",
              call. = FALSE)
    }

    # metric with lasso: cv.glmnet drives selection, not the user's metric
    if (identical(method, "lasso") && !identical(metric, AIC)) {
      warning(
        "method = 'lasso' uses cv.glmnet's internal CV error for variable ",
        "selection, not your metric. Your metric is only used to score the ",
        "final refit. For metric-driven selection, consider ",
        "method = 'lasso_path'.",
        call. = FALSE
      )
    }

    # metric_comparison with lasso: glmnet always minimises CV error
    if (identical(method, "lasso") && !identical(metric_comparison, min)) {
      warning(
        "method = 'lasso' does not use metric_comparison; cv.glmnet always ",
        "minimises CV error.",
        call. = FALSE
      )
    }

    # lambda_rule: only used by lasso (lasso_path warns internally too)
    if (lambda_rule != "lambda.min" &&
        !method %in% c("lasso", "lasso_path")) {
      warning(
        "lambda_rule is only used by method = 'lasso'; ignored for ",
        "method = '", method, "'.",
        call. = FALSE
      )
    }

    # max_terms: only used by exhaustive
    if (!is.null(max_terms) && !identical(method, "exhaustive")) {
      warning(
        "max_terms is only used by method = 'exhaustive'; ignored for ",
        "method = '", method, "'.",
        call. = FALSE
      )
    }

    # keep_all_vars with backward: always overridden to TRUE
    if (identical(method, "backward") && !isTRUE(keep_all_vars)) {
      warning(
        "method = 'backward' always starts from all first-order predictors; ",
        "keep_all_vars = FALSE is overridden.",
        call. = FALSE
      )
    }
  }

  candidate_terms <- setdiff(candidate_terms, initial_terms)

  # Backward elimination only removes terms from the starting formula; any
  # polynomial or interaction candidates that were built are unused.
  if (identical(method, "backward") && length(candidate_terms) > 0) {
    warning(
      "method = 'backward' starts from first-order predictors only. ",
      length(candidate_terms), " polynomial/interaction candidate(s) from ",
      "max_degree/max_interact_vars were built but will not be used.",
      call. = FALSE
    )
  }

  common_args <- list(
    candidate_terms   = candidate_terms,
    current_formula   = start_formula,
    current_metric    = start_metric,
    results           = start_results,
    model_func        = model_func,
    metric            = metric,
    metric_comparison = metric_comparison,
    data              = data,
    verbose           = verbose
  )

  result <- if (is.function(method)) {
    # Custom methods may not accept 'verbose'; strip it to avoid errors.
    custom_args <- common_args[names(common_args) != "verbose"]
    do.call(method, custom_args)
  } else if (method == "greedy") {
    do.call(.mine_greedy, common_args)
  } else if (method == "greedy_star") {
    do.call(.mine_greedy_star,
            c(common_args, list(predictor_vars    = predictor_vars,
                                numeric_vars      = numeric_vars,
                                max_degree        = max_degree,
                                max_interact_vars = max_interact_vars)))
  } else if (method %in% c("greedy_alt", "greedy_alt_full",
                            "greedy_alt_fb", "greedy_alt_full_fb")) {
    do.call(.mine_greedy_alt,
            c(common_args, list(predictor_vars    = predictor_vars,
                                numeric_vars      = numeric_vars,
                                max_degree        = max_degree,
                                max_interact_vars = max_interact_vars,
                                full_interactions = grepl("full", method),
                                do_backward       = grepl("_fb$", method),
                                response_str      = response_str)))
  } else if (method == "forward_backward") {
    do.call(.mine_forward_backward,
            c(common_args, list(response_str = response_str,
                                initial_terms = initial_terms)))
  } else if (method == "backward") {
    do.call(.mine_backward,
            c(common_args, list(response_str  = response_str,
                                initial_terms = initial_terms)))
  } else if (method == "lasso") {
    do.call(.mine_lasso,
            c(common_args, list(response_str = response_str,
                                lambda_rule = lambda_rule, ...)))
  } else if (method == "lasso_path") {
    do.call(.mine_lasso_path,
            c(common_args, list(response_str = response_str, ...)))
  } else {
    do.call(.mine_exhaustive,
            c(common_args, list(response_str = response_str,
                                max_terms = max_terms)))
  }

  # ---- Simplify Metric column when possible ----
  # During search, Metric is stored as a list column (I(list(...))) so it can
  # hold arbitrary return types.  When every element is a length-1 numeric
  # scalar -- the overwhelmingly common case -- unlist it to a plain numeric
  # column for convenience.
  m <- result$all_models$Metric
  if (is.list(m) && all(vapply(m, function(x) is.numeric(x) && length(x) == 1L,
                                logical(1)))) {
    result$all_models$Metric <- unlist(m)
  }

  # ---- Enrich and summarise result ----

  # Refit the best formula so the caller gets a ready-to-use model object.
  # Done here rather than inside each algorithm so the return structure is
  # consistent regardless of which search was used.
  result$model <- tryCatch(
    model_func(result$Formula, data = data),
    error = function(e) {
      warning("Could not refit best model: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  result$best_metric <- if (!is.null(result$model)) {
    tryCatch(metric(result$model), error = function(e) NA_real_)
  } else {
    NA_real_
  }

  # Record which algorithm produced this result -- useful when results are
  # collected by compare_methods() or inspected programmatically.
  result$method <- if (is.function(method)) "custom" else method

  if (verbose) {
    message("\n-- Best formula ------------------------------------------")
    message("Formula: ", deparse1(result$Formula))
    message("Metric:  ", result$best_metric)
    message("----------------------------------------------------------\n")
  }

  result
}
