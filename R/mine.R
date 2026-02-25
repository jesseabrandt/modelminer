#' Automated model selection with feature engineering
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
#'     \item \code{"greedy"} (default) — forward selection, interaction candidates use \code{:}
#'     \item \code{"greedy_star"} — same but interaction candidates use \code{*}, so main effects are always included with their interaction
#'     \item \code{"greedy_alt"} — phased: first-order → polynomial (selected vars) → interactions (selected terms)
#'     \item \code{"greedy_alt_full"} — same phases, but phase 3 considers interactions among all predictors
#'     \item \code{"greedy_alt_fb"} — \code{greedy_alt} with forward-backward within each phase
#'     \item \code{"greedy_alt_full_fb"} — \code{greedy_alt_full} with forward-backward within each phase
#'     \item \code{"forward_backward"} — forward-backward over a single pre-built pool
#'     \item \code{"exhaustive"} — best-subset selection up to \code{max_terms} terms
#'   }
#'   May also be a custom search function — see Details.
#' @param max_terms Maximum number of terms to include in a subset for the
#'   exhaustive method. Defaults to 5 if \code{NULL}. Ignored by other methods.
#'
#' @details
#' When \code{method} is a function it must have the signature:
#' \preformatted{
#'   function(candidate_terms, current_formula, current_metric,
#'            results, model_func, metric, metric_comparison, data)
#' }
#' and return \code{list(Formula, all_models)} matching the standard contract.
#' This lets you pass experimental ("draft") search implementations for comparison
#' via \code{\link{compare_methods}}.
#'
#' @returns A list with four elements:
#'   \describe{
#'     \item{\code{Formula}}{The best formula found, as a \code{formula} object.}
#'     \item{\code{all_models}}{A data frame of every formula evaluated and its
#'       metric value (list column, to support non-numeric metrics).}
#'     \item{\code{model}}{The fitted model object for the best formula, ready
#'       for \code{summary()}, \code{predict()}, \code{coef()}, etc.}
#'     \item{\code{best_metric}}{The metric value for the best formula as a
#'       plain numeric scalar.}
#'     \item{\code{method}}{The search algorithm used (\code{"greedy"},
#'       \code{"forward_backward"}, \code{"exhaustive"}, or \code{"custom"}).}
#'   }
#' @export
#'
#' @examples
#' result <- mine(mtcars, mpg)
#' result$Formula
#' @importFrom rlang enexpr as_string
#' @importFrom stats AIC as.formula lm formula hatvalues model.frame model.response predict residuals
#' @importFrom utils combn
mine <- function(data, response_var, model_func = lm,
                 max_degree = 3, max_interact_vars = 2, metric = AIC,
                 metric_comparison = min, keep_all_vars = FALSE,
                 method = "greedy", max_terms = NULL) {
  response_str <- as_string(enexpr(response_var))
  .mine_impl(data, response_str,
             model_func        = model_func,
             max_degree        = max_degree,
             max_interact_vars = max_interact_vars,
             metric            = metric,
             metric_comparison = metric_comparison,
             keep_all_vars     = keep_all_vars,
             method            = method,
             max_terms         = max_terms)
}

# Internal workhorse. Accepts response_var as a plain string rather than an
# unquoted symbol so it can be called via do.call() from compare_methods()
# without fighting R's NSE rules. All other arguments are identical to mine().
.mine_impl <- function(data, response_str, model_func = lm,
                       max_degree = 3, max_interact_vars = 2, metric = AIC,
                       metric_comparison = min, keep_all_vars = FALSE,
                       method = "greedy", max_terms = NULL) {

  if (!is.function(method)) {
    method <- match.arg(method, c("greedy", "greedy_star",
                                   "greedy_alt", "greedy_alt_full",
                                   "greedy_alt_fb", "greedy_alt_full_fb",
                                   "forward_backward", "exhaustive"))
  }

  # ---- Shared setup: candidate term pool ----

  predictor_vars  <- setdiff(names(data), response_str)
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
  # represents only the interaction itself — no implicit main effects.
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

  # A failed starting model is fatal: there is no baseline metric to improve
  # on, so the search cannot proceed. Per-term failures later are non-fatal
  # (the term is skipped with a warning) because the search can still continue
  # with the remaining candidates.
  start_model <- tryCatch(
    model_func(start_formula, data = data),
    error = function(e) {
      stop("Failed to fit starting model: ", conditionMessage(e), call. = FALSE)
    }
  )

  start_metric <- tryCatch(
    metric(start_model),
    error = function(e) {
      stop("Failed to compute metric for starting model: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  cat("Formula:", deparse(start_formula), "Metric:", start_metric, "\n")

  start_results <- data.frame(
    Formula = deparse1(start_formula),
    Metric  = I(list(start_metric))
  )

  # ---- Dispatch to search algorithm ----

  common_args <- list(
    candidate_terms   = candidate_terms,
    current_formula   = start_formula,
    current_metric    = start_metric,
    results           = start_results,
    model_func        = model_func,
    metric            = metric,
    metric_comparison = metric_comparison,
    data              = data
  )

  result <- if (is.function(method)) {
    do.call(method, common_args)
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
  } else {
    do.call(.mine_exhaustive,
            c(common_args, list(response_str = response_str,
                                max_terms = max_terms)))
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

  # Record which algorithm produced this result — useful when results are
  # collected by compare_methods() or inspected programmatically.
  result$method <- if (is.function(method)) "custom" else method

  cat("\n── Best formula ──────────────────────────\n")
  cat("Formula:", deparse1(result$Formula), "\n")
  cat("Metric: ", result$best_metric, "\n")
  cat("─────────────────────────────────────────\n\n")

  result
}
