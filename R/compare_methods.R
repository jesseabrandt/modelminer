#' Run and compare multiple mine() configurations side-by-side
#'
#' A convenience wrapper around \code{\link{mine}} for experimenting with
#' different search algorithms, candidate-pool settings, or custom ("draft")
#' search functions without having to manually re-run and collect results.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response_var The name of the response variable (unquoted).
#' @param configs A named list of argument lists. Each element is passed to
#'   \code{mine()} as overrides on top of the shared \code{...} defaults.
#'   \code{method} may be a string or a custom search function in any config.
#' @param ... Default \code{mine()} arguments shared by all configs. Values
#'   in individual \code{configs} entries take precedence.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{\code{summary}}{A data frame with one row per config: Config,
#'       Formula, BestMetric (numeric; NA if non-numeric), and
#'       ModelsEvaluated.}
#'     \item{\code{details}}{A named list of the full \code{mine()} results
#'       (each has \code{Formula} and \code{all_models}).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare greedy vs forward_backward with different polynomial degrees
#' cmp <- compare_methods(
#'   mtcars, mpg,
#'   configs = list(
#'     greedy_d1 = list(method = "greedy",          max_degree = 1),
#'     greedy_d3 = list(method = "greedy",          max_degree = 3),
#'     fwd_bwd   = list(method = "forward_backward", max_degree = 1)
#'   ),
#'   max_interact_vars = 1
#' )
#' cmp$summary
#'
#' # Use a custom draft search function
#' my_draft <- function(candidate_terms, current_formula, current_metric,
#'                      results, model_func, metric, metric_comparison, data) {
#'   # ... experimental logic ...
#' }
#' cmp2 <- compare_methods(
#'   mtcars, mpg,
#'   configs = list(
#'     greedy = list(method = "greedy"),
#'     draft  = list(method = my_draft)
#'   )
#' )
#' }
#' @importFrom rlang enexpr as_string
#' @importFrom utils modifyList
compare_methods <- function(data, response_var, configs, ...) {
  if (!is.list(configs) || is.null(names(configs)) || any(names(configs) == "")) {
    stop("'configs' must be a named list.", call. = FALSE)
  }

  response_str <- as_string(enexpr(response_var))
  defaults     <- list(...)

  details <- lapply(names(configs), function(nm) {
    cfg  <- modifyList(defaults, configs[[nm]])
    args <- c(list(data = data, response_str = response_str), cfg)
    tryCatch(
      do.call(.mine_impl, args),
      error = function(e) {
        warning("Config '", nm, "' failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  })
  names(details) <- names(configs)

  # Build summary table — one row per config
  rows <- lapply(names(details), function(nm) {
    r <- details[[nm]]
    if (is.null(r)) {
      return(data.frame(Config = nm, Formula = NA_character_,
                        BestMetric = NA_real_, ModelsEvaluated = NA_integer_,
                        stringsAsFactors = FALSE))
    }
    data.frame(
      Config          = nm,
      Formula         = deparse1(r$Formula),
      BestMetric      = if (is.numeric(r$best_metric)) r$best_metric else NA_real_,
      ModelsEvaluated = nrow(r$all_models),
      stringsAsFactors = FALSE
    )
  })

  list(
    summary = do.call(rbind, rows),
    details = details
  )
}
