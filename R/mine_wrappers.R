# Method-specific wrappers for mine().
#
# Each wrapper exposes only the arguments relevant to its search algorithm,
# providing clearer autocomplete and documentation than the catch-all mine().
# They all delegate to the internal .mine_impl() workhorse.

#' Greedy forward stepwise selection
#'
#' Adds the single best-improving term each round until no candidate improves
#' the metric.  A thin wrapper around \code{\link{mine}(method = "greedy")}.
#'
#' @inheritParams mine
#' @param variant Greedy variant to use:
#'   \describe{
#'     \item{\code{"greedy"}}{(default) interaction candidates use \code{:}}
#'     \item{\code{"greedy_star"}}{interaction candidates use \code{*}, so main
#'       effects are always included alongside their interaction}
#'     \item{\code{"greedy_alt"}}{phased: first-order, then polynomials on
#'       selected vars, then interactions on selected terms}
#'     \item{\code{"greedy_alt_full"}}{same phases, but phase 3 considers
#'       interactions among all predictors}
#'     \item{\code{"greedy_alt_fb"}}{like \code{greedy_alt} with
#'       forward-backward within each phase}
#'     \item{\code{"greedy_alt_full_fb"}}{like \code{greedy_alt_full} with
#'       forward-backward within each phase}
#'   }
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}} for fields and
#'   methods (\code{print}, \code{summary}, \code{coef}, \code{predict}, ...).
#' @export
#' @seealso \code{\link{mine}} for the general dispatcher.
#'
#' @examples
#' fit <- mine_greedy(mtcars, mpg)
#' print(fit)
#' formula(fit)
#' @importFrom rlang enexpr as_string
mine_greedy <- function(data, response_var, model_func = lm,
                        max_degree = 3, max_interact_vars = 2,
                        metric = AIC, metric_comparison = min,
                        keep_all_vars = FALSE, variant = "greedy",
                        verbose = TRUE) {
  call <- match.call()
  variant <- match.arg(variant, c("greedy", "greedy_star",
                                   "greedy_alt", "greedy_alt_full",
                                   "greedy_alt_fb", "greedy_alt_full_fb"))
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = max_degree,
                       max_interact_vars = max_interact_vars,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = keep_all_vars,
                       method            = variant,
                       verbose           = verbose)
  .wrap_mine(result, call)
}


#' Forward-backward stepwise selection
#'
#' Each round performs a forward step (add best term) then a backward step
#' (drop worst term if it improves the metric).  A thin wrapper around
#' \code{\link{mine}(method = "forward_backward")}.
#'
#' @inheritParams mine
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}}.
#' @export
#' @seealso \code{\link{mine}}
#'
#' @examples
#' fit <- mine_forward_backward(mtcars, mpg)
#' print(fit)
#' formula(fit)
#' @importFrom rlang enexpr as_string
mine_forward_backward <- function(data, response_var, model_func = lm,
                                  max_degree = 3, max_interact_vars = 2,
                                  metric = AIC, metric_comparison = min,
                                  keep_all_vars = FALSE, verbose = TRUE) {
  call <- match.call()
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = max_degree,
                       max_interact_vars = max_interact_vars,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = keep_all_vars,
                       method            = "forward_backward",
                       verbose           = verbose)
  .wrap_mine(result, call)
}


#' Backward elimination
#'
#' Starts from all first-order predictors and removes terms one at a time.
#' Polynomial and interaction terms are not considered.  A thin wrapper around
#' \code{\link{mine}(method = "backward")}.
#'
#' @param data A data frame containing the response and predictor variables.
#' @param response_var The name of the response variable (unquoted).
#' @param model_func A model function accepting \code{formula} and \code{data}.
#'   Defaults to \code{lm}.
#' @param metric A function to compute the model selection metric.
#'   Defaults to \code{AIC}.
#' @param metric_comparison A function that compares metric values and returns
#'   the preferable one.  Defaults to \code{min}.
#' @param verbose If \code{TRUE} (the default), print progress messages.
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}}.
#' @export
#' @seealso \code{\link{mine}}
#'
#' @examples
#' fit <- mine_backward(mtcars, mpg)
#' print(fit)
#' formula(fit)
#' @importFrom rlang enexpr as_string
mine_backward <- function(data, response_var, model_func = lm,
                          metric = AIC, metric_comparison = min,
                          verbose = TRUE) {
  call <- match.call()
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = 1,
                       max_interact_vars = 1,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = FALSE,
                       method            = "backward",
                       verbose           = verbose)
  .wrap_mine(result, call)
}


#' Best-subset (exhaustive) selection
#'
#' Evaluates every possible subset of candidate terms up to \code{max_terms}
#' terms and returns the one that optimizes the metric.  A thin wrapper around
#' \code{\link{mine}(method = "exhaustive")}.
#'
#' @inheritParams mine
#' @param max_terms Maximum number of terms to include in a subset.
#'   Defaults to 5.
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}}.
#' @export
#' @seealso \code{\link{mine}}
#'
#' @examples
#' \donttest{
#' fit <- mine_exhaustive(mtcars, mpg, max_terms = 3)
#' print(fit)
#' formula(fit)
#' }
#' @importFrom rlang enexpr as_string
mine_exhaustive <- function(data, response_var, model_func = lm,
                            max_degree = 3, max_interact_vars = 2,
                            metric = AIC, metric_comparison = min,
                            keep_all_vars = FALSE, max_terms = 5,
                            verbose = TRUE) {
  call <- match.call()
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = max_degree,
                       max_interact_vars = max_interact_vars,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = keep_all_vars,
                       method            = "exhaustive",
                       max_terms         = max_terms,
                       verbose           = verbose)
  .wrap_mine(result, call)
}


#' L1-regularised (lasso) selection via cross-validation
#'
#' Uses \code{\link[glmnet]{cv.glmnet}} to fit an L1-penalised model over the
#' full candidate pool, then extracts the selected terms at the chosen lambda.
#' Requires the \pkg{glmnet} package.  A thin wrapper around
#' \code{\link{mine}(method = "lasso")}.
#'
#' @inheritParams mine
#' @param lambda_rule Which lambda to use for the selected formula:
#'   \code{"lambda.min"} (default, best CV performance) or \code{"lambda.1se"}
#'   (sparser, within 1 SE of the minimum).  Both appear in \code{all_models}.
#' @param ... Additional arguments forwarded to
#'   \code{\link[glmnet]{cv.glmnet}}, e.g.
#'   \code{alpha} (0 = ridge, 0.5 = elastic net, 1 = lasso),
#'   \code{nfolds}, \code{family}.
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}}.
#' @export
#' @seealso \code{\link{mine_lasso_path}} for the full regularization path,
#'   \code{\link{mine}} for the general dispatcher.
#'
#' @examples
#' \donttest{
#' fit <- mine_lasso(mtcars, mpg)
#' print(fit)
#' formula(fit)
#'
#' # Multinomial response
#' fit <- mine_lasso(iris, Species,
#'                   model_func = nnet::multinom,
#'                   family = "multinomial", trace = FALSE)
#' }
#' @importFrom rlang enexpr as_string
mine_lasso <- function(data, response_var, model_func = lm,
                       max_degree = 3, max_interact_vars = 2,
                       metric = AIC, metric_comparison = min,
                       keep_all_vars = FALSE,
                       lambda_rule = "lambda.min",
                       verbose = TRUE, ...) {
  call <- match.call()
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = max_degree,
                       max_interact_vars = max_interact_vars,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = keep_all_vars,
                       method            = "lasso",
                       lambda_rule       = lambda_rule,
                       verbose           = verbose,
                       ...)
  .wrap_mine(result, call)
}


#' L1-regularised selection via the full regularization path
#'
#' Walks the full \code{\link[glmnet]{glmnet}} regularization path and records
#' every model where the selected variable set changes.  Produces a richer
#' \code{all_models} table than \code{\link{mine_lasso}}.  Requires the
#' \pkg{glmnet} package.  A thin wrapper around
#' \code{\link{mine}(method = "lasso_path")}.
#'
#' @inheritParams mine
#' @param ... Additional arguments forwarded to \code{\link[glmnet]{glmnet}},
#'   e.g. \code{alpha}, \code{family}.
#'
#' @returns A \code{"mine"} S3 object; see \code{\link{mine}}. The
#'   \code{$trace} (a.k.a. \code{$all_models}) data frame is richer than
#'   \code{\link{mine_lasso}}'s -- one row per change point on the
#'   regularization path.
#' @export
#' @seealso \code{\link{mine_lasso}} for cross-validated selection,
#'   \code{\link{mine}} for the general dispatcher.
#'
#' @examples
#' \donttest{
#' fit <- mine_lasso_path(mtcars, mpg)
#' print(fit)
#' head(fit$trace)
#' }
#' @importFrom rlang enexpr as_string
mine_lasso_path <- function(data, response_var, model_func = lm,
                            max_degree = 3, max_interact_vars = 2,
                            metric = AIC, metric_comparison = min,
                            keep_all_vars = FALSE,
                            verbose = TRUE, ...) {
  call <- match.call()
  response_str <- as_string(enexpr(response_var))
  result <- .mine_impl(data, response_str,
                       model_func        = model_func,
                       max_degree        = max_degree,
                       max_interact_vars = max_interact_vars,
                       metric            = metric,
                       metric_comparison = metric_comparison,
                       keep_all_vars     = keep_all_vars,
                       method            = "lasso_path",
                       verbose           = verbose,
                       ...)
  .wrap_mine(result, call)
}
