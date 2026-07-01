# Direct tests for the NSE call-rewriting helpers (.normalize_user_call /
# .build_mine) and the "mine" class constructor/validator (new_mine /
# validate_mine). These pin behaviour that is otherwise only exercised
# indirectly through print()/summary().

# --- .normalize_user_call: positional-vs-named arg leakage ------------------

test_that(".normalize_user_call (formula style) relabels x -> formula", {
  cl  <- quote(mine.formula(x = mpg ~ wt, data = mtcars, method = "greedy"))
  out <- modelminer:::.normalize_user_call(cl, formula_style = TRUE)

  expect_identical(out[[1L]], quote(mine))               # dispatch suffix dropped
  expect_identical(names(out), c("", "formula", "data", "method"))
  expect_equal(deparse1(out),
               "mine(formula = mpg ~ wt, data = mtcars, method = \"greedy\")")
})

test_that(".normalize_user_call (data style) drops x and response_var names", {
  cl  <- quote(mine.data.frame(x = mtcars, response_var = mpg, method = "greedy"))
  out <- modelminer:::.normalize_user_call(cl, formula_style = FALSE)

  expect_identical(out[[1L]], quote(mine))
  # x and response_var become positional; the genuinely-named arg is preserved
  # -- the auto-added formal names must not leak into the printed call.
  expect_identical(names(out), c("", "", "", "method"))
  expect_equal(deparse1(out), "mine(mtcars, mpg, method = \"greedy\")")
})

test_that(".normalize_user_call drops a named response_var even when x is positional", {
  cl  <- quote(mine.data.frame(mtcars, response_var = mpg))
  out <- modelminer:::.normalize_user_call(cl, formula_style = FALSE)
  expect_equal(deparse1(out), "mine(mtcars, mpg)")
})

test_that(".normalize_user_call leaves already-positional arguments untouched", {
  cl  <- quote(mine.formula(mpg ~ wt, mtcars))            # nothing named
  out <- modelminer:::.normalize_user_call(cl, formula_style = TRUE)
  expect_identical(out[[1L]], quote(mine))
  expect_equal(deparse1(out), "mine(mpg ~ wt, mtcars)")
})

# --- call display through the three user-facing entry points ----------------

test_that("the displayed $call is normalized for every call form (no leakage)", {
  forms <- list(
    formula  = mine(mpg ~ wt + cyl, data = mtcars, verbose = FALSE,
                    max_degree = 1, max_interact_vars = 1),
    data     = mine(mtcars, mpg, verbose = FALSE,
                    max_degree = 1, max_interact_vars = 1),
    piped    = mtcars |> mine(mpg ~ wt + cyl, verbose = FALSE,
                              max_degree = 1, max_interact_vars = 1)
  )
  for (fit in forms) {
    cl <- deparse1(fit$call)
    expect_match(cl, "^mine\\(")                          # generic, not a method
    expect_false(grepl("mine\\.formula|mine\\.data\\.frame", cl))
    expect_false(grepl("\\bx =", cl))                     # no leaked `x =`
    expect_false(grepl("response_var =", cl))             # no leaked `response_var =`
  }
})

test_that("piped form preserves the data symbol in the refitted model's call", {
  fit <- mtcars |> mine(mpg ~ wt + cyl, verbose = FALSE,
                        max_degree = 1, max_interact_vars = 1)
  # .build_mine rewrites the underlying model's stored call to reference the
  # user's data expression (`mtcars`), not the internal `data` placeholder.
  expect_match(deparse1(extract_model(fit)$call), "data = mtcars")
})

# --- piped-formula environment ----------------------------------------------

test_that("a piped formula is captured from the caller's environment", {
  # The data frame lives only in this helper's frame; the pipe must capture
  # the formula in the caller's environment for the search to resolve it.
  run <- function() {
    d <- mtcars[, c("mpg", "wt", "cyl")]
    d |> mine(mpg ~ wt + cyl, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  }
  fit <- run()
  expect_s3_class(fit, "mine")
  expect_s3_class(fit$model, "lm")
  expect_true(all(all.vars(fit$formula) %in% c("mpg", "wt", "cyl")))

  # ... and the outcome matches the non-piped formula form exactly.
  direct <- mine(mpg ~ wt + cyl, data = mtcars[, c("mpg", "wt", "cyl")],
                 verbose = FALSE, max_degree = 1, max_interact_vars = 1)
  expect_equal(deparse1(fit$formula), deparse1(direct$formula))
})

# --- new_mine() / validate_mine() -------------------------------------------

test_that("new_mine() derives the deprecated aliases from the canonical fields", {
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  obj <- modelminer:::new_mine(
    model = fit$model, formula = fit$formula, trace = fit$trace,
    best_metric = fit$best_metric, method = fit$method, call = fit$call
  )
  expect_s3_class(obj, "mine")
  # .subset2() reads the aliases without tripping the deprecation warning.
  expect_identical(.subset2(obj, "Formula"),    .subset2(obj, "formula"))
  expect_identical(.subset2(obj, "all_models"), .subset2(obj, "trace"))
})

test_that("validate_mine() accepts a well-formed object and returns it invisibly", {
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  expect_identical(modelminer:::validate_mine(fit), fit)
})

test_that("validate_mine() rejects structural violations", {
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)

  expect_error(modelminer:::validate_mine(unclass(fit)), "not a")

  bad_formula <- fit; bad_formula$formula <- "mpg ~ wt"          # not a formula
  expect_error(modelminer:::validate_mine(bad_formula), "formula")

  bad_trace <- fit; bad_trace$trace <- as.list(fit$trace)         # not a data frame
  expect_error(modelminer:::validate_mine(bad_trace), "trace")

  bad_method <- fit; bad_method$method <- c("a", "b")             # not length-1
  expect_error(modelminer:::validate_mine(bad_method), "method")

  drift <- fit; drift$Formula <- mpg ~ 1                          # alias drift
  expect_error(modelminer:::validate_mine(drift), "Formula")
})

test_that("validate_mine() tolerates a non-numeric best_metric (any-metric invariant)", {
  # Invariant #1: a metric function may return an arbitrary object, so the
  # validator must not insist best_metric be numeric.
  fit <- mine(mtcars, mpg, verbose = FALSE,
              max_degree = 1, max_interact_vars = 1)
  obj <- fit; obj$best_metric <- list(custom = "object")
  expect_identical(modelminer:::validate_mine(obj), obj)
})
