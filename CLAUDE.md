# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

`modelminer` is an R package that performs automated model building via greedy forward selection. Starting from an intercept-only (or all-first-order) model, it iteratively adds the single term—from polynomial expansions and interaction terms—that most improves a user-supplied metric (default: AIC).

## Development Commands

All commands run from R or the R console. Load the package interactively during development with:

```r
devtools::load_all()    # Load all R/ functions without installing
devtools::document()    # Regenerate NAMESPACE and man/ from roxygen2 comments
devtools::test()        # Run all tests
devtools::check()       # Full R CMD CHECK
```

Run a single test file:
```r
testthat::test_file("tests/testthat/test-mine.R")
```

## Architecture

### Core Algorithm (`R/mine.R`)

`mine()` implements greedy forward stepwise selection:
1. Builds a candidate term pool: first-order predictors + polynomial terms (`I(var^k)` up to `max_degree`) + interaction terms (all combinations up to `max_interact_vars` variables, using `*` so main effects are included).
2. Starts from intercept-only or all-first-order formula depending on `keep_all_vars`.
3. Each iteration evaluates every candidate term added to the current formula, picks the best by `metric_comparison` (default `min` for AIC), updates the formula, removes used terms, and repeats until no improvement.
4. Returns `list(Formula = best_formula, all_models = results_dataframe)`.

Key design detail: interaction terms use `*` (not `:`) so adding `a*b` automatically includes both main effects and the interaction. The `used_terms` pruning uses `attr(terms(formula), "term.labels")` to strip already-included terms from candidates.

### Formula Wrapper (`R/formula_wrapper.R` + `R/utils.R`)

`formula_wrap(model_func, x_name, y_name)` adapts matrix-based model functions (e.g., `glmnet`) to accept a `formula + data` interface compatible with `mine()`. It returns a closure that:
1. Calls `to_xy()` (in `utils.R`) to extract the response vector and predictor matrix from a data frame using the formula.
2. Passes `x` and `y` as named arguments to the original function alongside any `...` extras.

### Dependencies
- `rlang` (`enexpr`, `as_string`) — used to capture the unquoted `response_var` argument in `mine()` and `to_xy()`.
- `testthat` (>= 3.0.0) — test suite only.
- Documentation generated with `roxygen2` 7.3.3.
