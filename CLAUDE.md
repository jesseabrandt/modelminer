# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## North Star Framework

This project uses a north star framework. Read `north_star.md` and `framework.md` at session start.

## What This Package Does

`modelminer` is an R package that performs automated model building via configurable stepwise selection. Starting from an intercept-only (or all-first-order) model, it iteratively adds (and optionally removes) terms—from polynomial expansions and interaction terms—that most improve a user-supplied metric (default: AIC).

## Development Commands

`devtools` cannot be installed in this container (its dependencies `ragg` and
`gert` need system libraries `libharfbuzz-dev`, `libfribidi-dev`, `libgit2-dev`
that are not available). Use the underlying packages directly instead:

```r
pkgload::load_all()          # Load all R/ functions without installing
roxygen2::roxygenise()       # Regenerate NAMESPACE and man/ from roxygen2 comments
testthat::test_dir("tests/testthat")   # Run all tests
rcmdcheck::rcmdcheck()       # Full R CMD CHECK
```

Run a single test file:
```r
testthat::test_file("tests/testthat/test-mine.R")
```

From the shell (useful for CI or the /r-test skill):
```bash
Rscript -e "pkgload::load_all(quiet=TRUE); testthat::test_dir('tests/testthat', reporter='check')"
```

## Architecture

### Dispatcher (`R/mine.R`)

`mine()` is the public entry point. It captures `response_var` via NSE then delegates to `.mine_impl(data, response_str, ...)`, which:
1. Builds a candidate term pool: first-order predictors + polynomial terms (`I(var^k)` up to `max_degree`) + interaction terms (all combinations up to `max_interact_vars` variables, using `:` — interaction only, no implicit main effects).
2. Constructs the starting formula (intercept-only or all first-order terms, per `keep_all_vars`).
3. Dispatches to the chosen search algorithm based on `method`.
4. Returns `list(Formula = best_formula, all_models = results_dataframe)`.

Interaction terms use `:` (not `*`) so each step adds exactly one term. Main effects are included as separate candidates and added by the search algorithm if they improve the metric.

`.mine_impl()` is the internal workhorse (takes `response_str` as a plain string) so it can be called programmatically by `compare_methods()` via `do.call()`.

### Search Algorithms

- **`R/mine_greedy.R`** — `.mine_greedy()`: greedy forward selection; adds the single best term each round until no improvement.
- **`R/mine_forward_backward.R`** — `.mine_forward_backward()`: forward step then backward step each round; tracks `added_terms` in `:` notation so formula rebuilds cleanly for removal.
- **`R/mine_exhaustive.R`** — `.mine_exhaustive()`: stub (not yet implemented).

`method` may also be a custom function with signature `function(candidate_terms, current_formula, current_metric, results, model_func, metric, metric_comparison, data)` for draft/experimental algorithms.

### Comparison (`R/compare_methods.R`)

`compare_methods(data, response_var, configs, ...)` runs multiple configurations side-by-side. `configs` is a named list of argument overrides; `...` provides shared defaults. Returns `list(summary = data.frame, details = named_list)`.

### Formula Wrapper (`R/formula_wrapper.R` + `R/utils.R`)

`formula_wrap(model_func, x_name, y_name)` adapts matrix-based model functions (e.g., `glmnet`) to accept a `formula + data` interface compatible with `mine()`. It returns a closure that:
1. Calls `to_xy()` (in `utils.R`) to extract the response vector and predictor matrix from a data frame using the formula.
2. Passes `x` and `y` as named arguments to the original function alongside any `...` extras.

### Dependencies
- `rlang` (`enexpr`, `as_string`) — used to capture the unquoted `response_var` argument in `mine()` and `to_xy()`.
- `testthat` (>= 3.0.0) — test suite only.
- Documentation generated with `roxygen2` 7.3.3.
