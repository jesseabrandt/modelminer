# modelminer TODO

## License review

`DESCRIPTION` declares `MIT + file LICENSE` and `LICENSE` has the CRAN-required year/copyright stub (Jesse Brandt, Zakaria Zerhouni), but there is no `LICENSE.md` with the full MIT text — so the repo looks unlicensed to casual viewers on Codeberg/GitHub. Before CRAN:

1. Confirm MIT is the right choice (and that Zakaria agrees as co-copyright holder).
2. Add `LICENSE.md` with full MIT text (`usethis::use_mit_license("Jesse Brandt")` regenerates both files; may need manual edit to keep two copyright holders).

## Standards for modeling packages

Find and review external standards for what a well-behaved R modeling package should look like, to guide the "make modelminer a proper modeling package" effort (formula wrapper, `predict()`/`summary()`/`coef()` methods, etc.).

Starting point: <https://tidymodels.github.io/model-implementation-principles/model-predictions.html>. Read through and note which principles apply here — or find a machine-readable version to cite. Look for additional standards beyond this one page.

## Feature: Formula-based wrapper (standard model contract)

Add a wrapper around `mine()` that meets R's standard modelling contract:

```r
fit <- miner(mpg ~ ., data = mtcars)   # name tentative
predict(fit, newdata = ...)
summary(fit)
print(fit)
```

Signature takes `formula + data` (no NSE), passes through to `mine()`, and returns an S3 object carrying the fitted model, selected formula, search trace, and method. `predict()` / `summary()` / `print()` / `coef()` methods delegate to the underlying `$model`.

Rationale: `mine()`'s NSE interface is friendly for interactive use but doesn't compose well with pipelines that already build formulas programmatically. A formula-first wrapper is the standard R contract (`lm`, `glm`, `rpart`, `ranger`, ...) and keeps `mine()`'s signature stable for CRAN.

Name is open — `miner()` is the current suggestion.

## Feature: Multiple Imputation Support

### Background

When using `mine()` with MICE-imputed data (m>1), the current workflow fits on a single completed dataset. Interactions selected on one imputation may not generalize — e.g., in the GLM project, 6 of 7 interactions found by `mine()` on m=1 data turned out non-significant (p>0.1 in all outcomes) after pooling across 10 imputations via Rubin's rules.

### Tier 1: Mean-AIC pooled wrapper (works today, no package changes)

```r
make_pooled_func <- function(datasets) {
  function(formula, data) {
    fits <- lapply(datasets, function(d) {
      multinom(formula, data = d, maxit = 1000, trace = FALSE)
    })
    out <- fits[[1]]
    out$AIC <- mean(sapply(fits, AIC))
    out
  }
}

mined <- mine(data = datasets[[1]], response_var = Y,
              model_func = make_pooled_func(datasets),
              method = "greedy", max_degree = 1, max_interact_vars = 1)
```

Selects interactions that consistently reduce AIC across imputations. ~10x slower.

### Tier 2: D1-based selection via existing functional interface (no package changes)

The metric doesn't have to be scalar. `metric_comparison` just needs to return one of its inputs, and `.find_best_index` uses `identical()` for non-numeric types. This means we can pass mira objects as metrics and use `mice::D1()` for the selection gate.

**How it works:** `model_func` returns a mira object (list of m fits), `metric_func = identity`, and `metric_comparison` dispatches on argument count:

```r
make_pooled_mira_func <- function(datasets) {
  function(formula, data) {
    fits <- lapply(datasets, function(d) {
      multinom(formula, data = d, maxit = 1000, trace = FALSE)
    })
    as.mira(fits)
  }
}

d1_metric_comparison <- function(...) {
  miras <- list(...)
  if (length(miras) == 2) {
    # Pairwise: current vs candidate — these ARE nested (forward selection)
    # D1 is the proper MI Wald test for nested model comparison
    test <- tryCatch(mice::D1(miras[[2]], miras[[1]]), error = function(e) NULL)
    if (!is.null(test) && test$result[1, "P(>F)"] < 0.05) return(miras[[2]])
    return(miras[[1]])
  }
  # Multiple non-nested candidates: use mean AIC to rank
  # (D1 only works for nested models; candidates adding different terms aren't nested)
  aics <- sapply(miras, function(m) mean(sapply(m$analyses, AIC)))
  miras[[which.min(aics)]]
}

mined <- mine(data = datasets[[1]], response_var = Y,
              model_func = make_pooled_mira_func(datasets),
              metric_func = identity,
              metric_comparison = d1_metric_comparison,
              method = "greedy", max_degree = 1, max_interact_vars = 1)
```

**Why this works:**
- `.metric_improved(old, new)` calls `metric_comparison` with exactly 2 args → D1 test (nested)
- `.find_best_index(candidates)` calls with N args → mean AIC ranking (non-nested)
- D1 is the gate: a term only enters the model if it passes the pooled Wald test
- Mean AIC only decides which candidate to try, not whether to accept it

**Rough edges:**
- Memory: each "metric" is m model fits. 30 candidates × 10 fits = 300 fits in memory per round
- `.find_best_index` uses `identical()` on mira objects to find the winning index — works but slow on large objects
- Dispatching on argument count is implicit; would be cleaner with an explicit mode

### Tier 3: First-class `datasets` parameter (package change, simplest)

Add a `datasets` parameter to `mine()`. When provided, `mine()` automatically:
1. Wraps `model_func` to fit on all datasets
2. Uses mean AIC for ranking, D1 for acceptance
3. Returns pooled results

This is the cleanest UX and hides the complexity. Implementation: wrap the greedy loop to carry mira objects internally.

### Tier 4: `comparison_mode = "nested_test"` (package change, most principled)

Add a dedicated code path in the greedy loop that uses a user-supplied `nested_test_func(fit_full, fit_reduced) -> logical` instead of scalar metric comparison. Most flexible but largest change to the loop.
