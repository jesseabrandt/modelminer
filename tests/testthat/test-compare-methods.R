test_that("compare_methods returns expected structure", {
  cmp <- compare_methods(
    mtcars, mpg,
    configs = list(
      greedy = list(method = "greedy"),
      fwd_bwd = list(method = "forward_backward")
    ),
    max_degree = 1, max_interact_vars = 1
  )
  expect_type(cmp, "list")
  expect_named(cmp, c("summary", "details"))
  expect_s3_class(cmp$summary, "data.frame")
  expect_equal(nrow(cmp$summary), 2)
  expect_named(cmp$summary, c("Config", "Formula", "BestMetric", "ModelsEvaluated"))
  expect_named(cmp$details, c("greedy", "fwd_bwd"))
})

test_that("compare_methods summary Config column matches names(configs)", {
  cmp <- compare_methods(
    mtcars, mpg,
    configs = list(a = list(method = "greedy"), b = list(method = "greedy")),
    max_degree = 1, max_interact_vars = 1
  )
  expect_equal(cmp$summary$Config, c("a", "b"))
})

test_that("compare_methods accepts shared ... args overridden per config", {
  cmp <- compare_methods(
    mtcars, mpg,
    configs = list(
      deg1 = list(max_degree = 1),
      deg2 = list(max_degree = 2)
    ),
    method = "greedy", max_interact_vars = 1
  )
  # deg2 explores more terms so should evaluate more models
  expect_gt(cmp$summary$ModelsEvaluated[cmp$summary$Config == "deg2"],
            cmp$summary$ModelsEvaluated[cmp$summary$Config == "deg1"])
})

test_that("compare_methods works with a custom draft method function", {
  # Minimal draft: always returns the intercept-only model
  intercept_only <- function(candidate_terms, current_formula, current_metric,
                             results, model_func, metric, metric_comparison, data) {
    list(Formula = current_formula, all_models = results)
  }

  cmp <- compare_methods(
    mtcars, mpg,
    configs = list(
      greedy = list(method = "greedy"),
      draft  = list(method = intercept_only)
    ),
    max_degree = 1, max_interact_vars = 1
  )
  expect_equal(nrow(cmp$summary), 2)
  # Draft returns intercept only; greedy should find a better model
  draft_formula  <- cmp$summary$Formula[cmp$summary$Config == "draft"]
  greedy_formula <- cmp$summary$Formula[cmp$summary$Config == "greedy"]
  expect_equal(draft_formula, "mpg ~ 1")
  expect_false(identical(draft_formula, greedy_formula))
})

test_that("compare_methods stops on unnamed configs", {
  expect_error(
    compare_methods(mtcars, mpg, configs = list(list(method = "greedy"))),
    "named"
  )
})

test_that("mine() accepts a custom method function directly", {
  passthrough <- function(candidate_terms, current_formula, current_metric,
                          results, model_func, metric, metric_comparison, data) {
    list(Formula = current_formula, all_models = results)
  }
  result <- mine(mtcars, mpg, method = passthrough,
                 max_degree = 1, max_interact_vars = 1)
  expect_equal(deparse1(result$Formula), "mpg ~ 1")
})
