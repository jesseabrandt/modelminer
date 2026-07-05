# Direct unit tests for the candidate-pool generation helpers in utils.R.
# These are the core of what mine() searches over; previously they were only
# exercised indirectly through full mine() runs. Testing them directly pins the
# exact shape of the generated term pool (order, polynomial/interaction rules).

# .build_candidate_pool ------------------------------------------------------

test_that(".build_candidate_pool with no expansion returns first-order terms only", {
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = c("wt", "hp"), numeric_vars = c("wt", "hp"),
    max_degree = 1, max_interact_vars = 1
  )
  expect_identical(pool, c("wt", "hp"))
})

test_that(".build_candidate_pool adds I(var^k) for numeric predictors up to max_degree", {
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = c("wt", "hp"), numeric_vars = c("wt", "hp"),
    max_degree = 3, max_interact_vars = 1
  )
  # First-order terms come first, then polynomials in var-then-degree order.
  expect_identical(
    pool,
    c("wt", "hp", "I(wt^2)", "I(wt^3)", "I(hp^2)", "I(hp^3)")
  )
})

test_that(".build_candidate_pool generates polynomials only for numeric_vars", {
  # A predictor absent from numeric_vars (e.g. a factor) gets no I(var^k) terms.
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = c("wt", "gear"), numeric_vars = "wt",
    max_degree = 2, max_interact_vars = 1
  )
  expect_true("I(wt^2)" %in% pool)
  expect_false("I(gear^2)" %in% pool)
})

test_that(".build_candidate_pool builds : interactions across combination sizes", {
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = c("a", "b", "c"), numeric_vars = character(0),
    max_degree = 1, max_interact_vars = 3
  )
  # 2-way then 3-way combinations, using ':' (interaction only, no main effects).
  expect_identical(
    pool,
    c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")
  )
})

test_that(".build_candidate_pool caps interaction order at the predictor count", {
  # max_interact_vars exceeds the number of predictors: only the 2-way term
  # (all that is possible with two predictors) is produced, no error.
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = c("a", "b"), numeric_vars = character(0),
    max_degree = 1, max_interact_vars = 5
  )
  expect_identical(pool, c("a", "b", "a:b"))
})

test_that(".build_candidate_pool with a single predictor yields no interactions", {
  pool <- modelminer:::.build_candidate_pool(
    predictor_vars = "wt", numeric_vars = "wt",
    max_degree = 2, max_interact_vars = 3
  )
  expect_identical(pool, c("wt", "I(wt^2)"))
})

# .poly_base_var -------------------------------------------------------------

test_that(".poly_base_var extracts the base variable from a polynomial term", {
  expect_identical(modelminer:::.poly_base_var("I(hp^2)"), "hp")
  expect_identical(modelminer:::.poly_base_var("I(wt^10)"), "wt")
})

test_that(".poly_base_var captures a compound base expression", {
  # The regex captures everything before the trailing ^k, so an expression
  # base is returned verbatim rather than being rejected.
  expect_identical(modelminer:::.poly_base_var("I(log(x)^2)"), "log(x)")
})

test_that(".poly_base_var returns NA for non-polynomial terms", {
  expect_true(is.na(modelminer:::.poly_base_var("hp")))       # first-order
  expect_true(is.na(modelminer:::.poly_base_var("a:b")))      # interaction
  expect_true(is.na(modelminer:::.poly_base_var("I(hp^2)x"))) # anchored: no trailing junk
})
