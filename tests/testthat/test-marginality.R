# .eligible_candidates interaction marginality --------------------------------

test_that(".eligible_candidates filters interactions without main effects", {
  f <- mpg ~ wt
  candidates <- c("hp", "wt:hp", "cyl:hp", "I(wt^2)")
  result <- .eligible_candidates(candidates, f)
  # hp is first-order, always eligible

  expect_true("hp" %in% result)
  # wt:hp requires both wt AND hp in model; only wt is in model
  expect_false("wt:hp" %in% result)
  # cyl:hp requires both cyl AND hp; neither in model
  expect_false("cyl:hp" %in% result)
  # I(wt^2) requires wt in model (polynomial marginality)
  expect_true("I(wt^2)" %in% result)
})

test_that(".eligible_candidates allows interactions when both main effects present", {
  f <- mpg ~ wt + hp
  candidates <- c("cyl", "wt:hp", "I(wt^2)")
  result <- .eligible_candidates(candidates, f)
  expect_true("wt:hp" %in% result)
  expect_true("I(wt^2)" %in% result)
})

test_that(".eligible_candidates filters 3-way interactions correctly", {
  f <- mpg ~ wt + hp
  candidates <- c("wt:hp:cyl")
  result <- .eligible_candidates(candidates, f)
  expect_false("wt:hp:cyl" %in% result)  # cyl not in model
})

test_that("mine greedy enforces interaction marginality", {
  result <- mine(mtcars, mpg, method = "greedy", max_degree = 1, verbose = FALSE)
  terms <- attr(terms(result$Formula), "term.labels")
  interactions <- grep(":", terms, value = TRUE)
  for (int in interactions) {
    parts <- strsplit(int, ":", fixed = TRUE)[[1]]
    expect_true(all(parts %in% terms),
                info = paste("Interaction", int, "missing main effects"))
  }
})
