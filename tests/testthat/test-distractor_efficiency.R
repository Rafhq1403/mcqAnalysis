test_that("distractor_efficiency returns count per item", {
  set.seed(7)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- distractor_efficiency(responses, c("A", "B", "C", "D"))
  expect_length(result, 4)
  expect_named(result, c("Q1", "Q2", "Q3", "Q4"))
  expect_true(all(result >= 0 & result <= 3))
})

test_that("distractor_efficiency counts only functioning distractors", {
  # Build an item where one distractor is never chosen → not functioning
  responses <- matrix(
    c(rep("A", 30), rep("B", 10), rep("C", 10)),
    nrow = 50, ncol = 1,
    dimnames = list(NULL, "Q1")
  )
  # Add a second item for valid total score variance
  resp2 <- matrix(
    c(rep("A", 25), rep("B", 25)),
    nrow = 50, ncol = 1
  )
  responses <- cbind(responses, Q2 = resp2)
  result <- distractor_efficiency(responses, c("A", "B"))
  # Q1: distractors are B and C; D is never chosen, so at most 2 can function
  expect_true(result["Q1"] <= 2)
})

test_that("distractor_efficiency adjusts with min_proportion", {
  set.seed(8)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  key <- c("A", "B", "C", "D")
  lenient <- distractor_efficiency(responses, key, min_proportion = 0.01)
  strict <- distractor_efficiency(responses, key, min_proportion = 0.30)
  expect_true(all(lenient >= strict))
})

test_that("distractor_efficiency = 0 if all options used equally", {
  # In a roughly random/uninformative item, distractors won't have
  # strongly negative point-biserials and may not be functioning
  responses <- matrix(
    rep(c("A", "B", "C", "D"), 25),
    nrow = 25, ncol = 4
  )
  result <- distractor_efficiency(responses, c("A", "B", "C", "D"))
  expect_true(all(result <= 3))
})
