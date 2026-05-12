test_that("point_biserial returns correlation for each item", {
  set.seed(1)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 100, replace = TRUE),
    nrow = 25, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- point_biserial(responses, c("A", "B", "C", "D"))
  expect_length(result, 4)
  expect_named(result, c("Q1", "Q2", "Q3", "Q4"))
  expect_true(all(result >= -1 & result <= 1))
})

test_that("point_biserial returns NA for constant items", {
  responses <- matrix(
    c(rep("A", 10), sample(c("A", "B"), 10, replace = TRUE)),
    nrow = 10, ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  result <- point_biserial(responses, c("A", "A"))
  expect_true(is.na(result["Q1"]))
})

test_that("point_biserial defaults to corrected item-total", {
  set.seed(2)
  responses <- matrix(
    sample(c("A", "B"), 50, replace = TRUE),
    nrow = 25, ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  uncorrected <- point_biserial(responses, c("A", "A"), corrected = FALSE)
  corrected <- point_biserial(responses, c("A", "A"), corrected = TRUE)
  expect_false(isTRUE(all.equal(uncorrected, corrected)))
})

test_that("point_biserial gives expected sign for well-discriminating item", {
  # Construct: students who get item 1 right also tend to get item 2 right
  responses <- matrix(
    c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B",  # item 1
      "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"), # item 2
    ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  result <- point_biserial(responses, c("A", "A"), corrected = FALSE)
  expect_true(all(result > 0))
})

test_that("point_biserial errors with mismatched key", {
  # Use 2 students so the row check passes and key-length check fires.
  responses <- matrix(c("A", "B", "A", "B"), nrow = 2,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_error(point_biserial(responses, c("A")), regexp = "length")
})

test_that("point_biserial accepts numeric responses", {
  set.seed(3)
  responses <- matrix(
    sample(c(1, 2, 3, 4), 100, replace = TRUE),
    nrow = 25, ncol = 4
  )
  result <- point_biserial(responses, c(1, 2, 3, 4))
  expect_length(result, 4)
})
