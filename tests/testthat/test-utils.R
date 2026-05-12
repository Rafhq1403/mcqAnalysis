test_that("validate_inputs rejects non-matrix non-data.frame input", {
  expect_error(item_difficulty(list(c("A", "B")), "A"),
               regexp = "matrix or data frame")
})

test_that("validate_inputs rejects single-row response matrices", {
  responses <- matrix(c("A", "B"), nrow = 1)
  expect_error(item_difficulty(responses, c("A", "B")),
               regexp = "at least 2 students")
})

test_that("validate_inputs rejects zero-column response matrices", {
  responses <- matrix(character(0), nrow = 5, ncol = 0)
  expect_error(item_difficulty(responses, character(0)),
               regexp = "at least 1 item")
})

test_that("validate_inputs accepts data frames", {
  responses <- data.frame(
    Q1 = c("A", "A", "B"),
    Q2 = c("B", "B", "C"),
    stringsAsFactors = FALSE
  )
  expect_silent(item_difficulty(responses, c("A", "B")))
})

test_that("validate_inputs coerces numeric to character internally", {
  responses <- matrix(c(1, 1, 2, 2, 1, 2),
                      nrow = 3,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_silent(item_difficulty(responses, c(1, 2)))
})
