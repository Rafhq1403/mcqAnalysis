test_that("item_difficulty returns proportion correct per item", {
  responses <- matrix(
    c("A", "A", "B", "C",
      "A", "B", "B", "C",
      "A", "A", "C", "D",
      "B", "A", "B", "C",
      "A", "A", "B", "A"),
    nrow = 5, byrow = TRUE,
    dimnames = list(NULL, c("Q1", "Q2", "Q3", "Q4"))
  )
  key <- c("A", "A", "B", "C")
  # Q1 (key=A): A,A,A,B,A -> 4/5; Q2 (key=A): A,B,A,A,A -> 4/5;
  # Q3 (key=B): B,B,C,B,B -> 4/5; Q4 (key=C): C,C,D,C,A -> 3/5.
  result <- item_difficulty(responses, key)
  expect_equal(unname(result), c(0.8, 0.8, 0.8, 0.6))
})

test_that("item_difficulty preserves item names", {
  responses <- matrix(c("A", "B", "A", "B"), nrow = 2,
                      dimnames = list(NULL, c("Q1", "Q2")))
  result <- item_difficulty(responses, c("A", "B"))
  expect_equal(names(result), c("Q1", "Q2"))
})

test_that("item_difficulty assigns default item names if unnamed", {
  responses <- matrix(c("A", "B", "A", "B"), nrow = 2)
  result <- item_difficulty(responses, c("A", "B"))
  expect_equal(names(result), c("item1", "item2"))
})

test_that("item_difficulty handles all-correct items", {
  responses <- matrix(rep("A", 10), nrow = 5,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_equal(unname(item_difficulty(responses, c("A", "A"))), c(1, 1))
})

test_that("item_difficulty handles all-incorrect items", {
  responses <- matrix(rep("B", 10), nrow = 5,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_equal(unname(item_difficulty(responses, c("A", "A"))), c(0, 0))
})

test_that("item_difficulty treats NA as incorrect by default", {
  # Matrix fills by column: Q1 = c("A", NA, "A"), Q2 = c("A", "A", "A")
  responses <- matrix(c("A", NA, "A", "A", "A", "A"), nrow = 3,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_equal(unname(item_difficulty(responses, c("A", "A"))),
               c(2/3, 1.0))
})

test_that("item_difficulty with na.rm = TRUE excludes NA per item", {
  responses <- matrix(c("A", NA, "A", "A", "A", "A"), nrow = 3,
                      dimnames = list(NULL, c("Q1", "Q2")))
  result <- item_difficulty(responses, c("A", "A"), na.rm = TRUE)
  expect_equal(unname(result), c(1.0, 1.0))
})

test_that("item_difficulty errors on mismatched key length", {
  # Use 2 students so the row check passes and key-length check fires.
  responses <- matrix(c("A", "B", "A", "B"), nrow = 2,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_error(item_difficulty(responses, c("A")),
               regexp = "length")
})

test_that("item_difficulty errors with too few students", {
  responses <- matrix(c("A", "B"), nrow = 1,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_error(item_difficulty(responses, c("A", "B")),
               regexp = "at least 2 students")
})

test_that("item_difficulty handles data frame input", {
  responses <- data.frame(
    Q1 = c("A", "A", "B"),
    Q2 = c("C", "C", "D"),
    stringsAsFactors = FALSE
  )
  result <- item_difficulty(responses, c("A", "C"))
  expect_equal(unname(result), c(2/3, 2/3))
})

test_that("item_difficulty accepts numeric responses and key", {
  responses <- matrix(c(1, 1, 2, 1, 2, 2), nrow = 3,
                      dimnames = list(NULL, c("Q1", "Q2")))
  expect_equal(unname(item_difficulty(responses, c(1, 2))),
               c(2/3, 2/3))
})

test_that("item_difficulty values are in [0, 1]", {
  set.seed(1)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- item_difficulty(responses, c("A", "B", "C", "D"))
  expect_true(all(result >= 0 & result <= 1))
})
