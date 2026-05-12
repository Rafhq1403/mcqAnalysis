test_that("mcq_analysis returns an mcq_analysis S3 object", {
  set.seed(9)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- mcq_analysis(responses, c("A", "B", "C", "D"))
  expect_s3_class(result, "mcq_analysis")
  expect_named(result, c("items", "distractors", "total_scores",
                          "n_students", "n_items", "key"))
})

test_that("mcq_analysis items component has expected columns", {
  set.seed(10)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- mcq_analysis(responses, c("A", "B", "C", "D"))
  expect_named(result$items,
               c("item", "key", "difficulty", "point_biserial",
                 "discrimination_index", "distractor_efficiency"))
})

test_that("mcq_analysis n_students and n_items match input dimensions", {
  set.seed(11)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- mcq_analysis(responses, c("A", "B", "C", "D"))
  expect_equal(result$n_students, 50)
  expect_equal(result$n_items, 4)
})

test_that("mcq_analysis preserves answer key in result", {
  set.seed(12)
  responses <- matrix(
    sample(c("A", "B"), 100, replace = TRUE),
    nrow = 50, ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  key <- c("A", "B")
  result <- mcq_analysis(responses, key)
  expect_equal(unname(result$key), key)
})

test_that("mcq_analysis difficulty values match item_difficulty()", {
  set.seed(13)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  key <- c("A", "B", "C", "D")
  result <- mcq_analysis(responses, key)
  d_direct <- item_difficulty(responses, key)
  expect_equal(result$items$difficulty, unname(d_direct))
})

test_that("mcq_analysis point_biserial matches point_biserial()", {
  set.seed(14)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  key <- c("A", "B", "C", "D")
  result <- mcq_analysis(responses, key)
  pb_direct <- point_biserial(responses, key)
  expect_equal(result$items$point_biserial, unname(pb_direct))
})

test_that("print.mcq_analysis returns invisibly", {
  set.seed(15)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 200, replace = TRUE),
    nrow = 50, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  result <- mcq_analysis(responses, c("A", "B", "C", "D"))
  expect_output(print(result), regexp = "Multiple-Choice Item Analysis")
})

test_that("mcq_analysis works on the package example data", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  expect_s3_class(result, "mcq_analysis")
  expect_equal(result$n_students, 200)
  expect_equal(result$n_items, 30)
})
