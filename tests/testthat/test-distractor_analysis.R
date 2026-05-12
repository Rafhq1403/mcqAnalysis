test_that("distractor_analysis returns a data frame with expected columns", {
  responses <- matrix(
    c("A", "B", "A", "C", "B",
      "B", "A", "B", "B", "C"),
    nrow = 5,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  result <- distractor_analysis(responses, c("A", "A"))
  expect_s3_class(result, "data.frame")
  expect_named(result,
               c("item", "option", "is_key", "frequency",
                 "proportion", "point_biserial"))
})

test_that("distractor_analysis counts frequencies correctly", {
  responses <- matrix(
    c("A", "A", "B", "C", "D"),
    nrow = 5,
    dimnames = list(NULL, "Q1")
  )
  result <- distractor_analysis(responses, "A")
  expect_equal(result$frequency, c(2, 1, 1, 1))
  expect_equal(result$proportion, c(0.4, 0.2, 0.2, 0.2))
  expect_equal(result$option, c("A", "B", "C", "D"))
})

test_that("distractor_analysis flags the key correctly", {
  responses <- matrix(c("A", "A", "B", "B", "C", "D"),
                      nrow = 3,
                      dimnames = list(NULL, c("Q1", "Q2")))
  result <- distractor_analysis(responses, c("A", "B"))
  expect_equal(result$is_key[result$item == "Q1" & result$option == "A"],
               TRUE)
  expect_equal(result$is_key[result$item == "Q1" & result$option == "B"],
               FALSE)
  expect_equal(result$is_key[result$item == "Q2" & result$option == "B"],
               TRUE)
})

test_that("distractor_analysis respects explicit options", {
  responses <- matrix(c("A", "A", "B"), nrow = 3,
                      dimnames = list(NULL, "Q1"))
  result <- distractor_analysis(responses, "A",
                                options = c("A", "B", "C", "D"))
  expect_equal(nrow(result), 4)
  expect_equal(result$option, c("A", "B", "C", "D"))
  expect_equal(result$frequency, c(2, 1, 0, 0))
})

test_that("distractor_analysis returns NA point-biserial for constant options", {
  responses <- matrix(rep("A", 6), nrow = 6,
                      dimnames = list(NULL, "Q1"))
  result <- distractor_analysis(responses, "A",
                                options = c("A", "B"))
  expect_true(is.na(result$point_biserial[result$option == "B"]))
})

test_that("distractor_analysis includes all items", {
  set.seed(6)
  responses <- matrix(
    sample(c("A", "B", "C"), 60, replace = TRUE),
    nrow = 20, ncol = 3,
    dimnames = list(NULL, paste0("Q", 1:3))
  )
  result <- distractor_analysis(responses, c("A", "B", "C"))
  expect_equal(unique(result$item), c("Q1", "Q2", "Q3"))
})
