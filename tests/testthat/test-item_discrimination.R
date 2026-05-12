test_that("item_discrimination defaults to point_biserial", {
  set.seed(4)
  responses <- matrix(
    sample(c("A", "B", "C", "D"), 100, replace = TRUE),
    nrow = 25, ncol = 4,
    dimnames = list(NULL, paste0("Q", 1:4))
  )
  key <- c("A", "B", "C", "D")
  default <- item_discrimination(responses, key)
  pb <- point_biserial(responses, key, corrected = TRUE)
  expect_equal(default, pb)
})

test_that("item_discrimination with discrimination_index returns D values", {
  set.seed(5)
  responses <- matrix(
    sample(c("A", "B"), 200, replace = TRUE),
    nrow = 100, ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  D <- item_discrimination(responses, c("A", "A"),
                            method = "discrimination_index")
  expect_length(D, 2)
  expect_named(D, c("Q1", "Q2"))
  expect_true(all(D >= -1 & D <= 1))
})

test_that("discrimination_index = upper p - lower p by hand", {
  # 10 students, two items, both keyed to "A".
  # Students 1-5 answer "A" on both items (score 2),
  # students 6-10 answer "B" on both items (score 0).
  responses <- matrix(
    c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B",  # Q1
      "A", "A", "A", "A", "A", "B", "B", "B", "B", "B"), # Q2
    ncol = 2,
    dimnames = list(NULL, c("Q1", "Q2"))
  )
  # With group_pct = 0.4: upper 4 are students with score 2 (got it right),
  # lower 4 are students with score 0 (got it wrong).
  # Upper p = 1.0, lower p = 0.0 -> D = +1 on both items.
  D <- item_discrimination(responses, c("A", "A"),
                            method = "discrimination_index",
                            group_pct = 0.4)
  expect_equal(unname(D), c(1, 1))
})

test_that("item_discrimination errors on invalid group_pct", {
  responses <- matrix(c("A", "A"), nrow = 2,
                      dimnames = list(NULL, "Q1"))
  expect_error(
    item_discrimination(responses, "A",
                        method = "discrimination_index",
                        group_pct = 0.6),
    regexp = "between 0 and 0.5"
  )
  expect_error(
    item_discrimination(responses, "A",
                        method = "discrimination_index",
                        group_pct = 0),
    regexp = "between 0 and 0.5"
  )
})

test_that("item_discrimination errors on unknown method", {
  responses <- matrix(c("A", "A"), nrow = 2,
                      dimnames = list(NULL, "Q1"))
  expect_error(item_discrimination(responses, "A", method = "kappa"))
})
