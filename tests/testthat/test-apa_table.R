test_that("apa_table.mcq_analysis returns data frame by default", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab <- apa_table(result)
  expect_s3_class(tab, "data.frame")
})

test_that("apa_table.mcq_analysis includes interpretation columns", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab <- apa_table(result, include_interpretation = TRUE)
  expect_true("Difficulty Level" %in% names(tab))
  expect_true("Discrimination" %in% names(tab))
})

test_that("apa_table.mcq_analysis omits interpretation when requested", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab <- apa_table(result, include_interpretation = FALSE)
  expect_false("Difficulty Level" %in% names(tab))
  expect_false("Discrimination" %in% names(tab))
})

test_that("apa_table respects digits argument", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab2 <- apa_table(result, digits = 2)
  tab4 <- apa_table(result, digits = 4)
  # Values rounded to 2 digits should have ≤ 4 unique decimal patterns
  expect_true(any(tab2$Difficulty != tab4$Difficulty))
})

test_that("apa_table returns markdown when requested", {
  skip_if_not_installed("knitr")
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  md <- apa_table(result, format = "markdown")
  expect_true(any(grepl("\\|", md)))  # markdown table contains pipes
})

test_that("apa_table errors on invalid format", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  expect_error(apa_table(result, format = "csv"))
})

test_that("apa_table difficulty labels: Hard/Moderate/Easy", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab <- apa_table(result, include_interpretation = TRUE)
  expect_true(all(tab$`Difficulty Level` %in% c("Hard", "Moderate", "Easy")))
})

test_that("apa_table discrimination labels match cutoffs", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  tab <- apa_table(result, include_interpretation = TRUE)
  expect_true(all(tab$Discrimination %in%
                  c("Excellent", "Good", "Marginal", "Poor", NA)))
})
