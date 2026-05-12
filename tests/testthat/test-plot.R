test_that("plot.mcq_analysis returns the input invisibly", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  pdf(file = NULL)  # suppress graphical output during tests
  on.exit(dev.off())
  out <- plot(result)
  expect_identical(out, result)
})

test_that("plot.mcq_analysis runs without error on example data", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  pdf(file = NULL)
  on.exit(dev.off())
  expect_silent(plot(result))
})

test_that("plot.mcq_analysis accepts discrimination_index option", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  pdf(file = NULL)
  on.exit(dev.off())
  expect_silent(
    plot(result, discrimination_metric = "discrimination_index")
  )
})

test_that("plot.mcq_analysis accepts label = FALSE", {
  data(mcq_example)
  result <- mcq_analysis(mcq_example$responses, mcq_example$key)
  pdf(file = NULL)
  on.exit(dev.off())
  expect_silent(plot(result, label = FALSE))
})
