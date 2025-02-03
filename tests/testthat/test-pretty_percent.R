test_that("basic percentages work", {
  input <- c(7.5/100, 20/100, 0.3333/1, 100/100, 15.75/100, 0.005/0.1, 150/300)
  output <- pretty_percent(input, n_decimal = 2)
  expect_equal(output, c("7.5%", "20%", "33.33%", "100%", "15.75%", "5%", "50%"))
})

test_that("zero values are handled correctly", {
  input <- c(0, 0.0000, 0.00001)
  output <- pretty_percent(input, n_decimal = 2)
  expect_equal(output, c("0%", "0%", "0%"))
})

test_that("negative values are handled correctly", {
  input <- c(-0.05, -0.1234, -1)
  output <- pretty_percent(input, n_decimal = 2)
  expect_equal(output, c("-5%", "-12.34%", "-100%"))
})

test_that("large values above 1 are correctly formatted", {
  input <- c(1.25, 2, 10.3456)
  output <- pretty_percent(input, n_decimal = 1)
  expect_equal(output, c("125%", "200%", "1034.6%"))
})

test_that("trailing zeros are removed", {
  input <- c(0.5, 0.75, 0.333, 1)
  output <- pretty_percent(input, n_decimal = 3)
  expect_equal(output, c("50%", "75%", "33.3%", "100%"))
})

test_that("n_decimal is respected", {
  input <- c(0.125, 0.56789)
  output_0 <- pretty_percent(input, n_decimal = 0)
  output_1 <- pretty_percent(input, n_decimal = 1)
  output_3 <- pretty_percent(input, n_decimal = 3)

  expect_equal(output_0, c("12%", "57%"))
  expect_equal(output_1, c("12.5%", "56.8%"))
  expect_equal(output_3, c("12.5%", "56.789%"))
})

test_that("invalid inputs throw errors", {
  expect_error(pretty_percent("a"), "must be numeric")
  expect_error(pretty_percent(TRUE), "must be numeric")
  expect_error(pretty_percent(c(0.1, "b")), "must be numeric")
  expect_error(pretty_percent(0.5, n_decimal = -1), "must be a positive numeric value")
  expect_error(pretty_percent(0.5, n_decimal = "two"), "must be a positive numeric value")
})
