test_that("basic percentages work", {
  input <- c(7.5/100, 20/100, 0.3333/1, 100/100, 15.75/100, 0.005/0.1, 150/300)
  output <- pretty_percent(input, n_decimal = 0.01)
  expect_equal(output, c("7.5%", "20%", "33.33%", "100%", "15.75%", "5%", "50%"))
})
