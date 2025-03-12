testthat::test_that("nemsqa_binomial_confint() returns correct results", {

  # Simple case: Single value, Wilson method
  result <- nemsqa_binomial_confint(x = 5, n = 50, method = "wilson")
  testthat::expect_true(nrow(result) == 1)
  testthat::expect_true(all(c("prop", "lower_ci", "upper_ci", "prop_label") %in% colnames(result)))
  testthat::expect_true(result$prop == 5 / 50)

  # Simple case: Single value, Clopper-Pearson method
  result <- nemsqa_binomial_confint(x = 5, n = 50, method = "clopper-pearson")
  testthat::expect_true(nrow(result) == 1)
  testthat::expect_true(result$prop == 5 / 50)

  # Vectorized input: Multiple values, Wilson method
  result <- nemsqa_binomial_confint(x = c(5, 10), n = c(50, 100), method = "wilson")
  testthat::expect_true(nrow(result) == 2)
  testthat::expect_equal(result$prop, c(5 / 50, 10 / 100))

  # Vectorized input: Multiple values, Clopper-Pearson method
  result <- nemsqa_binomial_confint(x = c(5, 10), n = c(50, 100), method = "clopper-pearson")
  testthat::expect_true(nrow(result) == 2)

  # Test with data.frame input
  df <- tibble::tibble(successes = c(5, 10), trials = c(50, 100))
  result <- nemsqa_binomial_confint(df, x = successes, n = trials, method = "wilson")
  testthat::expect_true(nrow(result) == 2)
  testthat::expect_true(all(c("prop", "lower_ci", "upper_ci", "prop_label") %in% colnames(result)))

  # Test confidence level changes
  result_90 <- nemsqa_binomial_confint(x = 5, n = 50, conf.level = 0.90)
  result_99 <- nemsqa_binomial_confint(x = 5, n = 50, conf.level = 0.99)
  testthat::expect_true(result_90$lower_ci > result_99$lower_ci)
  testthat::expect_true(result_90$upper_ci < result_99$upper_ci)

  # Edge case: x = 0
  result <- nemsqa_binomial_confint(x = 0, n = 50, method = "wilson")
  testthat::expect_true(result$prop == 0)

  # Edge case: x = n
  result <- nemsqa_binomial_confint(x = 50, n = 50, method = "wilson")
  testthat::expect_true(result$prop == 1)

  # Edge case: n = 0
  result <- nemsqa_binomial_confint(x = 0, n = 0, method = "wilson")
  testthat::expect_true(is.nan(result$prop))

  # Partial matching for method
  result <- nemsqa_binomial_confint(x = 5, n = 50, method = "w")
  testthat::expect_true(nrow(result) == 1)

  result <- nemsqa_binomial_confint(x = 5, n = 50, method = "c")
  testthat::expect_true(nrow(result) == 1)

  # Handling of incorrect method input
  testthat::expect_error(nemsqa_binomial_confint(x = 5, n = 50, method = "invalid"))

  # Handling of non-numeric input
  testthat::expect_error(nemsqa_binomial_confint(x = "five", n = 50, method = "wilson"))
  testthat::expect_error(nemsqa_binomial_confint(x = 5, n = "fifty", method = "wilson"))

})
