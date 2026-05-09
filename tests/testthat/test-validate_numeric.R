testthat::test_that("validate_numeric() accepts valid numeric input", {
  testthat::expect_no_error(
    validate_numeric(input = c(1, 2, 3))
  )
})

testthat::test_that("validate_numeric() errors on non-numeric input", {
  testthat::expect_error(
    validate_numeric(input = "a", type = "error")
  )
})

testthat::test_that("validate_numeric() warns on non-numeric when type = 'warning'", {
  testthat::expect_warning(
    validate_numeric(input = "a", type = "warning")
  )
})

testthat::test_that("validate_numeric() messages on non-numeric when type = 'message'", {
  testthat::expect_message(
    validate_numeric(input = "a", type = "message")
  )
})

testthat::test_that("validate_numeric() handles NULL correctly", {
  testthat::expect_no_error(
    validate_numeric(input = NULL, null_ok = TRUE)
  )

  testthat::expect_error(
    validate_numeric(input = NULL, null_ok = FALSE)
  )
})

testthat::test_that("validate_numeric() handles NA values", {
  # NA allowed
  testthat::expect_no_error(
    validate_numeric(input = c(1, NA, 2), na_ok = TRUE)
  )

  # NA not allowed
  testthat::expect_error(
    validate_numeric(input = c(1, NA, 2), na_ok = FALSE)
  )
})

testthat::test_that("validate_numeric() checks finite values", {
  testthat::expect_no_error(
    validate_numeric(input = c(1, 2, 3), finite = TRUE)
  )

  testthat::expect_error(
    validate_numeric(input = c(1, Inf, 3), finite = TRUE)
  )
})

testthat::test_that("validate_numeric() enforces min range", {
  # Should pass
  testthat::expect_no_error(
    validate_numeric(input = c(5, 6), min = 1)
  )

  # Should fail
  testthat::expect_error(
    validate_numeric(input = c(0, 5), min = 1)
  )
})

testthat::test_that("validate_numeric() enforces max range", {
  testthat::expect_no_error(
    validate_numeric(input = c(1, 2), max = 3)
  )

  testthat::expect_error(
    validate_numeric(input = c(1, 5), max = 3)
  )
})

testthat::test_that("validate_numeric() enforces both min and max", {
  testthat::expect_no_error(
    validate_numeric(input = c(5, 6), min = 1, max = 10)
  )

  testthat::expect_error(
    validate_numeric(input = c(-1, 12), min = 0, max = 10)
  )
})

testthat::test_that("validate_numeric() honors var_name override", {
  # Ensure custom var_name does not error
  testthat::expect_no_error(
    validate_numeric(
      input = 5,
      var_name = "custom_var",
      type = "message"
    )
  )
})

testthat::test_that("validate_numeric() respects type matching", {
  # Any unsupported type should error via match.arg
  testthat::expect_error(
    validate_numeric(input = 1, type = "invalid_type")
  )
})
