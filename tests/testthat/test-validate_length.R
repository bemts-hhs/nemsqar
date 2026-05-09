testthat::test_that("validate_length() passes silently for correct exact length", {
  x <- 1:3
  testthat::expect_silent(
    validate_length(input = x, exact_length = 3)
  )
})

testthat::test_that("validate_length() errors when exact length is incorrect", {
  x <- 1:3
  testthat::expect_error(
    validate_length(input = x, exact_length = 2, type = "error"),
    regexp = "exact length"
  )
})

testthat::test_that("validate_length() warns when type = 'warning' for incorrect exact length", {
  x <- 1:3
  testthat::expect_warning(
    validate_length(input = x, exact_length = 2, type = "warning")
  )
})

testthat::test_that("validate_length() messages when type = 'message' for incorrect exact length", {
  x <- 1:3
  testthat::expect_message(
    validate_length(input = x, exact_length = 2, type = "message")
  )
})

testthat::test_that("validate_length() handles min_length only (max = Inf)", {
  x <- 1:5
  # Valid
  testthat::expect_silent(
    validate_length(input = x, min_length = 2)
  )
  # Invalid
  testthat::expect_error(
    validate_length(input = x, min_length = 10, type = "error"),
    regexp = "length within range"
  )
})

testthat::test_that("validate_length() handles max_length only (min = -Inf)", {
  x <- 1:5
  # Valid
  testthat::expect_silent(
    validate_length(input = x, max_length = 10)
  )
  # Invalid
  testthat::expect_error(
    validate_length(input = x, max_length = 2, type = "error"),
    regexp = "length within range"
  )
})

testthat::test_that("validate_length() handles range with both min and max", {
  x <- 1:5

  testthat::expect_silent(
    validate_length(input = x, min_length = 3, max_length = 6)
  )

  testthat::expect_error(
    validate_length(input = x, min_length = 6, max_length = 10, type = "error"),
    regexp = "\\[6, 10\\]"
  )
})

testthat::test_that("validate_length() handles list input", {
  x <- list(1, 2, 3)

  testthat::expect_silent(
    validate_length(input = x, exact_length = 3)
  )

  testthat::expect_error(
    validate_length(input = x, exact_length = 2, type = "error")
  )
})

testthat::test_that("validate_length() respects null_ok", {
  # NULL allowed
  testthat::expect_silent(
    validate_length(input = NULL, null_ok = TRUE)
  )

  # NULL not allowed
  testthat::expect_error(
    validate_length(input = NULL, null_ok = FALSE),
    regexp = "must not be NULL"
  )
})

testthat::test_that("validate_length() handles NA checking", {
  x <- c(1, NA, 3)

  testthat::expect_silent(
    validate_length(input = x, exact_length = 3, na_ok = TRUE)
  )

  testthat::expect_error(
    validate_length(input = x, na_ok = FALSE),
    regexp = "must not contain NA values"
  )
})

testthat::test_that("validate_length() honors var_name override", {
  x <- 1:3

  testthat::expect_error(
    validate_length(
      input = x,
      exact_length = 5,
      var_name = "my_vector",
      type = "error"
    ),
    regexp = "my_vector"
  )
})

testthat::test_that("validate_length() works when embedded in another function", {
  wrapper <- function(z) {
    validate_length(input = z, exact_length = 2)
    z
  }

  testthat::expect_silent(wrapper(c(10, 20)))

  testthat::expect_error(wrapper(c(10, 20, 30)))
})
