testthat::test_that("validate_error_type() throws an error with type='error'", {
  testthat::expect_error(
    validate_error_type(
      input = "x_var",
      message = "is invalid.",
      type = "error"
    ),
    regexp = "x_var: is invalid\\."
  )
})

testthat::test_that("validate_error_type() issues a warning with type='warning'", {
  testthat::expect_warning(
    validate_error_type(
      input = "my_input",
      message = "has a problem.",
      type = "warning"
    ),
    regexp = "my_input: has a problem\\."
  )
})

testthat::test_that("validate_error_type() issues a message with type='message'", {
  testthat::expect_message(
    validate_error_type(
      input = "id_code",
      message = "looks unusual.",
      type = "message"
    ),
    regexp = "id_code: looks unusual\\."
  )
})

testthat::test_that("validate_error_type() interpolates message with glue and cli correctly", {
  out <- testthat::capture_error(
    validate_error_type(
      input = "alpha",
      message = "must be positive.",
      type = "error"
    )
  )

  # Ignore ANSI color codes, test plain text
  clean_text <- cli::ansi_strip(out$message)

  testthat::expect_true(grepl("^alpha: must be positive\\.$", clean_text))
})

testthat::test_that("validate_error_type() respects the calls argument", {
  wrapper_fun <- function() {
    inner_fun()
  }

  inner_fun <- function() {
    validate_error_type(
      input = "inner_x",
      message = "failed.",
      type = "error",
      calls = 1
    )
  }

  err <- testthat::capture_error(wrapper_fun())

  # Ensure the call reported by cli_abort is from `inner_fun()`, not wrapper_fun()
  testthat::expect_true(grepl("inner_fun", err$call))
})

testthat::test_that("validate_error_type() works inside nested functions", {
  outer <- function() {
    middle()
  }

  middle <- function() {
    inner()
  }

  inner <- function() {
    validate_error_type(
      input = "deep",
      message = "is wrong.",
      type = "error",
      calls = 2
    )
  }

  err <- testthat::capture_error(outer())

  # Should reference `middle()` because calls=2 steps back
  testthat::expect_true(grepl("middle", err$call))
})
