# Load the testthat package
library(testthat)

# Define the test suite for validate_complete
testthat::test_that("validate_complete works correctly", {
  # Test case: input is complete and valid
  testthat::expect_silent(
    validate_complete(
      input = c(1, 2, 3, 4, 5),
      type = "warning",
      null_ok = FALSE
    )
  )

  # Test case: input contains missing values and type is "error"
  testthat::expect_error(
    validate_complete(
      input = c(1, 2, NA, 4, 5),
      type = "error",
      null_ok = TRUE
    ),
    "missing values detected. Found 1 missing value\\(s\\) out of 5 total values for 20% global missingness."
  )

  # Test case: input contains missing values and type is "warning"
  testthat::expect_warning(
    validate_complete(
      input = c(1, 2, NA, 4, 5),
      type = "warning",
      null_ok = TRUE
    ),
    "missing values detected. Found 1 missing value\\(s\\) out of 5 total values for 20% global missingness."
  )

  # Test case: input contains missing values and type is "message"
  testthat::expect_message(
    validate_complete(
      input = c(1, 2, NA, 4, 5),
      type = "message",
      null_ok = TRUE
    ),
    "missing values detected. Found 1 missing value\\(s\\) out of 5 total values for 20% global missingness."
  )

  # Test case: input is NULL and null_ok is TRUE
  testthat::expect_silent(
    validate_complete(
      input = NULL,
      type = "warning",
      null_ok = TRUE
    )
  )

  # Test case: input is NULL and null_ok is FALSE
  testthat::expect_error(
    validate_complete(
      input = NULL,
      type = "error",
      null_ok = FALSE
    ),
    "must not be NULL."
  )

  # Test case: input is complete and valid with var_name
  testthat::expect_silent(
    validate_complete(
      input = c(1, 2, 3, 4, 5),
      type = "warning",
      null_ok = FALSE,
      var_name = "complete_vector"
    )
  )

  # Test case: input contains missing values and valid with var_name
  testthat::expect_error(
    validate_complete(
      input = c(1, 2, NA, 4, 5),
      type = "error",
      null_ok = TRUE,
      var_name = "incomplete_vector"
    ),
    "missing values detected. Found 1 missing value\\(s\\) out of 5 total values for 20% global missingness."
  )

  # Test case: input is complete and valid with calls
  testthat::expect_silent(
    validate_complete(
      input = c(1, 2, 3, 4, 5),
      type = "warning",
      null_ok = FALSE,
      calls = 3
    )
  )

  # Test case: input contains missing values and valid with calls
  testthat::expect_error(
    validate_complete(
      input = c(1, 2, NA, 4, 5),
      type = "error",
      null_ok = TRUE,
      calls = 3
    ),
    "missing values detected. Found 1 missing value\\(s\\) out of 5 total values for 20% global missingness."
  )
})
