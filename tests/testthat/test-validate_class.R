# Define the test suite for validate_class
testthat::test_that("validate_class works correctly", {
  # Test case: input is numeric and valid
  testthat::expect_silent(
    validate_class(
      input = c(1.1, 2.2, 3.3),
      class_type = c("numeric"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      finite = TRUE
    )
  )

  # Test case: input is integer and valid
  testthat::expect_silent(
    validate_class(
      input = c(1L, 2L, 3L),
      class_type = c("integer"),
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE,
      finite = FALSE
    )
  )

  # Test case: input is character and valid
  testthat::expect_silent(
    validate_class(
      input = c("a", "b", "c"),
      class_type = c("character"),
      type = "message",
      na_ok = TRUE,
      null_ok = TRUE,
      finite = FALSE
    )
  )

  # Test case: input is NULL and null_ok is TRUE
  testthat::expect_silent(
    validate_class(
      input = NULL,
      class_type = c("numeric"),
      type = "warning",
      na_ok = FALSE,
      null_ok = TRUE,
      finite = TRUE
    )
  )

  # Test case: input is NULL and null_ok is FALSE
  testthat::expect_error(
    validate_class(
      input = NULL,
      class_type = c("numeric"),
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE,
      finite = TRUE
    ),
    "must not be NULL."
  )

  # Test case: input contains NA and na_ok is FALSE
  testthat::expect_error(
    validate_class(
      input = c(1.1, NA, 3.3),
      class_type = c("numeric"),
      type = "error",
      na_ok = FALSE,
      null_ok = TRUE,
      finite = TRUE
    ),
    "must not contain NA values."
  )

  # Test case: input contains non-finite values and finite is TRUE
  testthat::expect_error(
    validate_class(
      input = c(1.1, Inf, 3.3),
      class_type = c("numeric"),
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE,
      finite = TRUE
    ),
    "must contain only finite values."
  )

  # Test case: input is not of the specified class type
  testthat::expect_error(
    validate_class(
      input = c(1.1, 2.2, 3.3),
      class_type = c("integer"),
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE,
      finite = TRUE
    ),
    "must be of class \\(integer\\)."
  )

  # Test case: input is valid with var_name
  testthat::expect_silent(
    validate_class(
      input = c(1.1, 2.2, 3.3),
      class_type = c("numeric"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      finite = TRUE,
      var_name = "numeric_column"
    )
  )

  # Test case: input is valid with calls
  testthat::expect_silent(
    validate_class(
      input = c(1.1, 2.2, 3.3),
      class_type = c("numeric"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      finite = TRUE,
      calls = 3
    )
  )
})
