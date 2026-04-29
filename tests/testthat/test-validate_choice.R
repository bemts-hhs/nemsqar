# Define the test suite for validate_choice
testthat::test_that("validate_choice works correctly", {
  # Test case: input is valid and matches one of the choices
  testthat::expect_equal(
    validate_choice(
      input = "wilson",
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE
    ),
    "wilson"
  )

  # Test case: input is valid and matches one of the choices with several.ok = TRUE
  testthat::expect_equal(
    validate_choice(
      input = c("wilson", "clopper-pearson"),
      choices = c("wilson", "clopper-pearson"),
      several.ok = TRUE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE
    ),
    c("wilson", "clopper-pearson")
  )

  # Test case: input is NULL and null_ok is TRUE
  testthat::expect_silent(
    validate_choice(
      input = NULL,
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE
    )
  )

  # Test case: input is NULL and null_ok is FALSE
  testthat::expect_error(
    validate_choice(
      input = NULL,
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = FALSE
    ),
    "must not be NULL."
  )

  # Test case: input contains NA and na_ok is FALSE
  testthat::expect_error(
    validate_choice(
      input = c("wilson", NA),
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = FALSE,
      null_ok = TRUE
    ),
    "must not contain NA values."
  )

  # Test case: input contains invalid values
  testthat::expect_error(
    validate_choice(
      input = "invalid_choice",
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE
    ),
    "contains invalid values: \\(invalid_choice\\). Valid values are: \\(wilson, clopper-pearson\\)"
  )

  # Test case: input is valid with var_name
  testthat::expect_equal(
    validate_choice(
      input = "wilson",
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE,
      var_name = "calculate_ci"
    ),
    "wilson"
  )

  # Test case: input is valid with calls
  testthat::expect_equal(
    validate_choice(
      input = "wilson",
      choices = c("wilson", "clopper-pearson"),
      several.ok = FALSE,
      type = "error",
      na_ok = TRUE,
      null_ok = TRUE,
      calls = 3
    ),
    "wilson"
  )
})
