# Define the test suite for validate_character_factor
testthat::test_that("validate_character_factor works correctly", {
  # Test case: input is character and valid
  testthat::expect_silent(
    validate_character_factor(
      input = c("Blunt", "Penetrating", "Blunt", "Unknown"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is factor and valid
  testthat::expect_silent(
    validate_character_factor(
      input = factor(c("Blunt", "Penetrating", "Blunt", "Unknown")),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is NULL and null_ok is TRUE
  testthat::expect_silent(
    validate_character_factor(
      input = NULL,
      type = "warning",
      na_ok = FALSE,
      null_ok = TRUE
    )
  )

  # Test case: input is NULL and null_ok is FALSE
  testthat::expect_error(
    validate_character_factor(
      input = NULL,
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must not be NULL."
  )

  # Test case: input contains NA and na_ok is FALSE
  testthat::expect_error(
    validate_character_factor(
      input = c("Blunt", "Penetrating", NA, "Unknown"),
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must not contain NA values."
  )

  # Test case: input is not character or factor
  testthat::expect_error(
    validate_character_factor(
      input = c(1, 2, 3, 4),
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must be of class.*character.*factor"
  )

  # Test case: input is character and valid with var_name
  testthat::expect_silent(
    validate_character_factor(
      input = c("Blunt", "Penetrating", "Blunt", "Unknown"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      var_name = "Trauma_Type"
    )
  )

  # Test case: input is factor and valid with var_name
  testthat::expect_silent(
    validate_character_factor(
      input = factor(c("Blunt", "Penetrating", "Blunt", "Unknown")),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      var_name = "Trauma_Type"
    )
  )

  # Test case: input is character and valid with calls
  testthat::expect_silent(
    validate_character_factor(
      input = c("Blunt", "Penetrating", "Blunt", "Unknown"),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      calls = 3
    )
  )

  # Test case: input is factor and valid with calls
  testthat::expect_silent(
    validate_character_factor(
      input = factor(c("Blunt", "Penetrating", "Blunt", "Unknown")),
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      calls = 3
    )
  )
})
