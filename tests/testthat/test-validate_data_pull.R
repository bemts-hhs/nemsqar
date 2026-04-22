# Define the test suite for validate_data_pull
testthat::test_that("validate_data_pull works correctly", {
  # Synthetic data for testing
  data <- data.frame(
    Trauma_Type = c("Blunt", "Penetrating", "Blunt", "Unknown"),
    Patient_Age_Years = c(30, 60, 45, 50),
    RTS = c(7.84, 6.90, 7.00, 6.50),
    ISS = c(10, 25, 15, 20)
  )

  # Test case: valid column extraction
  testthat::expect_equal(
    validate_data_pull(
      input = data,
      col = Trauma_Type,
      type = "error",
      var_name = "Trauma_Type",
      calls = 2
    ),
    data$Trauma_Type
  )

  # Test case: valid column extraction without var_name
  testthat::expect_equal(
    validate_data_pull(
      input = data,
      col = Trauma_Type,
      type = "error",
      calls = 2
    ),
    data$Trauma_Type
  )

  # Test case: valid column extraction with custom function
  custom_function <- function(df, col) {
    extracted_col <- validate_data_pull(
      input = df,
      col = {{ col }},
      var_name = deparse(substitute(col))
    )
    return(extracted_col)
  }

  testthat::expect_equal(
    custom_function(df = data, col = Trauma_Type),
    data$Trauma_Type
  )

  # Test case: invalid column extraction
  testthat::expect_error(
    validate_data_pull(
      input = data,
      col = Invalid_Column,
      type = "error",
      var_name = "Invalid_Column",
      calls = 2
    ),
    "It was not possible to validate `Invalid_Column`, please check this column in the function call."
  )

  # Test case: invalid column extraction without var_name
  testthat::expect_error(
    validate_data_pull(
      input = data,
      col = Invalid_Column,
      type = "error",
      calls = 2
    ),
    "It was not possible to validate `Invalid_Column`, please check this column in the function call."
  )

  # Test case: valid column extraction with warning type
  testthat::expect_warning(
    validate_data_pull(
      input = data,
      col = Trauma_Type,
      type = "warning",
      var_name = "Trauma_Type",
      calls = 2
    ),
    NA
  )

  # Test case: valid column extraction with message type
  testthat::expect_message(
    validate_data_pull(
      input = data,
      col = Trauma_Type,
      type = "message",
      var_name = "Trauma_Type",
      calls = 2
    ),
    NA
  )
})
