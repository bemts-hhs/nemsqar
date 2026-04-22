# Define the test suite for validate_data_structure
testthat::test_that("validate_data_structure works correctly", {
  # Synthetic data for testing
  data <- data.frame(
    a = 1:15,
    b = 1:15,
    c = 1:15
  )

  # Test case: input is a data frame and valid
  testthat::expect_silent(
    validate_data_structure(
      input = data,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is a tibble and valid
  tibble_data <- tibble::tibble(a = 1:15, b = 1:15, c = 1:15)
  testthat::expect_silent(
    validate_data_structure(
      input = tibble_data,
      structure_type = c("tbl", "tbl_df"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is a matrix and valid
  matrix_data <- matrix(1:9, nrow = 3)
  testthat::expect_silent(
    validate_data_structure(
      input = matrix_data,
      structure_type = c("matrix"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is a list and valid
  list_data <- list(a = 1, b = 2, c = 3)
  testthat::expect_silent(
    validate_data_structure(
      input = list_data,
      structure_type = c("list"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is an array and valid
  array_data <- array(1:8, dim = c(2, 2, 2))
  testthat::expect_silent(
    validate_data_structure(
      input = array_data,
      structure_type = c("array"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is an atomic vector and valid
  atomic_vector_data <- c(1, 2, 3, 4, 5)
  testthat::expect_silent(
    validate_data_structure(
      input = atomic_vector_data,
      structure_type = c("atomic_vector"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE
    )
  )

  # Test case: input is NULL and null_ok is TRUE
  testthat::expect_silent(
    validate_data_structure(
      input = NULL,
      structure_type = c("data.frame"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = TRUE
    )
  )

  # Test case: input is NULL and null_ok is FALSE
  testthat::expect_error(
    validate_data_structure(
      input = NULL,
      structure_type = c("data.frame"),
      logic = "or",
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must not be NULL."
  )

  # Test case: input contains NA and na_ok is FALSE
  set.seed(123)
  data_with_na <- data.frame(
    a = sample(c(1, 2, NA, 4, 5), size = 15, replace = T),
    b = 1:15,
    c = 1:15
  )
  testthat::expect_error(
    validate_data_structure(
      input = data_with_na,
      structure_type = c("data.frame"),
      logic = "or",
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must not contain NA values."
  )

  # Test case: input is not of the specified structure type
  testthat::expect_error(
    validate_data_structure(
      input = data,
      structure_type = c("matrix"),
      logic = "or",
      type = "error",
      na_ok = FALSE,
      null_ok = FALSE
    ),
    "must be of class \\(matrix\\)."
  )

  # Test case: input is valid with var_name
  testthat::expect_silent(
    validate_data_structure(
      input = data,
      structure_type = c("data.frame"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      var_name = "data_frame"
    )
  )

  # Test case: input is valid with calls
  testthat::expect_silent(
    validate_data_structure(
      input = data,
      structure_type = c("data.frame"),
      logic = "or",
      type = "warning",
      na_ok = FALSE,
      null_ok = FALSE,
      calls = 3
    )
  )
})
