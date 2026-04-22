#' @title Validate Column Names
#'
#' @description
#' This function checks if all column names of a data frame or tibble are within
#' a specified set of valid values. Depending on the specified type, it will
#' either throw an error, issue a warning, or send a message.
#'
#' @inheritParams validate_numeric
#' @param input A data.frame or tibble. validate_names() will run
#' colnames(input) to get the expected column names.
#' @param check_names A vector of column names as strings to check against
#' input.
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_names <- function(
  input,
  check_names,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  var_name = NULL,
  calls = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 2, calls)

  # Get the input name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- rlang::as_name(rlang::enquo(check_names))
  } else {
    # Validate var_name
    validate_character_factor(input = var_name, type = "error", calls = 1)

    # Initialize input_name using var_name
    input_name <- var_name
  }

  # Check if the input is NULL
  if (is.null(check_names)) {
    if (!null_ok) {
      validate_error_type(
        input = input_name,
        message = "must not be NULL.",
        type = "error",
        calls = calls
      )
    }
    return(NULL)
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not be a missing value.",
      type = "error",
      calls = calls
    )
  }

  # Validate the valid_set input
  validate_data_structure(
    input = input,
    structure_type = c("data.frame", "tibble"),
    logic = "or",
    type = "error",
    calls = calls
  )

  # Validate check_names, ensure it has class character
  validate_character_factor(input = check_names, type = "error", calls = 1)

  # Get the column names of the target data
  valid_set <- colnames(input)

  # Check if all column names are within the valid set
  invalid_values <- setdiff(x = check_names, y = valid_set)

  # Check to ensure the invalid_values are not empty
  if (length(invalid_values) > 0) {
    if (length(valid_set) <= 10) {
      # Clip invalid_values down to a length of <= 10
      invalid_values <- head(invalid_values, n = 10)

      # Call the validate_error_type function to handle the message display
      # For small valid_set
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "contains invalid column names such as {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Valid column names are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), ')'))}."
        ),
        type = type,
        calls = calls
      )
    } else {
      # Clip invalid_values down to a length of <= 10
      invalid_values <- head(invalid_values, n = 10)

      # Call the validate_error_type function to handle the message display
      # For large valid_set

      # Clip valid_set down to a length of <= 10
      valid_set <- head(valid_set, 10)

      # Modified messaging
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "contains invalid column names such as {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Some examples of valid column names are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), ',...', ')'))}."
        ),
        type = type,
        calls = calls
      )
    }
  }
}
