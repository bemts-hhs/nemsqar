#' @title Validate Set Equality
#'
#' @description
#' This function checks if all elements of an input are within a specified set
#' of valid values. Depending on the specified type, it will either throw an
#' error, issue a warning, or send a message.
#'
#' @inheritParams validate_numeric
#' @param valid_set A vector of valid values.
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_set <- function(
  input,
  valid_set,
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
    input_name <- deparse(substitute(input))
  } else {
    # Validate var_name
    validate_character_factor(input = var_name, type = "error", calls = 1)

    # Initialize input_name using var_name
    input_name <- var_name
  }

  # Check if the input is NULL
  if (is.null(input)) {
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
      message = "must not contain NA values.",
      type = "error",
      calls = calls
    )
  }

  # Check if all elements of the input are within the valid set
  invalid_values <- setdiff(x = unique(input), y = valid_set)

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
          " contains invalid values such as {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Valid values are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), ')'))}."
        ),
        type = type,
        calls = calls
      )
    } else {
      # Call the validate_error_type function to handle the message display
      # For large valid_set

      # Clip valid_set down to a length of <= 10
      valid_set <- head(valid_set, n = 10)

      # Clip invalid_values down to a length of <= 10
      invalid_values <- head(invalid_values, n = 10)

      # Modified messaging
      validate_error_type(
        input = input_name,
        message = glue::glue(
          " contains invalid values such as {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Some examples of valid values are: {cli::col_blue(paste0('(', paste0(valid_set, collapse = ', '), '...', ')'))}."
        ),
        type = type,
        calls = calls
      )
    }
  }
}
