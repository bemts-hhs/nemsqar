#' @title Validate Length of an Input
#'
#' @description
#' This function checks if the length of a vector or list is within a specified
#' range. Depending on the specified type, it will either throw an error, issue
#' a warning, or send a message. It also checks for NULL and NA values based on
#' the specified parameters.
#'
#' @inheritParams validate_numeric
#' @param exact_length The required length of the vector or list. If this
#' argument is used, then min_length and max_length are not required.
#' @param min_length The minimum length of the vector or list.
#' @param max_length The maximum length of the vector or list.
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_length <- function(
  input,
  exact_length = NULL,
  min_length = NULL,
  max_length = NULL,
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

  # Check if the exact length is specified
  if (!is.null(exact_length)) {
    if (length(input) != exact_length) {
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "must have an exact length of {cli::col_blue(exact_length)}."
        ),
        type = type,
        calls = calls
      )
    }
  } else {
    # Handle nulls in min_length and max_length
    # Max goes to Inf
    max_length <- ifelse(
      all(is.null(exact_length), is.null(max_length), !is.null(min_length)),
      Inf,
      max_length
    )

    # Min goes to -Inf
    min_length <- ifelse(
      all(is.null(exact_length), !is.null(max_length), is.null(min_length)),
      -Inf,
      min_length
    )

    # Get required range
    required_range <- glue::glue("[{min_length}, {max_length}]")

    # Check if the length is within the specified range
    if (length(input) < min_length || length(input) > max_length) {
      validate_error_type(
        input = input_name,
        message = glue::glue(
          "must have a length within range {cli::col_blue(required_range)}."
        ),
        type = type,
        calls = calls
      )
    }
  }
}
