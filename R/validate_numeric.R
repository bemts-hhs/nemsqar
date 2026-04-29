#' @title Validate Numeric Input
#'
#' @description
#' This function checks if an input is numeric and optionally checks if the
#' values are within a specified range. Depending on the specified type, it will
#' either throw an error, issue a warning, or send a message. Additional
#' arguments allow  for checking NA values, NULL values, and finite values.
#'
#' @param input The data to be validated.
#' @param min Optional. The minimum value for the range check.
#' @param max Optional. The maximum value for the range check.
#' @param na_ok Logical. If TRUE, NA values are allowed. Default is TRUE.
#' @param null_ok Logical. If TRUE, NULL values are allowed. Default is TRUE.
#' @param finite Logical. If TRUE, only finite values are allowed. Default is
#' FALSE.
#' @param type A character string specifying the type of message to be displayed
#' if the input is not numeric or if the values are out of range. Must be one
#' of "error", "warning", or "message".
#' @param var_name Optional. A character string giving the desired variable (or
#' object) name that will appear in console output in place of the how the
#' object will typically be named in messages via deparse(substitute(input)).
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
validate_numeric <- function(
  input,
  min = NULL,
  max = NULL,
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  type = c("error", "warning", "message"),
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

  # Check if the input is numeric
  if (!is.numeric(input)) {
    validate_error_type(
      input = input_name,
      message = "must be {.cls numeric}.",
      type = type,
      calls = calls
    )
  }

  # Check for finite values if finite is TRUE
  if (finite && any(!is.finite(input), na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = "must contain only finite values.",
      type = "error",
      calls = calls
    )
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "e",
      calls = calls
    )
  }

  # Get descriptive statistics on input to provide information in a message
  # only take descriptive statistics if limits are requested
  if (!is.null(min) || !is.null(max)) {
    # Define the required range
    required_range <- glue::glue("[{min}, {max}]")

    # Get unique values of the input
    unique_input <- unique(input)

    # Check the length of the unique values
    if (length(unique_input) > 1) {
      # get minimum
      observed_min <- min(input, na.rm = TRUE)

      # get max
      observed_max <- max(input, na.rm = TRUE)

      # create a pretty range
      observed_range <- glue::glue("[{observed_min}, {observed_max}]")

      # dynamnic text
      dynamic_text <- "Range"
    } else {
      # If only one unique value, use that value for the message
      observed_range <- unique_input

      # dynamnic text
      dynamic_text <- "Value"
    }
  }

  # Check if the input values are within the specified range when only min is provided
  if (!is.null(min) && is.null(max) && any(input < min, na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be greater than or equal to {cli::col_blue(min)}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type,
      calls = calls
    )
  }

  # Check if the input values are within the specified range when only max is provided
  if (!is.null(max) && is.null(min) && any(input > max, na.rm = TRUE)) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be less than or equal to {cli::col_blue(max)}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type,
      calls = calls
    )
  }

  # Check if the input values are within the specified range when min and max are provided
  if (
    !is.null(min) &&
      !is.null(max) &&
      any(input < min | input > max, na.rm = TRUE)
  ) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "values must be contained within range {cli::col_blue(required_range)}. {dynamic_text} of this input was {cli::col_grey(observed_range)}."
      ),
      type = type,
      calls = calls
    )
  }
}
