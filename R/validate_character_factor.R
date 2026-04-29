#' @title Validate a Character or Factor Input
#'
#' @description
#' This function checks if an input is of type character or factor. Depending on
#' the specified type, it will either throw an error, issue a warning, or send a
#' message. It also checks for NULL and NA values based on the specified
#' parameters.
#'
#' @inheritParams validate_numeric
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_character_factor <- function(
  input,
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
    validate_class(
      input = var_name,
      class_type = c("character", "factor"),
      logic = "or",
      type = "error",
      calls = calls
    )

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

  # Check if the input is character or factor
  if (!is.character(input) && !is.factor(input)) {
    # Call the validate_error_type function to handle the message display
    validate_error_type(
      input = input_name,
      message = "must be of class {.cls character} or {.cls factor}.",
      type = type,
      calls = calls
    )
  }
}
