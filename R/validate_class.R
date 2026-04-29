#' @title Validate Class
#'
#' @description
#' This function checks if the input is of the specified class type(s).
#' Depending on the specified type, it will either throw an error, issue a
#' warning, or send a message. It also checks for NULL and NA values based on
#' the specified parameters.
#'
#' @inheritParams validate_numeric
#' @inheritParams validate_data_structure
#' @param class_type A vector of class types to check. Possible values are
#' "numeric", "integer", "logical", "character", "factor", "complex", "raw",
#' "date", "date-time", "hms".
#' @param finite Logical. If TRUE, only finite values are allowed. Default is
#' FALSE.
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_class <- function(
  input,
  class_type = c(
    "numeric",
    "integer",
    "logical",
    "character",
    "factor",
    "complex",
    "raw",
    "date",
    "date-time",
    "hms"
  ),
  logic = c("or", "and"),
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  var_name = NULL,
  calls = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 2, calls)

  # Validate the class_type argument
  class_type <- match.arg(
    arg = class_type,
    choices = c(
      "numeric",
      "integer",
      "logical",
      "character",
      "factor",
      "complex",
      "raw",
      "date",
      "date-time",
      "hms"
    ),
    several.ok = TRUE
  )

  # Validate the logic argument
  logic <- match.arg(arg = logic, choices = c("or", "and"))

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

  # Check for finite values if finite is TRUE and the input is numeric or integer
  if (
    finite &&
      (is.numeric(input) || is.integer(input)) &&
      any(!is.finite(input), na.rm = TRUE)
  ) {
    validate_error_type(
      input = input_name,
      message = "must contain only finite values.",
      type = "error",
      calls = calls
    )
  }

  # Perform the checks for each specified class type
  checks <- sapply(class_type, function(type) {
    switch(
      type,
      "numeric" = is.numeric(input),
      "integer" = is.integer(input),
      "logical" = is.logical(input),
      "character" = is.character(input),
      "factor" = is.factor(input),
      "complex" = is.complex(input),
      "raw" = is.raw(input),
      "date" = lubridate::is.Date(input),
      "date-time" = lubridate::is.POSIXct(input),
      "hms" = hms::is_hms(input)
    )
  })

  # Combine the results based on the logic argument
  is_valid <- if (logic == "and") {
    sum(checks) == length(checks)
  } else {
    sum(checks) >= 1
  }

  # If the input is not valid, display an error, warning, or message
  if (!is_valid) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "must be of class {cli::col_blue(paste0('(', paste(class_type, collapse = ', '), ')'))}."
      ),
      type = type,
      calls = calls
    )
  }
}
