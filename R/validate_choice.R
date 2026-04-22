#' @title Validate Choice
#'
#' @description
#' This function checks if an input is within a specified set of valid choices.
#' Depending on the specified type, it will either throw an error, issue a
#' warning, or send a message.
#'
#' @inheritParams validate_numeric
#' @param choices a character vector of candidate values, often missing, see
#' documentation for `base::match.arg()` for more information.
#' @inheritParams base::match.arg
#' @inheritParams validate_data_pull
#'
#' @details
#' This function uses \code{\link[base]{match.arg}} to validate the input
#' against the allowed choices. Please see the documentation for
#' \code{\link[base]{match.arg}} for more details about how matching is
#' performed.
#'
#'
#' @return The validated input if it is valid.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_choice <- function(
  input,
  choices,
  several.ok = FALSE,
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
    validate_character_factor(input = var_name, type = "error")

    # Initialize input_name using var_name
    input_name <- var_name
  }

  # Check if the input is NULL
  if (is.null(input)) {
    if (!null_ok) {
      validate_error_type(
        input = input_name,
        message = "must not be NULL.",
        type = "error"
      )
    }
    return(NULL)
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "error"
    )
  }

  # Check if all elements of the input are within the valid set
  invalid_values <- setdiff(x = unique(input), y = choices)

  # Attempt to match the argument against allowed choices
  attempt <- try(
    match.arg(arg = input, choices = choices, several.ok = several.ok),
    silent = TRUE
  )

  # If match.arg failed, provide a user-friendly error message
  if (inherits(attempt, "try-error")) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "contains invalid values: {cli::col_grey(paste0('(', paste0(invalid_values, collapse = ', '), ')'))}. Valid values are: {cli::col_blue(paste0('(', paste0(choices, collapse = ', '), ')'))}"
      ),
      type = type,
      calls = calls
    )
  } else {
    # If valid, return the standardized value
    return(attempt)
  }
}
