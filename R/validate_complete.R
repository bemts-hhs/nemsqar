#' @title Validate Complete Input
#'
#' @description
#' This function checks if the input contains any missing values (NA). Depending
#' on the specified type, it will either throw an error, issue a warning, or
#' send a message. It also checks for NULL values based on the specified
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
validate_complete <- function(
  input,
  type = c("error", "warning", "message"),
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

  # Check for missing values
  missing_count <- sum(is.na(input))

  # Get total values
  total_values <- sum(!is.na(input)) + missing_count

  # Proportion missing
  prop_missing <- paste0(
    round(missing_count / total_values, digits = 4) * 100,
    "%"
  )

  if (missing_count > 0) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "missing values detected. Found {missing_count} missing value(s) out of {total_values} total values for {prop_missing} global missingness."
      ),
      type = type,
      calls = calls
    )
  }
}
