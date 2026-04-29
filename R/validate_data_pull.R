#' @title Validate Data Extraction
#'
#' @description
#' This function extracts a column from a data frame or tibble and returns it as
#' a vector. If the column does not exist or an error occurs, it returns a clean
#' error message using the cli package.
#'
#' @inheritParams validate_numeric
#' @param input A data frame or tibble.
#' @param col The column to be extracted.
#' @param var_name Optional. The name of the variable for error messaging.
#' @param calls Optional. The number of callers to go back in the call stack for
#' error messaging. If NULL, will default to 2.
#'
#' @return The extracted column as a vector.
#'
#' @details
#' This function is designed to validate and extract a specified column from a
#' data frame or tibble. When using `validate_data_pull()` within custom
#' functions, it is necessary to call a given bare column name  using tidy
#' evaluation (e.g., `{{ col }}`). This allows the function to correctly capture
#' and evaluate the column name within the custom function. However, when
#' calling this function directly on a data frame, tidy evaluation is not
#' required.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_data_pull <- function(
  input,
  type = c("error", "warning", "message"),
  col,
  var_name = NULL,
  calls = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 2, calls)

  # Get the column name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- deparse(substitute(col))
  } else {
    input_name <- var_name
  }

  # Extract the column and handle errors
  result <- try(
    input |> dplyr::pull({{ col }}),
    silent = TRUE
  )

  # If result flags an error, return the error, else return the value
  if (inherits(x = result, what = "try-error")) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "It was not possible to validate `{cli::col_blue(input_name)}`, please check this column in the function call."
      ),
      type = type,
      calls = calls
    )
  } else {
    # Return extracted vector
    return(result)
  }
}
