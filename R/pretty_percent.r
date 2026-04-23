#' @title Format Numeric Variables as Percentages
#'
#' @description
#'
#' This function formats numeric variables as percentages with a specified
#' number of decimal places. It refines the output by removing unnecessary
#' trailing zeros after the decimal point and ensures the percentage sign is
#' correctly applied without extraneous characters, resulting in a polished,
#' human-readable percentage representation.
#'
#' @param variable A numeric vector representing proportions to format as
#'   percentages. The values are on a scale from 0 to 1.
#' @param n_decimal A numeric value specifying the number of decimal places.
#'   Defaults to `1`.
#'
#' @returns A character vector containing the formatted percentages.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' pretty_percent(0.12345)  # Default decimal places
#' pretty_percent(0.12345, n_decimal = 2)  # Two decimal places
#' pretty_percent(c(0.1, 0.25, 0.3333), n_decimal = 1)  # Vector input
#'
#' @author Nicolas Foss, Ed.D., MS
#'
pretty_percent <- function(variable, n_decimal = 1) {
  # Save the current options settings to restore them later ----
  old <- options()

  # Ensure that the original options are restored when the function exits, ----
  # even if an error occurs
  on.exit(options(old))

  # Set the 'scipen' option to a high value to prevent scientific notation ----
  # in the output. This ensures that large numbers are displayed in their
  # full numeric form rather than in scientific notation.
  options(scipen = 9999)

  # Ensure the input is numeric ----
  validate_numeric(input = variable, min = -1, max = 1, type = "error")

  # Ensure n_decimal is valid ----
  validate_numeric(input = n_decimal, min = 0, type = "error")

  # Format percentages with NA handling ----
  formatted_percent <- ifelse(
    is.na(variable),
    NA_character_,
    paste0(round(variable * 100, digits = n_decimal), "%")
  )

  # Remove trailing zeros after decimal point and the period if unnecessary ----
  formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent) # Remove trailing zeros
  formatted_percent <- sub("\\.%$", "%", formatted_percent) # Remove ending "."

  # Complete the transformation ----
  return(formatted_percent)
}
