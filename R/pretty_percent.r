#' Format Numeric Variables as Percentages
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
  # Ensure the input is numeric
  if (!is.numeric(variable)) {
    cli::cli_abort("The `variable` argument must be numeric. You supplied {.cls {class(variable)}}.")
  }

  # Ensure n_decimal is valid
  if (!is.numeric(n_decimal) || n_decimal < 0) {
    cli::cli_abort("The `n_decimal` argument must be a positive numeric value. You supplied {.val {n_decimal}}.")
  }

  # Convert to percentage with specified decimal places
  formatted_percent <- paste0(round(variable * 100, digits = n_decimal), "%")

  # Remove trailing zeros after decimal point and the period if unnecessary
  formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)  # Remove trailing zeros
  formatted_percent <- sub("\\.%$", "%", formatted_percent)             # Remove ending "."

  return(formatted_percent)
}

