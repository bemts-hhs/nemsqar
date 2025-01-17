#' Pretty Percent
#'
#' Function to clean up percent formatted numbers
#'
#' The function utilizes the percent function from the scales package to format
#' the input as a percentage.It removes trailing zeros after the decimal point
#' and simplifies formatting by eliminating unnecessary characters.

#'
#' @param variable A numeric vector representing the values to be formatted as percentages.
#' @param n_decimal The precision (number of decimal places, scale from 0 to 1) to which the percentage is formatted (default is 0.1).
#'
#' @return string representation of percentage
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
#' @examples
#' numerator <- c(7.5, 20.0, 0.3333, 100.0, 15.75, 0.005, 150.0)
#' denominator <- c(100.0, 100.0, 1.0, 100.0, 100.0, 0.1, 300.0)
#'
#' percent_values <- numerator / denominator  # This will create values like 0.075, 0.20, etc.
#' formatted_percent <- pretty_percent(percent_values, n_decimal = 0.01)
#' print(formatted_percent)
#' #Output: "7.5%", "20%", "33.33%", "100%", "15.75%", "5%", "50%"
#'
pretty_percent <- function(variable, n_decimal = 0.1) {

  formatted_percent <- scales::percent(variable, accuracy = n_decimal)

  # If there are trailing zeros after decimal point, remove them
  formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)

  # If it ends with ".%", replace it with "%"
  formatted_percent <- sub("\\.%$", "%", formatted_percent)

  formatted_percent

}
