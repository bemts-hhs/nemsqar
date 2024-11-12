#' Format p Value
#'
#' Prints a prettified p value that groups on statistical significance
#'
#' @param p_value numeric p-value to be formatted
#'
#' @return string containing p-value significance and p-value.
#' 
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.
#' 
#' @export
#'
#' @examples
#' format_p_value(0.12)
#' # "p > 0.05 (p = 0.12)"
#' format_p_value(0.0123)
#' # "p < 0.05 (p = 0.0123)"
format_p_value <- function(p_value) {

  if (is.na(p_value)) {

    return(NA)

  } else if (p_value < 0.001) {

    return(

      paste0("p < 0.001 (p = ", formatC(p_value, format = "g"), ")")

    )

  } else if (p_value < 0.01) {

    return(

      paste0("p < 0.01 (p = ", formatC(p_value, format = "g"), ")")

    )

  } else if (p_value < 0.05) {

    return(

      paste0("p < 0.05 (p = ", formatC(p_value, format = "g"), ")")

    )
  } else {

    return(

      paste0("p > 0.05 (p = ", formatC(p_value, format = "g"), ")")

    )

  }
}

#' Pretty Number
#'
#' Formats large numeric values into a more human-readable string.
#'
#' The `pretty_number` function formats large numeric values into a more readable
#' string representation with optional decimal rounding and prefix support. It
#' converts numbers into a compact format using standard suffixes (e.g., "k" for
#'  thousand, "m" for million).
#'
#'
#' @param x A numeric vector to be formatted.
#' @param n_decimal The number of decimal places to round to (default is 2).
#' @param prefix An optional character string to prepend to the formatted number.
#' @param round A logical value indicating whether to round the numbers (default is `TRUE`).
#'
#' @return Returns a character vector of formatted numbers.
#' 
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.
#' @export
#'
#' @examples
#' pretty_number(1500)
#' # "1.5k"
#'
#' pretty_number(2500000, prefix = "$")
#' # "$2.5m"
#'
pretty_number <- function(x, n_decimal = 2, prefix = NULL, round = T) {

    scipen_val <- options()$scipen # get current scipen setting

    options(scipen = 9999) # set scipen to allow very long numbers

    # set values to different orders of magnitude
    thousand <- 1e3
    million <- 1e6
    billion <- 1e9
    trillion <- 1e12
    quadrillion <- 1e15
    quintillion <- 1e18
    sextillion <- 1e21
    septillion <- 1e24
    octillion <- 1e27
    nonillion <- 1e30
    decillion <- 1e33

    if(round == F) { # without rounding, you can get numbers like 0.6k, so you can get the next period up
      # it is important when using round == F to have numbers that are rounded to no further than 1 significant digit

      # get the number of characters in x
      x_length <- nchar(x)

    } else {

      rounded_x <- round(x)

      # get the number of characters in x
      x_length <- nchar(rounded_x)

    }

    # classify x

    suffix <- c("k", "m", "b", "t", "qd", "qt", "sxt", "spt", "oct", "non", "dec")


    x_val <- dplyr::case_when(x_length %in% 4:6 ~ paste0(round(x / thousand, digits = n_decimal), suffix[1]),
                              x_length %in% 7:9 ~ paste0(round(x / million, digits = n_decimal), suffix[2]),
                              x_length %in% 10:12 ~ paste0(round(x / billion, digits = n_decimal), suffix[3]),
                              x_length %in% 13:15 ~ paste0(round(x / trillion, digits = n_decimal), suffix[4]),
                              x_length %in% 16:18 ~ paste0(round(x / quadrillion, digits = n_decimal), suffix[5]),
                              x_length %in% 19:21 ~ paste0(round(x / quintillion, digits = n_decimal), suffix[6]),
                              x_length %in% 22:24 ~ paste0(round(x / sextillion, digits = n_decimal), suffix[7]),
                              x_length %in% 25:27 ~ paste0(round(x / septillion, digits = n_decimal), suffix[8]),
                              x_length %in% 28:30 ~ paste0(round(x / octillion, digits = n_decimal), suffix[9]),
                              x_length %in% 31:33 ~ paste0(round(x / nonillion, digits = n_decimal), suffix[10]),
                              x_length %in% 34:36 ~ paste0(round(x / decillion, digits = n_decimal), suffix[11]),
                              TRUE ~ paste0(round(x, digits = n_decimal))
    )


    if(is.null(prefix)) {

      options(scipen = scipen_val) # set option back to original setting

      return(x_val)

    } else {

      if(!is.character(prefix)) {

        cli::cli_h1("Problem with Input")

        cli::cli_abort(c("The object you passed to prefix was of class {.cls {class(prefix)}}",
                         "i" = "You must supply a {.cls character} vector of length 1 for the prefix argument of {.fn pretty_number} to work."
        ))

      }

      x_val <-  paste0(prefix, x_val)

      options(scipen = scipen_val)

      return(x_val)

    }

}

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
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.
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
