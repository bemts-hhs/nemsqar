#' @title Wilson and Clopper-Pearson Confidence Intervals for Binomial Proportions
#'
#' @description
#' Computes confidence intervals for binomial proportions using either the Wilson or Clopper-Pearson method.
#' This function supports vectorized operations and allows optional correction for continuity.
#'
#' @param data An optional `tibble` or `data.frame` containing the variables `x` and `n`. If provided, `x` and `n` should be column names.
#' @param x Numeric vector or column name (if `data` is provided) representing the number of successes.
#' @param n Numeric vector or column name (if `data` is provided) representing the total number of trials.
#' @param method Character string specifying the confidence interval method. Must be either "wilson" (default) or "clopper-pearson".
#' @param conf.level Numeric value between 0 and 1 indicating the confidence level. Defaults to 0.95 (95% confidence interval).
#' @param correct Logical, indicating whether to apply continuity correction for Wilson intervals. Defaults to `TRUE`.
#'
#' @returns
#' A `tibble` containing the estimated proportion (`prop`), lower confidence interval (`lower_ci`),
#' upper confidence interval (`upper_ci`), and a formatted proportion label (`prop_label`). If `data` is provided,
#' these columns are appended to `data` via `dplyr::bind_cols()`.
#'
#' @examples
#' # Example without a data frame
#' nemsqa_binomial_confint(x = c(5, 10, 20), n = c(50, 100, 200), method = "wilson")
#'
#' # Example with a tibble
#' library(tibble)
#' data <- tibble(successes = c(5, 10, 20), trials = c(50, 100, 200))
#' nemsqa_binomial_confint(data, x = successes, n = trials, method = "clopper-pearson")
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
nemsqa_binomial_confint <- function(data = NULL, x, n,
                                    method = c("wilson", "clopper-pearson"),
                                    conf.level = 0.95,
                                    correct = TRUE) {

  # confidence interval function for the nemsqar package
  # Set default method and adjustment method
  method <- match.arg(method)

  # If the user passes a tibble or data.frame
  if (!is.null(data)) {
    x <- data |> dplyr::pull({{x}})
    n <- data |> dplyr::pull({{n}})
  }

  # Initialize lower and upper CI bounds
  lower <- numeric()
  upper <- numeric()

  # Initialize the calculated proportion
  estimate <- numeric()

  # Vectorized Wilson Interval
  # Based on Wilson, E. B. (1927)
  if (method == "wilson") {

    # Create a vectorized version of the function for computing confidence intervals
    # for each pair of (x, n) values using prop.test().
    # Vectorize() makes the function work element-wise over vectors of x and n
    # Define an anonymous function here
    ci <- Vectorize(function(x, n) {

      if (n == 0) {
        return(c(NaN, NaN))  # Return NaN if n == 0
      }

      # Calculate the confidence interval for the proportion using the Wilson method
      # calculate the estimate (proportion) as well
      result <- prop.test(x, n, correct = correct, conf.level = conf.level)

      # Return CI bounds and the estimate
      c(result$conf.int, result$estimate)

    }, vectorize.args = c("x", "n"))  # Specify the arguments to be vectorized

    # Call the vectorized function on the x and n values
    ci_result <- ci(x, n)  # Apply the vectorized function to the vectors of x and n

    # Extract the lower confidence interval (CI) values from the result matrix
    lower <- ci_result[1,]  # First row contains lower CIs

    # Extract the upper confidence interval (CI) values from the result matrix
    upper <- ci_result[2,]  # Second row contains upper CIs

    # Extract the estimate from the result matrix
    estimate <- ci_result[3,] # Third row contains the estimates

  }

  # Vectorized Clopper-Pearson Interval
  # Based on Clopper, C. & Pearson, E. S. (1934)
  if (method == "clopper-pearson") {

    # Create a vectorized version of the function for computing confidence intervals
    # for each pair of (x, n) values using binom.test().
    # Vectorize() makes the function work element-wise over vectors of x and n
    # Define an anonymous function here
    ci <- Vectorize(function(x, n) {

      if (n == 0) {
        return(c(NaN, NaN))  # Return NaN if n == 0
      }

      # Calculate the confidence interval for the proportion using the Clopper-Pearson method
      # calculate the estimate (proportion) as well
      result <- binom.test(x, n, conf.level = conf.level)

      # Return CI bounds and the estimate
      c(result$conf.int, result$estimate)

    }, vectorize.args = c("x", "n"))  # Specify the arguments to be vectorized

    # Call the vectorized function on the x and n values
    ci_result <- ci(x, n)  # Apply the vectorized function to the vectors of x and n

    # Extract the lower confidence interval (CI) values from the result matrix
    lower <- ci_result[1,]  # First row contains lower CIs

    # Extract the upper confidence interval (CI) values from the result matrix
    upper <- ci_result[2,]  # Second row contains upper CIs

    # Extract the estimate from the result matrix
    estimate <- ci_result[3,] # Third row contains the estimates

  }

  # Return as a dataframe/tibble-compatible structure
  lower_upper <- tibble::tibble(prop = estimate, lower_ci = lower, upper_ci = upper) |>
    dplyr::mutate(prop_label = pretty_percent(prop),
                  .after = prop
    )

  if (!is.null(data)) {

    return(dplyr::bind_cols(data, lower_upper))

  } else {

    return(lower_upper)

  }

}
