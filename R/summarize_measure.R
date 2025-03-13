#' @title Summarize Measure
#'
#' @description
#'
#' Calculates measure numerator, denominator, proportions, and optional
#' confidence intervals for a NEMSQA measure. This function summarizes the
#' information for a specified population and measure, returning a tibble with
#' the calculated values. If requested, the function can also calculate
#' confidence intervals for the proportions using either the Wilson score
#' interval or the Clopper-Pearson exact binomial interval.
#'
#' @param data A dataframe or tibble containing the filtered and calculated
#'   fields for the population of interest.
#' @param measure_name A string containing the description of the measure being
#'   calculated (e.g., "Airway-01").
#' @param population_name A string containing the description of the population
#'   for which the measure is being calculated (e.g., "Adults", "Peds", or
#'   "All").
#' @param numerator_col The tidyselect column containing the numerator data for
#'   the measure (e.g., the number of cases).
#' @param confidence_interval `r lifecycle::badge("experimental")` A logical
#'   value indicating whether to calculate a confidence interval for the
#'   proportion estimate. Defaults to `FALSE`.
#' @param method `r lifecycle::badge("experimental")` A string specifying the
#'   method to calculate the confidence intervals. Options are `"wilson"`
#'   (Wilson score interval) or `"clopper-pearson"` (exact binomial interval).
#'   Partial matching is allowed (e.g., `"w"` or `"c"`). Default is `"wilson"`.
#' @param conf.level `r lifecycle::badge("experimental")` A numeric value
#'   indicating the confidence level for the interval, expressed as a proportion
#'   (e.g., 0.95 for a 95% confidence interval). Defaults to 0.95.
#' @param correct `r lifecycle::badge("experimental")` A logical value
#'   specifying whether to apply continuity correction to the Wilson score
#'   interval when `method = "wilson"`. Default is `TRUE`.
#' @param ... (optional) Additional arguments passed to
#'   `nemsqa_binomial_confint` when calculating confidence intervals.
#'
#' @return A summarized data frame containing:
#'   - `measure`: The measure name.
#'   - `pop`: The population group.
#'   - `numerator`: The count of qualifying events.
#'   - `denominator`: The total count of records.
#'   - `prop`: The proportion of qualifying events.
#'   - `prop_label`: A formatted percentage representation of `prop`
#'      (when `confidence_interval = FALSE`).
#'   - `lower_ci`, `upper_ci`: The lower and upper confidence interval bounds
#'      (when `confidence_interval = TRUE`).
#'
#' @author Samuel Kordik, BBA, BS
#'
summarize_measure <- function(data,
                              measure_name,
                              population_name,
                              numerator_col,
                              confidence_interval = FALSE,
                              method = c("wilson", "clopper-pearson"),
                              conf.level = 0.95,
                              correct = TRUE,
                              ...) {

  # Ensure the confidence interval method is valid
  method <- match.arg(method)

  # If confidence intervals are NOT requested, compute basic summary statistics
  if (!confidence_interval) {
    data |>
      dplyr::summarize(
        measure = measure_name,  # Measure name
        pop = population_name,  # Population category
        numerator = sum({{ numerator_col }}, na.rm = TRUE),  # Count of qualifying events
        denominator = dplyr::n(),  # Total count of records
        prop = sum(numerator / denominator, na.rm = TRUE),  # Proportion of qualifying events
        prop_label = pretty_percent(prop, n_decimal = 2),  # Formatted percentage
        ...
      )

    # If confidence intervals ARE requested, compute summary statistics and CI
  } else {
    data |>
      dplyr::summarize(
        measure = measure_name,
        pop = population_name,
        numerator = sum({{ numerator_col }}, na.rm = TRUE),
        denominator = dplyr::n(),
        ...
      ) |>
      nemsqa_binomial_confint(
        x = numerator,  # Number of qualifying events
        n = denominator,  # Total number of events
        method = method,  # Chosen confidence interval method
        conf.level = conf.level,  # Confidence level (e.g., 0.95 for 95% CI)
        correct = correct  # Apply continuity correction if applicable
      )
  }
}
