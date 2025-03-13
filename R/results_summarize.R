#' @title Tabulate Measure Results
#'
#' @description
#'
#' Calculates measure numerator, denominator, and proportions for a NEMSQA
#' measure and each population (total, adult, and pediatric). The function
#' returns a summarized result table for the selected populations, with optional
#' confidence intervals for the proportions.
#'
#' This function is used throughout the package to calculate measure results
#' for different populations (e.g., total population, adults, and pediatric
#' groups) based on the given input data. Each of the population arguments
#' (`total_population`, `adult_population`, `peds_population`) defaults to `NULL`.
#' If a population argument is `NULL`, it will be excluded from the results.
#'
#' @param total_population A dataframe or tibble containing the filtered and
#'   calculated fields for the total population. Defaults to `NULL`.
#' @param adult_population A dataframe or tibble containing the filtered and
#'   calculated fields for the adult population. Defaults to `NULL`.
#' @param peds_population A dataframe or tibble containing the filtered and
#'   calculated fields for the pediatric population. Defaults to `NULL`.
#' @param measure_name A string containing the description of the measure being
#'   calculated.
#' @param population_names A vector of strings specifying which populations
#'   (total, adult, peds) to include in the result. Default includes all
#'   populations.
#' @param numerator_col The tidyselect column containing the numerator data for
#'   the measure (e.g., the number of cases).
#' @param confidence_interval `r lifecycle::badge("experimental")` A logical
#'   value indicating whether to include confidence intervals in the result.
#'   Defaults to FALSE.
#' @param method `r lifecycle::badge("experimental")` A string specifying the
#'   method to calculate confidence intervals. Options are "wilson" or
#'   "clopper-pearson". Default is "wilson".
#' @param conf.level `r lifecycle::badge("experimental")` A numeric value
#'   indicating the confidence level for the confidence intervals. Default is
#'   0.95 (95% confidence).
#' @param correct `r lifecycle::badge("experimental")` A logical value
#'   specifying whether to apply continuity correction when calculating
#'   confidence intervals. Default is TRUE.
#' @param ... (optional) Additional arguments passed to the `summarize_measure`
#'   function used for calculating measure results.
#'
#' @return A tibble containing the summarized measure results for the selected
#' populations. The output includes:
#'   - `measure`: The measure name.
#'   - `pop`: The population group (e.g., "All", "Adults", "Peds").
#'   - `numerator`: The count of qualifying events.
#'   - `denominator`: The total number of records in the population.
#'   - `prop`: The proportion of qualifying events.
#'   - `prop_label`: A formatted percentage representation of `prop`.
#'   - `lower_ci`, `upper_ci`: The lower and upper confidence interval bounds
#'     (if `confidence_interval = TRUE`).
#'
#' If multiple populations are specified, their results are combined into a
#' single tibble using `dplyr::bind_rows()`.
#'
#' @author Samuel Kordik, BBA, BS
#'
results_summarize <- function(total_population = NULL,
                              adult_population = NULL,
                              peds_population = NULL,
                              measure_name,
                              population_names = c("all", "adults", "peds"),
                              numerator_col,
                              confidence_interval = FALSE,
                              method = c("wilson", "clopper-pearson"),
                              conf.level = 0.95,
                              correct = TRUE,
                              ...) {

  # Ensure method argument is valid (either "wilson" or "clopper-pearson")
  method <- match.arg(method)

  # Ensure confidence_interval is a logical (TRUE/FALSE)
  confidence_interval <- as.logical(confidence_interval)

  # Ensure population_names are valid
  valid_populations <- c("all", "adults", "peds")
  population_names <- match.arg(population_names, choices = valid_populations, several.ok = TRUE)

  # Dictionary to map population codes to descriptive labels
  population_labels <- c(all = "All",
                         adults = "Adults",
                         peds = "Peds")

  # Initialize an empty list to store results for each selected population
  results_list <- list()

  # Check if "adult" is in the input vector and process if present
  if ("adults" %in% population_names && !is.null(adult_population)) {
    results_list$adult <- adult_population |> summarize_measure(
      measure_name,
      population_labels[["adults"]],  # Use standardized label
      {{numerator_col}},
      confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )
  }

  # Check if "peds" is in the input vector and process if present
  if ("peds" %in% population_names && !is.null(peds_population)) {
    results_list$peds <- peds_population |> summarize_measure(
      measure_name,
      population_labels[["peds"]],  # Use standardized label
      {{numerator_col}},
      confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )
  }

  # Check if "total" is in the input vector and process if present
  if ("all" %in% population_names && !is.null(total_population)) {
    results_list$total <- total_population |> summarize_measure(
      measure_name,
      population_labels[["all"]],  # Use standardized label
      {{numerator_col}},
      confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )
  }

  # Combine only the selected population results into a single tibble
  dplyr::bind_rows(results_list)
}
