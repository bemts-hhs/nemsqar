#' @title Summarize Documentation Time Bands
#'
#' @description
#'
#' Calculates counts and proportions of records within documentation
#' timeliness bands. Documentation timeliness bands are expected to be stored
#' in a categorical variable supplied through `documentation_time_bands_col`.
#' The function summarizes the number and proportion of records falling within
#' each documentation timeliness interval.
#'
#' Optional subgroup analyses may be performed using `group_vars`. When
#' subgroup analyses are requested, denominators are calculated within each
#' subgroup and proportions are reported relative to the subgroup-specific
#' denominator. Confidence intervals may optionally be calculated for the
#' resulting proportions.
#'
#' This helper is intended for use by higher-level documentation timeliness
#' reporting functions and mirrors the role of `summarize_measure()` for
#' NEMSQA measure calculations.
#'
#' @inheritParams summarize_validity_bands
#' @param documentation_time_bands_col The tidyselect column containing the
#' formatted documentation time bands.
#'
#' @return A named list containing one or two tibbles:
#'
#'   - `overall`: Counts and proportions of records within documentation time
#'     bands calculated across the entire population.
#'
#'   - `stratified`: Counts and proportions of records within documentation time
#'     bands calculated independently within levels of the variables supplied
#'     through `group_vars`. Returned only when `group_vars` is specified.
#'
#' Each tibble contains:
#'   - `measure`: Measure identifier.
#'   - `pop`: Population identifier.
#'   - `documentation_time_band`: Documentation time band category.
#'   - Grouping variables supplied through `group_vars`, when requested.
#'   - `numerator`: Count of records in the documentation time band.
#'   - `denominator`: Total number of records within the population or
#'     subgroup.
#'   - `prop`: Proportion of records in the documentation time band.
#'   - `prop_label`: Formatted percentage representation of `prop`.
#'   - `lower_ci`, `upper_ci`: Optional confidence interval bounds.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#'
#' @noRd
#'
summarize_time_bands <- function(
  data,
  measure_name,
  population_name,
  documentation_time_bands_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  group_vars = NULL
) {
  # Ensure the confidence interval method is valid ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # initialize the empty list for output ----
  out <- list()

  # Always get overall counts ----
  out[["overall"]] <- data |>
    dplyr::summarize(
      measure = measure_name,
      pop = population_name,
      numerator = dplyr::n(),
      .by = {{ documentation_time_bands_col }}
    ) |>
    dplyr::arrange({{ documentation_time_bands_col }}) |>
    dplyr::rename(documentation_time_band = {{ documentation_time_bands_col }})

  # Case when CIs are requested and subgroup analysis is not ----
  if (confidence_interval) {
    out$overall <- out$overall |>
      dplyr::mutate(denominator = sum(numerator, na.rm = TRUE)) |>
      nemsqa_binomial_confint(
        x = numerator, # Number of records in documentation time band
        n = denominator, # Total number of records
        method = method, # Chosen confidence interval method
        conf.level = conf.level, # Confidence level (e.g., 0.95 for 95% CI)
        correct = correct # Apply continuity correction if applicable
      )
  } else {
    # Case when neither CIs nor subgroup analysis are requested ----
    out$overall <- out$overall |>
      dplyr::mutate(
        denominator = sum(numerator, na.rm = TRUE),
        prop = numerator / denominator,
        prop_label = pretty_percent(prop, n_decimal = 2)
      )
  }

  # In the case that CIs are not requested and subgroup analysis is requested ----
  if (!is.null(group_vars)) {
    # get total records by group for (optional) requested subgroup analysis ----
    denominators <- data |>
      dplyr::summarize(
        denominator = dplyr::n(),
        .by = dplyr::all_of(group_vars)
      )

    # perform calculations ----
    out[["stratified"]] <- data |>
      dplyr::summarize(
        measure = measure_name,
        pop = population_name,
        numerator = dplyr::n(),
        .by = c(dplyr::all_of(group_vars), {{ documentation_time_bands_col }})
      ) |>
      dplyr::left_join(denominators, by = group_vars) |>
      dplyr::arrange({{ documentation_time_bands_col }}) |>
      dplyr::rename(
        documentation_time_band = {{ documentation_time_bands_col }}
      )

    # Case when CIs are requested and subgroup anlaysis is requested ----
    if (confidence_interval) {
      # perform calculations ----
      out$stratified <- out$stratified |>
        nemsqa_binomial_confint(
          x = numerator, # Number of records in documentation time band
          n = denominator, # Total number of records
          method = method, # Chosen confidence interval method
          conf.level = conf.level, # Confidence level (e.g., 0.95 for 95% CI)
          correct = correct # Apply continuity correction if applicable
        )
    } else {
      # Case when CIs are not requested and subgroup anlaysis is requested ----
      out$stratified <- out$stratified |>
        dplyr::mutate(
          prop = numerator / denominator,
          prop_label = pretty_percent(prop, n_decimal = 2)
        )
    }
  }

  # return the list with requested analyses
  return(out)
}
