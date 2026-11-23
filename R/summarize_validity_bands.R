#' @title Summarize Validity Score Bands
#'
#' @description
#'
#' Calculates counts and proportions of records within validity score ranges.
#' Validity score ranges are expected to be stored in a formatted categorical
#' variable supplied through `validity_score_col`. The function summarizes the
#' number and proportion of records falling within each validity score range.
#'
#' Optional subgroup analyses may be performed by supplying one or more column
#' names through `group_vars`. When subgroup analyses are requested,
#' denominators are calculated within each subgroup and proportions are reported
#' relative to the subgroup-specific denominator. Optionally, confidence
#' intervals may be computed for the resulting proportions.
#'
#' This helper is intended for use by higher-level validity score reporting
#' functions and mirrors the role of `summarize_measure()` for NEMSQA measure
#' calculations.
#'
#' @param data A dataframe or tibble containing the validity score data.
#' @param measure_name A string containing the description of the validity
#'   measure being calculated.
#' @param population_name A string identifying the population represented in
#'   the input data (e.g., `"All"`, `"Adults"`, `"Peds"`).
#' @param validity_score_col The tidyselect column containing the formatted
#'   validity score ranges.
#' @param confidence_interval
#'   `r lifecycle::badge("experimental")`
#'   A logical value indicating whether confidence intervals should be
#'   calculated for the band proportions. Defaults to `FALSE`.
#' @param method
#'   `r lifecycle::badge("experimental")`
#'   A string specifying the confidence interval method. Options are
#'   `"wilson"` or `"clopper-pearson"`. Defaults to `"wilson"`.
#' @param conf.level
#'   `r lifecycle::badge("experimental")`
#'   A numeric value specifying the confidence level. Defaults to `0.95`.
#' @param correct
#'   `r lifecycle::badge("experimental")`
#'   A logical value indicating whether continuity correction should be applied
#'   when using the Wilson method. Defaults to `TRUE`.
#' @param group_vars Optional character vector containing one or more column
#'   names used to perform subgroup analyses. Defaults to `NULL`, which
#'   calculates overall validity score band frequencies for the supplied
#'   population.
#'
#' @return A named list containing one or two tibbles:
#'
#'   - `overall`: Counts and proportions of records within validity score
#'     ranges calculated across the entire population.
#'
#'   - `stratified`: Counts and proportions of records within validity score
#'     ranges calculated independently within levels of the variables supplied
#'     through `group_vars`. Returned only when `group_vars` is specified.
#'
#' Each tibble contains:
#'   - `measure`: Measure identifier.
#'   - `pop`: Population identifier.
#'   - `validity_score_range`: Validity score range category.
#'   - Grouping variables supplied through `group_vars`, when requested.
#'   - `numerator`: Count of records in the validity score range.
#'   - `denominator`: Total number of records within the population or
#'     subgroup.
#'   - `prop`: Proportion of records in the validity score range.
#'   - `prop_label`: Formatted percentage representation of `prop`.
#'   - `lower_ci`, `upper_ci`: Optional confidence interval bounds.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#'
#' @noRd
#'
summarize_validity_bands <- function(
  data,
  measure_name,
  population_name,
  validity_score_col,
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
      .by = {{ validity_score_col }}
    ) |>
    dplyr::arrange({{ validity_score_col }}) |>
    dplyr::rename(validity_score_range = {{ validity_score_col }})

  # Case when CIs are requested and subgroup analysis is not ----
  if (confidence_interval) {
    out$overall <- out$overall |>
      dplyr::mutate(denominator = sum(numerator, na.rm = TRUE)) |>
      nemsqa_binomial_confint(
        x = numerator, # Number of records in validity score range
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
        .by = c(dplyr::all_of(group_vars), {{ validity_score_col }})
      ) |>
      dplyr::left_join(denominators, by = group_vars) |>
      dplyr::arrange({{ validity_score_col }}) |>
      dplyr::rename(validity_score_range = {{ validity_score_col }})

    # Case when CIs are requested and subgroup anlaysis is requested ----
    if (confidence_interval) {
      # perform calculations ----
      out$stratified <- out$stratified |>
        nemsqa_binomial_confint(
          x = numerator, # Number of records in validity score range
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
