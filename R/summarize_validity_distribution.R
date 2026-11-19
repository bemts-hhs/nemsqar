#' @title Summarize Validity Score Distribution
#'
#' @description
#'
#' Calculates descriptive statistics for validity scores. Summary measures
#' include measures of central tendency, dispersion, and completeness.
#'
#' Optional subgroup analyses may be performed by supplying one or more column
#' names through `group_vars`. When subgroup analyses are requested, summary
#' statistics are calculated independently within each subgroup.
#'
#' This helper is intended for use by higher-level validity score reporting
#' functions and complements `summarize_validity_bands()`.
#'
#' @param data A dataframe or tibble containing the validity score data.
#' @param measure_name A string containing the description of the validity
#'   measure being calculated.
#' @param population_name A string identifying the population represented in
#'   the input data (e.g., `"All"`, `"Adults"`, `"Peds"`).
#' @param validity_score_col The numeric validity score column, typically
#'   bounded to `[0, 1]`.
#' @param group_vars Optional character vector containing one or more column
#'   names used to perform subgroup analyses. Defaults to `NULL`.
#'
#' @return A named list containing one or two tibbles:
#'
#'   - `overall`: Descriptive statistics for the validity score calculated
#'     across the entire population (the empirical marginal distribution).
#'
#'   - `stratified`: Descriptive statistics for the validity score calculated
#'     within levels of the variables supplied through `group_vars`
#'     (the empirical conditional distribution). Returned only when
#'     `group_vars` is specified.
#'
#' Each tibble contains:
#'   - `measure`: Measure identifier.
#'   - `pop`: Population identifier.
#'   - Grouping variables supplied through `group_vars`, when requested.
#'   - `n_obs`: Total number of observations.
#'   - `n_validity_complete`: Number of non-missing validity scores.
#'   - `n_validity_miss`: Number of missing validity scores.
#'   - `min`: Minimum validity score.
#'   - `q25`: 25th percentile.
#'   - `median`: Median validity score.
#'   - `q75`: 75th percentile.
#'   - `p90`: 90th percentile.
#'   - `max`: Maximum validity score.
#'   - `mean`: Mean validity score.
#'   - `sd`: Standard deviation of validity scores.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#'
#' @noRd
#'
summarize_validity_distribution <- function(
  data,
  measure_name,
  population_name,
  validity_score_col,
  group_vars = NULL
) {
  # initialize an empty list ----
  out <- list()

  # perform calculations ----
  # always return the empirical marginal distribution ----
  out[["overall"]] <- data |>
    dplyr::summarize(
      measure = measure_name,
      pop = population_name,
      min = min({{ validity_score_col }}, na.rm = TRUE),
      q25 = stats::quantile(
        {{ validity_score_col }},
        probs = 0.25,
        na.rm = TRUE
      ),
      median = stats::median(
        {{ validity_score_col }},
        na.rm = TRUE
      ),
      q75 = stats::quantile(
        {{ validity_score_col }},
        probs = 0.75,
        na.rm = TRUE
      ),
      p90 = stats::quantile(
        {{ validity_score_col }},
        probs = 0.90,
        na.rm = TRUE
      ),
      max = max({{ validity_score_col }}, na.rm = TRUE),
      mean = mean({{ validity_score_col }}, na.rm = TRUE),
      sd = stats::sd({{ validity_score_col }}, na.rm = TRUE),
      n_validity_complete = sum(!is.na({{ validity_score_col }})),
      n_validity_miss = sum(is.na({{ validity_score_col }})),
      n_obs = dplyr::n()
    )

  if (!is.null(group_vars)) {
    # return the empirical conditional distribution
    out[["stratified"]] <- data |>
      dplyr::summarize(
        measure = measure_name,
        pop = population_name,
        min = min({{ validity_score_col }}, na.rm = TRUE),
        q25 = stats::quantile(
          {{ validity_score_col }},
          probs = 0.25,
          na.rm = TRUE
        ),
        median = stats::median(
          {{ validity_score_col }},
          na.rm = TRUE
        ),
        q75 = stats::quantile(
          {{ validity_score_col }},
          probs = 0.75,
          na.rm = TRUE
        ),
        p90 = stats::quantile(
          {{ validity_score_col }},
          probs = 0.90,
          na.rm = TRUE
        ),
        max = max({{ validity_score_col }}, na.rm = TRUE),
        mean = mean({{ validity_score_col }}, na.rm = TRUE),
        sd = stats::sd({{ validity_score_col }}, na.rm = TRUE),
        n_validity_complete = sum(!is.na({{ validity_score_col }})),
        n_validity_miss = sum(is.na({{ validity_score_col }})),
        n_obs = dplyr::n(),
        .by = group_vars
      )
  }

  # return the list object
  return(out)
}
