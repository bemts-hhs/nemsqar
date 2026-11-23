#' @title Summarize Documentation Time Distribution
#'
#' @description
#'
#' Calculates descriptive statistics for documentation timeliness using both
#' documentation time in hours and documentation time in days.
#'
#' The function returns empirical marginal distributions for the supplied
#' population and, optionally, empirical conditional distributions stratified
#' by one or more grouping variables supplied through `group_vars`.
#'
#' Documentation timeliness is summarized separately for hours and days and
#' identified using the `time_unit` variable in the returned output.
#'
#' @inheritParams summarize_validity_bands
#' @inheritParams summarize_time_bands
#' @param documentation_time_hours The tidyselect column containing
#'   documentation timeliness expressed in hours.
#' @param documentation_time_days The tidyselect column containing
#'   documentation timeliness expressed in days.
#'
#' @return A named list containing two or more tibbles:
#'
#'   - `overall`: Descriptive statistics for the documentation time calculated
#'     across the entire population (the empirical marginal distribution).
#'
#'   - `stratified`: Descriptive statistics for the documentation time
#'     calculated within levels of the variables supplied through `group_vars`
#'     (the empirical conditional distribution). Returned only when `group_vars`
#'     is specified.
#'
#' Each tibble contains:
#'   - `measure`: Measure identifier.
#'   - `pop`: Population identifier.
#'   - Grouping variables supplied through `group_vars`, when requested.
#'   - `n_obs`: Total number of observations.
#'   - `n_time_complete`: Number of non-missing documentation times.
#'   - `n_time_miss`: Number of missing documentation times.
#'   - `min`: Minimum documentation time.
#'   - `q25`: 25th percentile.
#'   - `median`: Median documentation time.
#'   - `q75`: 75th percentile.
#'   - `p90`: 90th percentile.
#'   - `max`: Maximum documentation time.
#'   - `mean`: Mean documentation time.
#'   - `sd`: Standard deviation of documentation times.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#'
#' @noRd
#'
summarize_time_distribution <- function(
  data,
  measure_name,
  population_name,
  documentation_time_hours,
  documentation_time_days,
  group_vars = NULL
) {
  # initialize the empty list ----
  out <- list()

  # perform calculations ----
  # always return the empirical marginal distribution ----
  ## hours
  out_hours <- data |>
    dplyr::summarize(
      time_unit = "Hours",
      measure = measure_name,
      pop = population_name,
      min = min({{ documentation_time_hours }}, na.rm = TRUE),
      q25 = stats::quantile(
        {{ documentation_time_hours }},
        probs = 0.25,
        na.rm = TRUE
      ),
      median = stats::median(
        {{ documentation_time_hours }},
        na.rm = TRUE
      ),
      q75 = stats::quantile(
        {{ documentation_time_hours }},
        probs = 0.75,
        na.rm = TRUE
      ),
      p90 = stats::quantile(
        {{ documentation_time_hours }},
        probs = 0.90,
        na.rm = TRUE
      ),
      max = max({{ documentation_time_hours }}, na.rm = TRUE),
      mean = mean({{ documentation_time_hours }}, na.rm = TRUE),
      sd = stats::sd({{ documentation_time_hours }}, na.rm = TRUE),
      n_time_complete = sum(!is.na({{ documentation_time_hours }})),
      n_time_miss = sum(is.na({{ documentation_time_hours }})),
      n_obs = dplyr::n()
    )

  ## days
  out_days <- data |>
    dplyr::summarize(
      time_unit = "Days",
      measure = measure_name,
      pop = population_name,
      min = min({{ documentation_time_days }}, na.rm = TRUE),
      q25 = stats::quantile(
        {{ documentation_time_days }},
        probs = 0.25,
        na.rm = TRUE
      ),
      median = stats::median(
        {{ documentation_time_days }},
        na.rm = TRUE
      ),
      q75 = stats::quantile(
        {{ documentation_time_days }},
        probs = 0.75,
        na.rm = TRUE
      ),
      p90 = stats::quantile(
        {{ documentation_time_days }},
        probs = 0.90,
        na.rm = TRUE
      ),
      max = max({{ documentation_time_days }}, na.rm = TRUE),
      mean = mean({{ documentation_time_days }}, na.rm = TRUE),
      sd = stats::sd({{ documentation_time_days }}, na.rm = TRUE),
      n_time_complete = sum(!is.na({{ documentation_time_days }})),
      n_time_miss = sum(is.na({{ documentation_time_days }})),
      n_obs = dplyr::n()
    )

  ## union the hours and days analyses ----
  out[["overall"]] <- dplyr::bind_rows(
    out_hours,
    out_days
  )

  # Case when subclass analysis is requested ----
  if (!is.null(group_vars)) {
    ## subclass analysis of hours ----
    out_strat_hours <- data |>
      dplyr::summarize(
        time_unit = "Hours",
        measure = measure_name,
        pop = population_name,
        min = min({{ documentation_time_hours }}, na.rm = TRUE),
        q25 = stats::quantile(
          {{ documentation_time_hours }},
          probs = 0.25,
          na.rm = TRUE
        ),
        median = stats::median(
          {{ documentation_time_hours }},
          na.rm = TRUE
        ),
        q75 = stats::quantile(
          {{ documentation_time_hours }},
          probs = 0.75,
          na.rm = TRUE
        ),
        p90 = stats::quantile(
          {{ documentation_time_hours }},
          probs = 0.90,
          na.rm = TRUE
        ),
        max = max({{ documentation_time_hours }}, na.rm = TRUE),
        mean = mean({{ documentation_time_hours }}, na.rm = TRUE),
        sd = stats::sd({{ documentation_time_hours }}, na.rm = TRUE),
        n_time_complete = sum(!is.na({{ documentation_time_hours }})),
        n_time_miss = sum(is.na({{ documentation_time_hours }})),
        n_obs = dplyr::n(),
        .by = dplyr::all_of(group_vars)
      )

    ## subclass analysis of days
    out_strat_days <- data |>
      dplyr::summarize(
        time_unit = "Days",
        measure = measure_name,
        pop = population_name,
        min = min({{ documentation_time_days }}, na.rm = TRUE),
        q25 = stats::quantile(
          {{ documentation_time_days }},
          probs = 0.25,
          na.rm = TRUE
        ),
        median = stats::median(
          {{ documentation_time_days }},
          na.rm = TRUE
        ),
        q75 = stats::quantile(
          {{ documentation_time_days }},
          probs = 0.75,
          na.rm = TRUE
        ),
        p90 = stats::quantile(
          {{ documentation_time_days }},
          probs = 0.90,
          na.rm = TRUE
        ),
        max = max({{ documentation_time_days }}, na.rm = TRUE),
        mean = mean({{ documentation_time_days }}, na.rm = TRUE),
        sd = stats::sd({{ documentation_time_days }}, na.rm = TRUE),
        n_time_complete = sum(!is.na({{ documentation_time_days }})),
        n_time_miss = sum(is.na({{ documentation_time_days }})),
        n_obs = dplyr::n(),
        .by = dplyr::all_of(group_vars)
      )

    ## union the return of the empirical conditional distribution ----
    out[["stratified"]] <- dplyr::bind_rows(
      out_strat_hours,
      out_strat_days
    )
  }

  # return the list object
  return(out)
}
