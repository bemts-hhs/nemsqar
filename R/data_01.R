#' @title Data-01 Calculation
#'
#' @description
#'
#' The `data_01()` function summarizes EMS registry record validity scores.
#' Validity scores are assumed to be pre-calculated by an EMS software vendor
#' or EMS repository and supplied at the record level. Validity scores must be
#' numeric values bounded to the interval `[0, 1]`, where larger values
#' indicate greater record validity. Validity scores typically represent the
#' proportion of data elements within a patient care record that satisfy
#' registry-defined validation requirements, including field completeness,
#' allowable values, formatting rules, and other data quality checks.
#'
#' The function calculates validity score distribution statistics and validity
#' score range frequencies for all patients, adult patients, and pediatric
#' patients. Optional subgroup analyses may be performed using variables
#' supplied through `group_vars`.
#'
#' Data-01 is not an official NEMSQA measure. It was developed to support EMS
#' registry data quality assessment and quality improvement initiatives related
#' to record validity and completeness.
#'
#' @inheritParams data_01_population
#' @inheritParams airway_01
#' @param group_vars Optional character vector of column names used to perform
#'   subgroup analyses.
#'
#' @return A named list with two primary elements:
#'
#' - `distribution`: Validity score distribution summaries.
#' - `counts`: Validity score range counts and proportions.
#'
#' Both elements contain:
#'
#' - `overall`: Results for the selected populations (`All`, `Adults`,
#'   and `Peds`).
#' - `stratified`: Results stratified by `group_vars`
#'   (when requested).
#'
#' Population-specific results are combined within each output and identified
#' by the `pop` variable.
#'
#' Distribution summaries include:
#'
#' - Number of observations.
#' - Number of missing validity scores.
#' - Minimum, maximum, mean, and standard deviation.
#' - 25th percentile, median, 75th percentile, and 90th percentile.
#'
#' Count summaries include:
#'
#' - Validity score range.
#' - Count (`numerator`).
#' - Population total (`denominator`).
#' - Proportion (`prop`).
#' - Confidence intervals when requested.
#'
#' @examples
#'
#' # Synthetic example data
#' test_data <- tibble::tibble(
#'   erecord_01 = paste0("R", 1:10),
#'   incident_date = as.Date("2024-01-01"),
#'   patient_dob = as.Date(c(
#'     "1980-01-01", "1975-01-01", "1990-01-01", "1985-01-01",
#'     "1970-01-01", "2015-01-01", "2012-01-01", "2010-01-01",
#'     "2018-01-01", "2016-01-01"
#'   )),
#'   epatient_15 = c(44, 49, 34, 39, 54, 9, 12, 14, 6, 8),
#'   epatient_16 = rep("Years", 10),
#'   patient_disposition = rep(
#'     "Patient Treated, Transported by this EMS Unit (4212033)",
#'     10
#'   ),
#'   incident_validity_score = c(
#'     1.00, 0.98, 0.96, 0.92, 0.89,
#'     0.84, 0.77, 0.65, 0.54, 0.41
#'   )
#' )
#'
#' # Run the Data-01 validity score analysis
#' test_data |>
#' data_01(
#'   df = _,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   patient_care_col = patient_disposition,
#'   validity_score_col = incident_validity_score
#' )
#'
#' # Stratified analysis
#' test_data$vendor <- rep(c("Vendor A", "Vendor B"), each = 5)
#'
#' test_data |>
#' data_01(
#'   df = _,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   patient_care_col = patient_disposition,
#'   validity_score_col = incident_validity_score,
#'   group_vars = "vendor"
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
data_01 <- function(
  df = NULL,
  patient_scene_table = NULL,
  audit_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  patient_care_col,
  validity_score_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  group_vars = NULL
) {
  # Set default method and adjustment method ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # Check for tables or DF ----

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_01} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_01} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
    )
  }

  # ensure all *_col arguments are fulfilled ----
  if (
    any(
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(patient_care_col),
      missing(validity_score_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn data_01}."
    )
  }

  # User must pass either `df` or all table arguments, but not both
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Data-01")

    # header ----
    cli::cli_h2("Gathering Records for Data-01")

    # gather the population of interest ----
    data_01_populations <- data_01_population(
      patient_scene_table = patient_scene_table,
      audit_table = audit_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      patient_care_col = {{ patient_care_col }},
      validity_score_col = {{ validity_score_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Data-01")

    # summary ----
    data.01 <- results_validity_summarize(
      total_population = data_01_populations$initial_population,
      adult_population = data_01_populations$adults,
      peds_population = data_01_populations$peds,
      population_names = c("all", "adults", "peds"),
      measure_name = "Data-01",
      validity_score_col = {{ validity_score_col }},
      validity_score_range_col = VALIDITY_SCORE_RANGE_FORMAT,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      group_vars = group_vars
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----

    # end time
    end_time <- Sys.time()

    # raw runtime from difftime()
    runtime_raw <- difftime(end_time, start_time, units = "auto")

    # rounded numeric value from difftime()
    runtime <- runtime_raw |> as.numeric() |> round(digits = 5) # large `digits` number to avoid bad rounding

    # runtime unit from attr()
    runtime_unit <- attr(x = runtime_raw, which = "units") # <- will return "secs", "mins", "hours", etc.

    # issue a dynamic message to the console
    cli::cli_alert_success(
      "Function completed in {cli::col_green(paste(runtime, runtime_unit))}."
    )

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(data.01$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(data.01)
  } else if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table)
    ) &&
      !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics ----
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Data-01")

    # header ----
    cli::cli_h2("Gathering Records for Data-01")

    # gather the population of interest ----
    data_01_populations <- data_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      patient_care_col = {{ patient_care_col }},
      validity_score_col = {{ validity_score_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Data-01")

    # summary ----
    data.01 <- results_validity_summarize(
      total_population = data_01_populations$initial_population,
      adult_population = data_01_populations$adults,
      peds_population = data_01_populations$peds,
      population_names = c("all", "adults", "peds"),
      measure_name = "Data-01",
      validity_score_col = {{ validity_score_col }},
      validity_score_range_col = VALIDITY_SCORE_RANGE_FORMAT,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      group_vars = group_vars
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----

    # end time
    end_time <- Sys.time()

    # raw runtime from difftime()
    runtime_raw <- difftime(end_time, start_time, units = "auto")

    # rounded numeric value from difftime()
    runtime <- runtime_raw |> as.numeric() |> round(digits = 5) # large `digits` number to avoid bad rounding

    # runtime unit from attr()
    runtime_unit <- attr(x = runtime_raw, which = "units") # <- will return "secs", "mins", "hours", etc.

    # issue a dynamic message to the console
    cli::cli_alert_success(
      "Function completed in {cli::col_green(paste(runtime, runtime_unit))}."
    )

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(data.01$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(data.01)
  }
}
