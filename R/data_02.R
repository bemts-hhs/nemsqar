#' @title Data-02 Calculation
#'
#' @description
#'
#' Calculates documentation timeliness summaries for Data-02.
#'
#' Data-02 evaluates the elapsed time between a user-defined event at which
#' patient care documentation could reasonably begin and a user-defined event
#' representing documentation occurrence. The function produces descriptive
#' summaries of documentation timeliness and counts of records within
#' predefined documentation time bands.
#'
#' Results are generated for all patients, adult patients, and pediatric
#' patients. Optional subgroup analyses may be performed using variables
#' supplied through `group_vars`.
#'
#' @inheritParams data_01_population
#' @inheritParams data_02_population
#' @inheritParams airway_01
#' @inheritParams data_01
#'
#' @return A named list with two primary elements:
#'
#' - `distribution`: Documentation time distribution summaries.
#' - `counts`: Documentation time band counts and proportions.
#'
#' Both elements contain:
#'
#' - `overall`: Results for the selected populations (`All`, `Adults`,
#'   and `Peds`).
#' - `stratified`: Results stratified by `group_vars`
#'   (when requested).
#'
#' Population-specific results are combined within each output and identified
#' by the `pop` variable. Distribution summaries include separate rows for
#' documentation time expressed in hours and days, identified by
#' `time_unit`.
#'
#' Distribution summaries include separate results for documentation
#' time expressed in hours and days, identified by the `time_unit`
#' variable.
#'
#' Distribution summaries include:
#'
#' - `time_unit`: The unit of measurement used for the documentation
#'   timeliness summary (`Hours` or `Days`).
#' - Number of observations.
#' - Number of missing documentation times.
#' - Minimum, maximum, mean, and standard deviation.
#' - Percentiles: 25, median, 75, and 90.
#'
#' Count summaries include:
#'
#' - Documentation time band.
#' - Count (`numerator`).
#' - Population total (`denominator`).
#' - Proportion (`prop`).
#' - Confidence intervals when requested.
#'
#' @examples
#'
#' # Synthetic Data-02 example
#' test_data <- tibble::tibble(
#'   erecord_01 = paste0("R", 1:10),
#'
#'   # Incident and demographic variables
#'   incident_date = as.Date("2024-01-01"),
#'   patient_dob = as.Date(c(
#'     "1980-01-01", "1975-01-01", "1990-01-01", "1985-01-01",
#'     "1970-01-01", "2015-01-01", "2012-01-01", "2010-01-01",
#'     "2018-01-01", "2016-01-01"
#'   )),
#'   epatient_15 = c(44, 49, 34, 39, 54, 9, 12, 14, 6, 8),
#'   epatient_16 = rep("Years", 10),
#'
#'   # Patient contact disposition
#'   patient_disposition = rep(
#'     "Patient Treated, Transported by this EMS Unit (4212033)",
#'     10
#'   ),
#'
#'   # Time documentation became available
#'   unit_back_in_service = as.POSIXct(
#'     "2024-01-01 08:00:00",
#'     tz = "UTC"
#'   ) + c(
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0
#'   ) * 86400,
#'
#'   # Time documentation occurred
#'   incident_created = as.POSIXct(
#'     c(
#'       "2024-01-01 11:00:00",  # 3 hours
#'       "2024-01-01 18:00:00",  # 10 hours
#'       "2024-01-02 04:00:00",  # 20 hours
#'       "2024-01-02 12:00:00",  # 28 hours
#'       "2024-01-03 12:00:00",  # 52 hours
#'       "2024-01-05 12:00:00",  # 4 days
#'       "2024-01-10 12:00:00",  # 9 days
#'       "2024-01-20 12:00:00",  # 19 days
#'       "2024-02-15 12:00:00",  # 45 days
#'       "2024-04-15 12:00:00"   # >60 days
#'     ),
#'     tz = "UTC"
#'   )
#' )
#'
#' # Calculate Data-02 timeliness summaries
#' data_02(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   patient_care_col = patient_disposition,
#'   time_available_to_document_col = unit_back_in_service,
#'   time_documentation_occurred_col = incident_created
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
data_02 <- function(
  df = NULL,
  patient_scene_table = NULL,
  audit_table = NULL,
  disposition_table = NULL,
  times_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  patient_care_col,
  time_available_to_document_col,
  time_documentation_occurred_col,
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
      !is.null(disposition_table),
      !is.null(times_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_02} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table),
      is.null(times_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_02} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
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
      missing(time_available_to_document_col),
      missing(time_documentation_occurred_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn data_02}."
    )
  }

  # User must pass either `df` or all table arguments, but not both
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table),
      !is.null(times_table)
    ) &&
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Data-02")

    # header ----
    cli::cli_h2("Gathering Records for Data-02")

    # gather the population of interest ----
    data_02_populations <- data_02_population(
      patient_scene_table = patient_scene_table,
      audit_table = audit_table,
      disposition_table = disposition_table,
      times_table = times_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      patient_care_col = {{ patient_care_col }},
      time_available_to_document_col = {{ time_available_to_document_col }},
      time_documentation_occurred_col = {{ time_documentation_occurred_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Data-02")

    # summary ----
    data.02 <- results_time_summarize(
      total_population = data_02_populations$initial_population,
      adult_population = data_02_populations$adults,
      peds_population = data_02_populations$peds,
      population_names = c("all", "adults", "peds"),
      measure_name = "Data-02",
      documentation_time_hours = RECORD_COMPLETION_HOURS,
      documentation_time_days = RECORD_COMPLETION_DAYS,
      documentation_time_bands_col = RECORD_COMPLETION_DAYS_LABEL,
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

    ## check for small n in the overall object ----
    small_n_overall <- any(
      data.02$counts$overall$denominator < 10,
      na.rm = TRUE
    )

    ## initialize small_n_stratified in case group_vars is null ----
    small_n_stratified <- FALSE

    ## optionally check for small n in the stratified object ----
    if (!is.null(group_vars)) {
      small_n_stratified <- any(
        data.02$counts$stratified$denominator < 10,
        na.rm = TRUE
      )
    }

    if (
      confidence_interval &&
        method == "wilson" &&
        (small_n_overall || small_n_stratified)
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(data.02)
  } else if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table),
      is.null(times_table)
    ) &&
      !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics ----
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Data-02")

    # header ----
    cli::cli_h2("Gathering Records for Data-02")

    # gather the population of interest ----
    data_02_populations <- data_02_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      patient_care_col = {{ patient_care_col }},
      time_available_to_document_col = {{ time_available_to_document_col }},
      time_documentation_occurred_col = {{ time_documentation_occurred_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Data-02")

    # summary ----
    data.02 <- results_time_summarize(
      total_population = data_02_populations$initial_population,
      adult_population = data_02_populations$adults,
      peds_population = data_02_populations$peds,
      population_names = c("all", "adults", "peds"),
      measure_name = "Data-02",
      documentation_time_hours = RECORD_COMPLETION_HOURS,
      documentation_time_days = RECORD_COMPLETION_DAYS,
      documentation_time_bands_col = RECORD_COMPLETION_DAYS_LABEL,
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

    ## check for small n in the overall object ----
    small_n_overall <- any(
      data.02$counts$overall$denominator < 10,
      na.rm = TRUE
    )

    ## initialize small_n_stratified in case group_vars is null ----
    small_n_stratified <- FALSE

    ## optionally check for small n in the stratified object ----
    if (!is.null(group_vars)) {
      small_n_stratified <- any(
        data.02$counts$stratified$denominator < 10,
        na.rm = TRUE
      )
    }

    if (
      confidence_interval &&
        method == "wilson" &&
        (small_n_overall || small_n_stratified)
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(data.02)
  }
}
