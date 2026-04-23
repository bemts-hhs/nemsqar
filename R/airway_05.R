#' @title Airway-05 Calculation
#'
#' @description
#'
#' Calculates the NEMSQA Airway-05 measure.
#'
#' Calculates the proportion of endotracheal intubation attempts with adequate
#' oxygenation.
#'
#' @inheritParams airway_01_population
#' @inheritParams airway_01
#'
#' @return A data.frame summarizing results for two population groups (Adults
#'   and Peds) with the following columns:
#' - `pop`: Population type (Adults and Peds).
#' - `numerator`: Count of incidents meeting the measure.
#' - `denominator`: Total count of included incidents.
#' - `prop`: Proportion of incidents meeting the measure.
#' - `prop_label`: Proportion formatted as a percentage with a specified number
#'    of decimal places.
#' - `lower_ci`: Lower bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#' - `upper_ci`: Upper bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#'
#' @examples
#'
#' # If you are sourcing your data from a SQL database connection
#' # or if you have your data in several different tables,
#' # you can pass table inputs versus a single data.frame or tibble
#'
#' # create tables to test correct functioning
#'
#'   # patient table
#'   patient_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'     "2025-01-01", "2025-06-01")), 2),
#'     patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'                                 "2023-01-01", "1970-06-01")), 2),
#'     epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
#'     epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2)
#'
#'   )
#'
#'   # response table
#'   response_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eresponse_05 = rep(2205001, 10)
#'
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 22:59:00",
#'     "2025-01-05 11:58:00", "2025-02-01 18:57:00", "2025-01-01 04:58:00",
#'     "2025-06-01 12:57:00", "2025-01-01 23:05:00", "2025-01-05 12:04:00",
#'     "2025-02-01 19:03:00", "2025-01-01 05:02:00", "2025-06-01 13:01:00")),
#'     evitals_12 = rep(c(94, 95, 96, 97, 98), 2)
#'
#'   )
#'
#' # arrest table
#'   arrest_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     earrest_01 = rep("No", 10)
#'   )
#'
#'   # procedures table
#'   procedures_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
#'     "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
#'     "2025-06-01 13:00:00")), 2),
#'     eprocedures_02 = rep("No", 10),
#'     eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
#'                            673005), 2)
#'
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' airway_05(df = NULL,
#'          patient_scene_table = patient_table,
#'          procedures_table = procedures_table,
#'          vitals_table = vitals_table,
#'          arrest_table = arrest_table,
#'          response_table = response_table,
#'          erecord_01_col = erecord_01,
#'          incident_date_col = incident_date,
#'          patient_DOB_col = patient_dob,
#'          epatient_15_col = epatient_15,
#'          epatient_16_col = epatient_16,
#'          eresponse_05_col = eresponse_05,
#'          eprocedures_01_col = eprocedures_01,
#'          eprocedures_02_col = eprocedures_02,
#'          eprocedures_03_col = eprocedures_03,
#'          earrest_01_col = earrest_01,
#'          evitals_01_col = evitals_01,
#'          evitals_12_col = evitals_12,
#'          confidence_interval = TRUE
#'          )
#'
#' @author Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS
#'
#' @export
#'
airway_05 <- function(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  procedures_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  earrest_01_col,
  eresponse_05_col,
  evitals_01_col,
  evitals_12_col,
  eprocedures_01_col,
  eprocedures_02_col,
  eprocedures_03_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
) {
  # Set default method and adjustment method ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # Check for tables or DF ----

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(procedures_table),
      !is.null(vitals_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn airway_05} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(arrest_table),
      is.null(response_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn airway_05} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
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
      missing(earrest_01_col),
      missing(eresponse_05_col),
      missing(evitals_01_col),
      missing(evitals_12_col),
      missing(eprocedures_01_col),
      missing(eprocedures_02_col),
      missing(eprocedures_03_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn airway_05}."
    )
  }

  # utilize applicable tables to analyze the data for the measure ----
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(procedures_table),
      !is.null(vitals_table)
    ) &&
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Airway-05")

    # header ----
    cli::cli_h2("Gathering Records for Airway-05")

    #############################################################################
    #                                                                           #
    #     Get Population Level Information from tables and columns ----         #
    #                                                                           #
    #############################################################################

    airway_05_populations <- airway_05_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      arrest_table = arrest_table,
      procedures_table = procedures_table,
      vitals_table = vitals_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      earrest_01_col = {{ earrest_01_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_12_col = {{ evitals_12_col }},
      eprocedures_01_col = {{ eprocedures_01_col }},
      eprocedures_02_col = {{ eprocedures_02_col }},
      eprocedures_03_col = {{ eprocedures_03_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Airway-05")

    # summary ----
    airway.05 <- results_summarize(
      total_population = NULL,
      adult_population = airway_05_populations$adults,
      peds_population = airway_05_populations$peds,
      measure_name = "Airway-05",
      population_names = c("adults", "peds"),
      numerator_col = numerator,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2) # Convert to minutes and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 'm'))}."
      )
    } else {
      run_time <- round(run_time_secs, 2) # Keep in seconds and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 's'))}."
      )
    }

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(airway.05$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(airway.05)
  } else if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(arrest_table)
    ) &&
      !is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Airway-05")

    # header ----
    cli::cli_h2("Gathering Records for Airway-05")

    #############################################################################
    #                                                                           #
    #     Get Population Level Information from tables and columns ----         #
    #                                                                           #
    #############################################################################

    airway_05_populations <- airway_05_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      earrest_01_col = {{ earrest_01_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_12_col = {{ evitals_12_col }},
      eprocedures_01_col = {{ eprocedures_01_col }},
      eprocedures_02_col = {{ eprocedures_02_col }},
      eprocedures_03_col = {{ eprocedures_03_col }}
    )

    # create a separator <-
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Airway-05")

    # summary ----
    airway.05 <- results_summarize(
      adult_population = airway_05_populations$adults,
      peds_population = airway_05_populations$peds,
      measure_name = "Airway-05",
      population_names = c("adults", "peds"),
      numerator_col = numerator,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2) # Convert to minutes and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 'm'))}."
      )
    } else {
      run_time <- round(run_time_secs, 2) # Keep in seconds and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 's'))}."
      )
    }

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(airway.05$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(airway.05)
  }
}
