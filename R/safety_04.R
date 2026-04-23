#' @title Safety-04 Calculation
#'
#' @description
#'
#' The `safety_04` function processes EMS incident data for specific safety and
#' transport criteria, filtering by patient age and incident type to identify
#' cases that meet specified exclusion or inclusion criteria. This function
#' accommodates data with various EMS-specific codes, age descriptors, and
#' procedure identifiers.
#'
#' @inheritParams airway_01_population
#' @inheritParams safety_02_population
#' @inheritParams safety_04_population
#' @inheritParams airway_01
#'
#' @return A data.frame summarizing results for two population groups (All,
#'   Adults and Peds) with the following columns:
#' - `pop`: Population type (All, Adults, and Peds).
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
#' # Synthetic test data
#'   test_data <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     earrest_01 = rep("No", 5),
#'     einjury_03 = rep("non-injury", 5),
#'     edisposition_14 = rep(4214001, 5),
#'     edisposition_30 = rep(4230001, 5),
#'     eprocedures_03 = rep("other response", 5)
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#'   safety_04(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     incident_date_col = NULL,
#'     patient_DOB_col = NULL,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     earrest_01_col = earrest_01,
#'     einjury_03_col = einjury_03,
#'     edisposition_14_col = edisposition_14,
#'     transport_disposition_cols = edisposition_30,
#'     eprocedures_03_col = eprocedures_03,
#'     confidence_interval = TRUE
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_04 <- function(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  injury_table = NULL,
  procedures_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  earrest_01_col,
  einjury_03_col,
  eprocedures_03_col,
  edisposition_14_col,
  transport_disposition_cols,
  transport_disposition_col = lifecycle::deprecated(),
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
) {
  # Handle deprecated transport_disposition_col argument ----
  if (lifecycle::is_present(transport_disposition_col)) {
    # Issue an error
    lifecycle::deprecate_stop(
      when = "1.2.0",
      what = "safety_04(transport_disposition_col)",
      with = "safety_04(transport_disposition_cols)"
    )
  }

  # Set default method and adjustment method ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # Ensure that not all table arguments AND the df argument are fulfilled ----
  # User must pass either `df` or all table arguments, but not both
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Safety-04")

    # header ----
    cli::cli_h2("Gathering Records for Safety-04")

    # gather the population of interest ----
    safety_04_populations <- safety_04_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      arrest_table = arrest_table,
      injury_table = injury_table,
      procedures_table = procedures_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      earrest_01_col = {{ earrest_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      edisposition_14_col = {{ edisposition_14_col }},
      transport_disposition_cols = {{ transport_disposition_cols }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Safety-04")

    # summary ----
    safety.04 <- results_summarize(
      total_population = NULL,
      adult_population = NULL,
      peds_population = safety_04_populations$peds,
      measure_name = "Safety-04",
      population_names = "peds",
      numerator_col = CAR_SEAT,
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
      any(safety.04$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(safety.04)
  } else if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    ) &&
      !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics ----
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Safety-04")

    # header ----
    cli::cli_h2("Gathering Records for Safety-04")

    # gather the population of interest ----
    safety_04_populations <- safety_04_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      earrest_01_col = {{ earrest_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      edisposition_14_col = {{ edisposition_14_col }},
      transport_disposition_cols = {{ transport_disposition_cols }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Safety-04")

    # summary ----
    safety.04 <- results_summarize(
      total_population = NULL,
      adult_population = NULL,
      peds_population = safety_04_populations$peds,
      measure_name = "Safety-04",
      population_names = "peds",
      numerator_col = CAR_SEAT,
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
      any(safety.04$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(safety.04)
  }
}
