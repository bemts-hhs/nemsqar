#' @title Trauma-03 Calculation
#'
#' @description
#'
#' This function calculates the "Trauma-03" measure, which evaluates pain scale
#' reassessment for trauma patients, using a comprehensive data frame with EMS
#' records. The function processes input data to create both fact and dimension
#' tables, identifies eligible patients, and summarizes results for adult and
#' pediatric populations.
#'
#' @param df A data frame or tibble containing EMS data with all relevant
#'   columns. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col The column for patient age numeric value.
#' @param epatient_16_col The column for patient age unit (e.g., "Years",
#'   "Months").
#' @param esituation_02_col The column containing information on the presence of
#'   injury.
#' @param eresponse_05_col The column representing the 911 response type.
#' @param edisposition_28_col The column for patient care disposition details.
#' @param transport_disposition_col The column for patient transport
#'   disposition.
#' @param evitals_01_col The column for the time of pain scale measurement.
#' @param evitals_27_col The column for the full set of pain scale scores.
#' @param evitals_27_initial_col The column for the initial pain scale score.
#' @param evitals_27_last_col The column for the last pain scale score.
#' @param ... Additional arguments passed to helper functions for further
#'   customization.
#'
#' @return A tibble summarizing results for three population groups (All,
#'   Adults, and Peds) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (All, Adults, Peds).
#'   `numerator`: Count of incidents where there was a reduction in patient
#'   pain.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents where there was a reduction in patient
#'   pain.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_03 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      disposition_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      edisposition_28_col,
                      transport_disposition_col,
                      evitals_01_col,
                      evitals_27_col = NULL,
                      evitals_27_initial_col = NULL,
                      evitals_27_last_col = NULL,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("Trauma-03")

    # Header
    cli::cli_h2("Gathering Records for Trauma-03")

    # Gather the population of interest
    trauma_03_populations <- trauma_03_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      vitals_table = vitals_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_27_col = {{ evitals_27_col }},
      evitals_27_initial_col = {{ evitals_27_initial_col }},
      evitals_27_last_col = {{ evitals_27_last_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating Trauma-03")

  # summarize
  trauma.03 <- results_summarize(total_population = trauma_03_populations$initial_population,
                                 adult_population = trauma_03_populations$adults,
                                 peds_population = trauma_03_populations$peds,
                                 measure_name = "Trauma-03",
                                 numerator_col = PAIN_SCALE,
                                 ...
                                 )

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

    } else {
      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(trauma.03)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&
    !is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("Trauma-03")

    # Header
    cli::cli_h2("Gathering Records for Trauma-03")

    # Gather the population of interest
    trauma_03_populations <- trauma_03_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_27_col = {{ evitals_27_col }},
      evitals_27_initial_col = {{ evitals_27_initial_col }},
      evitals_27_last_col = {{ evitals_27_last_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating Trauma-03")

    # summarize
    trauma.03 <- results_summarize(
      total_population = trauma_03_populations$initial_population,
      adult_population = trauma_03_populations$adults,
      peds_population = trauma_03_populations$peds,
      measure_name = "Trauma-03",
      numerator_col = PAIN_SCALE,
      ...
    )

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

    } else {
      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(trauma.03)

  }

}


