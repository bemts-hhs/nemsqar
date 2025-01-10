#' Trauma-01 Calculation
#'
#' This function processes EMS data to calculate the Trauma-01 performance measure, 
#' which evaluates the percentage of trauma patients assessed for pain using a numeric scale. 
#' The function filters and summarizes the data based on specified inclusion criteria.
#' 
#' @section Data Assumptions:
#' 
#' - `evitals_23_col` is the highest GCS total score or all GCS total scores.
#' - `evitals_26_col` contains all the AVPU responses for all records.
#' - `evitals_27_col` is the initial pain scale score or all pain scale scores.
#'
#' @param df A data frame or tibble containing EMS records. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data.frame or tibble containing only the edisposition fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name representing the EMS record ID.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name for the incident date. Default is `NULL`.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name for the patient's date of birth. Default is `NULL`.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name for the patient's age in numeric format.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for the unit of age (e.g., "Years", "Months").
#' @param esituation_02_col <['tidy-select'][dplyr_tidy_select]> Column name indicating if the situation involved an injury.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for the type of EMS response (e.g., 911 call).
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> Column name for the Glasgow Coma Scale (GCS) total score.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> Column name for AVPU (Alert, Voice, Pain, Unresponsive) status.
#' @param evitals_27_col <['tidy-select'][dplyr_tidy_select]> Column name for the pain scale assessment.
#' @param edisposition_28_col <['tidy-select'][dplyr_tidy_select]> Column name for patient care disposition details.
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> Column name for transport disposition details.
#' @param ... Additional arguments passed to the `summarize_measure` function for custom summarization.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where a pain scale was administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where a pain scale was administered.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_01 <- function(df = NULL,
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
                      evitals_23_col,
                      evitals_26_col,
                      evitals_27_col,
                      edisposition_28_col,
                      transport_disposition_col,
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
    cli::cli_h1("Trauma-01")
    
    # Header
    cli::cli_h2("Gathering Records for Trauma-01")
    
    # Gather the population of interest
    trauma_01_populations <- trauma_01_population(
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
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }},
      evitals_27_col = {{ evitals_27_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating Trauma-01")
    
    # summarize
    trauma.01 <- results_summarize(
      total_population = trauma_01_populations$initial_population,
      adult_population = trauma_01_populations$adults,
      peds_population = trauma_01_populations$peds,
      measure_name = "Trauma-01",
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
    
    return(trauma.01)
    
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
    cli::cli_h1("Trauma-01")
    
    # Header
    cli::cli_h2("Gathering Records for Trauma-01")
    
    # Gather the population of interest
    trauma_01_populations <- trauma_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }},
      evitals_27_col = {{ evitals_27_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating Trauma-01")
    
    # summarize
    trauma.01 <- results_summarize(
      total_population = trauma_01_populations$initial_population,
      adult_population = trauma_01_populations$adults,
      peds_population = trauma_01_populations$peds,
      measure_name = "Trauma-01",
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
    
    return(trauma.01)
    
  }
  
}
