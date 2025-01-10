#' Trauma-08 Calculation
#'
#' This function calculates the Trauma-08 measure, which evaluates the completeness 
#' of pain scale documentation for patients experiencing traumatic injury. It determines 
#' the total population, adult population, and pediatric population meeting the criteria 
#' for the Trauma-08 measure.
#' 
#' @section Data Assumptions:
#' 
#' - All vital signs are the initial vital signs.
#' - This function accepts the input dataset contains the *initial* vital signs for respiratory rate, systolic blood pressure (SBP), 
#' and total Glasgow Coma Scale (GCS) score, respectively. If those values are not available, the user may pass all those values
#' from the respective columns and the function will find the initial values.
#'
#' @param df A data frame or tibble containing EMS data with all relevant columns. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data frame or tibble containing only the esituation fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the edisposition fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> A column specifying unique patient records.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> A column indicating the incident date. Must be of class `Date` or `POSIXct`. Default is `NULL`.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient's date of birth. Must be of class `Date` or `POSIXct`. Default is `NULL`.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient’s age in numeric form.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> A column specifying the unit of patient age (e.g., "Years", "Days").
#' @param esituation_02_col <['tidy-select'][dplyr_tidy_select]> A column containing information about the nature of the patient’s condition (e.g., injury type).
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> A column specifying the type of response (e.g., 911 codes).
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> A column specifying transport disposition for the patient.
#' @param evitals_06_col <['tidy-select'][dplyr_tidy_select]> A column containing systolic blood pressure (SBP) data from initial vital signs.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> A column containing respiratory rate data from initial vital signs.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> A column containing total Glasgow Coma Scale (GCS) scores from initial vital signs.
#' @param ... Additional arguments passed to the `summarize_measure` function.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where the respiratory rate, SBP, and GCS vitals were taken.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where the respiratory rate, SBP, and GCS vitals were taken.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
trauma_08 <- function(df = NULL,
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
                      transport_disposition_col,
                      evitals_06_col,
                      evitals_14_col,
                      evitals_23_col,
                      ...) {
  
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
    cli::cli_h1("Trauma-08")
    
    # Header
    cli::cli_h2("Gathering Records for Trauma-08")
    
    # Gather the population of interest
    trauma_08_populations <- trauma_08_population(
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
      evitals_06_col = {{ evitals_06_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating Trauma-08")
    
    # summarize
    
    # adults
    adult_population <- trauma_08_populations$adults |>
      summarize_measure(measure_name = "Trauma-08",
                        population_name = "Adult",
                        VITALS,
                        ...)
    
    # peds
    peds_population <- trauma_08_populations$peds |>
      summarize_measure(measure_name = "Trauma-08",
                        population_name = "Peds",
                        VITALS,
                        ...) 
    
    # bind rows
    trauma.08 <- bind_rows(adult_population, peds_population)
    
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
    
    return(trauma.08)
    
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
    cli::cli_h1("Trauma-08")
    
    # Header
    cli::cli_h2("Gathering Records for Trauma-08")
    
    # Gather the population of interest
    trauma_08_populations <- trauma_08_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating Trauma-08")
    
    # summarize
    
    # adults
    adult_population <- trauma_08_populations$adults |>
      summarize_measure(measure_name = "Trauma-08",
                        population_name = "Adult",
                        VITALS,
                        ...)
    
    # peds
    peds_population <- trauma_08_populations$peds |>
      summarize_measure(measure_name = "Trauma-08",
                        population_name = "Peds",
                        VITALS,
                        ...) 
    
    # bind rows
    trauma.08 <- bind_rows(adult_population, peds_population)
    
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
    
    return(trauma.08)
    
  }
  
  
}
