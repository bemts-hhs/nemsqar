#' Seizure-02 Calculation
#'
#' Calculates the NEMSQA Seizure-02 Measure.
#' 
#' Calculates age-based seizure metrics for a dataset. This function filters data 
#' for patients based on incident information, diagnoses, and administered medications 
#' to assess adherence to Seizure-02 metrics.
#' 
#' @section Data Assumptions:
#'
#' This function assumes that:
#'
#' Data are already loaded in a data frame or tibble where each row represents
#' one observation (e.g., patient), and each column is a feature (field).
#' Alternatively, data can consist of distinct datasets referenced as unique columns.
#'
#' Age in years will be calculated using the patient's date of birth and the
#' incident date. These fields must be valid `Date` or `POSIXct` data types.
#'
#' Any missing values in rows are coded as `NA` rather than ImageTrend-specific
#' "not known" or "not recorded" values, or other codes that correspond to
#' unspecified values.
#'
#' The primary and secondary impression fields contain ICD-10 codes, although
#' descriptive text may also be present for reference. These fields are
#' identified as `eSituation.11` for the primary impression and `eSituation.12`
#' for secondary impressions.
#'
#' The `eResponse.05` column contains NEMSIS response codes. Additional descriptive
#' text may also be present in this field for reference.
#'
#' The `eMedications.03` column holds all medications administered to a patient
#' during an event, represented as a list column or comma-separated values. 
#' Each entry should use generic medication names, and while RxNORM codes can 
#' be included, they will not be validated by the function.
#'
#' The `eSituation.12` (secondary impressions) field is best prepared as a 
#' comma-separated list within a single string or as a list column containing
#' each secondary impression.
#'
#' The first argument to the function is a data frame; any joins needed to
#' incorporate additional data (e.g., vitals) must be conducted prior to calling 
#' this function.
#'
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#' 
#' @section Practical Tips:
#' 
#' The first argument is the data.frame prepared as above. No joining is done.
#' Any joins to get specific data elements will need to be done outside of this function.
#' 
#' @section Value:
#' A summarized tibble with counts and proportions of benzodiazepine treatment
#' among 911 seizure calls, segmented by population group.
#'
#' @param df A data frame where each row is an observation, containing all necessary 
#' columns for analysis.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name representing the incident date as `Date` or `POSIXct.`
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name representing the patient's date of birth as `Date` or `POSIXct.`
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name for patient age in numeric form.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for age unit (e.g., `"Years"` or `"Months"`).
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for response codes; "911" call codes are filtered.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column name for primary impressions.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column name for secondary impressions.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column name for medications administered; ideally a list column 
#' or string with comma-separated values.
#' @param ... Additional arguments passed to `dplyr::summarize`.
#'
#' @return A tibble summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
seizure_02 <- function(df = NULL,
                       patient_scene_table = NULL,
                       response_table = NULL,
                       situation_table = NULL,
                       medications_table = NULL,
                       erecord_01_col,
                       incident_date_col = NULL,
                       patient_DOB_col = NULL,
                       epatient_15_col,
                       epatient_16_col,
                       eresponse_05_col,
                       esituation_11_col,
                       esituation_12_col,
                       emedications_03_col,
                       ...) {

  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(medications_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    is.null(df)
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Seizure-02")
    
    # header
    cli::cli_h2("Gathering Records for Seizure-02")
    
    # gather the population of interest
    seizure_02_populations <- seizure_02_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    medications_table = medications_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    emedications_03_col = {{ emedications_03_col }}
                                                    )
    
  # create a separator
  cli::cli_text("\n")
    
  # header for calculations
  cli::cli_h2("Calculating Seizure-02")
    
  # summarize
  seizure.02 <- results_summarize(total_population = seizure_02_populations$initial_population,
                                  adult_population = seizure_02_populations$adults,
                                  peds_population = seizure_02_populations$peds,
                                  measure_name = "Seizure-02",
                                  numerator_col = BENZO_MED,
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
  
  return(seizure.02)
  
  } else if (
    any(
      is.null(patient_scene_table),
      is.null(medications_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Seizure-02")
    
    # header
    cli::cli_h2("Gathering Records for Seizure-02")
    
    # gather the population of interest
    seizure_02_populations <- seizure_02_population(df = df,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    emedications_03_col = {{ emedications_03_col }}
                                                    )
    
  # create a separator
  cli::cli_text("\n")
    
  # header for calculations
  cli::cli_h2("Calculating Seizure-02")
    
  # summarize
  seizure.02 <- results_summarize(total_population = seizure_02_populations$initial_population,
                                  adult_population = seizure_02_populations$adults,
                                  peds_population = seizure_02_populations$peds,
                                  measure_name = "Seizure-02",
                                  numerator_col = BENZO_MED,
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
  
  return(seizure.02)

    
  }
  
}
