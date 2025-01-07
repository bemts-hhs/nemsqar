#' Syncope-01 Calculation
#' 
#' The `syncope_01` function processes EMS dataset to identify potential syncope (fainting) cases
#' based on specific criteria and calculates related ECG measures. This function dplyr::filters data for 
#' 911 response calls, assesses primary and associated symptoms for syncope, determines age-based 
#' populations (adult and pediatric), and aggregates results by unique patient encounters.
#' 
#' @section Data Assumptions:
#' 
#' This function assumes that:
#' 
#' * Data are loaded into a data frame or tibble where each row represents one observation (e.g., patient)
#' and each column is a distinct feature (field).
#' * Missing values in rows are represented as `NA`. Any "Not Known" or "Not Recorded" values commonly used 
#' in EMS datasets should be pre-cleaned before use.
#' * The `eresponse_05_col` column contains NEMSIS response codes, specifically for identifying 911 responses.
#' * Symptom and impression columns (`esituation_09_col`, `esituation_10_col`, `esituation_11_col`, and `esituation_12_col`) 
#' should contain relevant codes or text descriptions that identify syncope and related conditions.  The secondary and associated symptoms
#' columns need to be columns that contain all values in a comma separated format with all responses in one cell for a unique incident.
#' These can also be a list column that is unnested before passing to the function.   
#' * Patient ECG results, if present, are found in `evitals_04_col`. Values for this column need to contain all values in a comma
#' separated format with all responses in one cell for a unique incident. This can also be a list column that is unnested before passing to the 
#' function. 
#' * Age information is captured across columns `epatient_15_col` and `epatient_16_col` to distinguish between adults 
#' and pediatric cases.
#' * Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#' 
#' The first argument is the main data frame (`df`). No joins are performed; any necessary joins 
#' should be completed prior to calling this function.
#' 
#' @section Practical Tips:
#' 
#' Ensure data are pre-processed with any missing values coded as `NA`. Additionally, ensure that date fields 
#' (e.g., `incident_date_col`, `patient_DOB_col`) are of type `Date` or `POSIXct`, as incorrect formatting will 
#' trigger an error message.
#' 
#' @param df <['tidy-select'][dplyr_tidy_select]> Main data frame containing EMS records.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column containing the incident date, used to calculate age.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column containing the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient age (numeric).
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column for the patient age units (e.g., "Years", "Months").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing response type codes, specifically 911 codes.
#' @param esituation_09_col <['tidy-select'][dplyr_tidy_select]> Column with primary symptoms associated with the patient encounter.
#' @param esituation_10_col <['tidy-select'][dplyr_tidy_select]> Column with other associated symptoms.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column for primary impression code.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column for secondary impression codes.
#' @param evitals_04_col <['tidy-select'][dplyr_tidy_select]> Column with ECG information if available.
#' @param ... Additional arguments passed to `dplyr::summarize` for grouped summaries.
#'
#' @return A tibble summarizing results for three population groups (Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (Adults, Peds).
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
syncope_01 <- function(df = NULL,
                       patient_scene_table = NULL,
                       response_table = NULL,
                       situation_table = NULL,
                       vitals_table = NULL,
                       erecord_01_col,
                       incident_date_col = NULL,
                       patient_DOB_col = NULL,
                       epatient_15_col,
                       epatient_16_col,
                       eresponse_05_col,
                       esituation_09_col,
                       esituation_10_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_04_col,
                       ...) {
  
  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    
    is.null(df)
    
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Syncope-01")
    
    # header
    cli::cli_h2("Gathering Records for Syncope-01")
    
    # gather the population of interest
    syncope_01_populations <- syncope_01_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    vitals_table = vitals_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_09_col = {{ esituation_09_col }},
                                                    esituation_10_col = {{ esituation_10_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_04_col = {{ evitals_04_col }}
                                                    )
    
  # create a separator
  cli::cli_text("\n")
    
  # header for calculations
  cli::cli_h2("Calculating Syncope-01")
    
  # summarize
  
  # adults
  adult_population <- syncope_01_populations$adults |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Adult",
                      ECG_PERFORMED,
                      ...)
  
  # peds
  peds_population <- syncope_01_populations$peds |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Peds",
                      ECG_PERFORMED,
                      ...)
  
  # summary
  syncope.01 <- dplyr::bind_rows(adult_population, peds_population)

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
  
  return(syncope.01)
  
  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    
    !is.null(df)
    
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Syncope-01")
    
    # header
    cli::cli_h2("Gathering Records for Syncope-01")
    
    # gather the population of interest
    syncope_01_populations <- syncope_01_population(df = df,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_09_col = {{ esituation_09_col }},
                                                    esituation_10_col = {{ esituation_10_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_04_col = {{ evitals_04_col }}
                                                    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Syncope-01")
    
    # summarize
    
    # adults
    adult_population <- syncope_01_populations$adults |>
      summarize_measure(measure_name = "Syncope-01",
                        population_name = "Adult",
                        ECG_PERFORMED,
                        ...)
    
    # peds
    peds_population <- syncope_01_populations$peds |>
      summarize_measure(measure_name = "Syncope-01",
                        population_name = "Peds",
                        ECG_PERFORMED,
                        ...)
    
    # summary
    syncope.01 <- dplyr::bind_rows(adult_population, peds_population)
    
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
    
    return(syncope.01)
    
  }

}
