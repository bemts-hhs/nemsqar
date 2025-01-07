#' Stroke-01 Calculation
#' 
#' The `stroke_01` function processes EMS dataset to identify potential stroke cases based on specific criteria
#' and calculates the stroke scale measures. It filters the data for 911 response calls, identifies stroke-related
#' impressions and scales, and aggregates results by unique patient encounters.
#' 
#' @section Data Assumptions:
#' 
#' This function assumes that:
#' 
#' Data are already loaded into a data frame or tibble where each row represents
#' one observation (e.g., patient) and each column is a dplyr::distinct feature (field).
#' Alternatively, data may consist of separate datasets referenced by unique columns.
#'  
#' Missing values in rows are represented as `NA`. "Not known" or "not recorded" values,
#' common to ImageTrend or other non-numeric codes, should be pre-cleaned before use.
#' 
#' The `eresponse_05_col` contains NEMSIS response codes and may also include
#' descriptive text for reference.
#' 
#' The vital signs columns `evitals.23`, `evitals.26`, `evitals.29`, and `evitals.30` can contain
#' all the unique values entered per incident, which may cause row explosion.  The function will
#' handle this issue elegantly under the hood.
#'  
#' The `esituation.11` contains single responses per response, but `esituation.12` should be a list 
#' column or text field with all relevant secondary provider impression entries provided in a single cell 
#' as comma-separated values for each unique incident.
#' 
#' The first argument to this function is the main data frame. No joins are performed within
#' the function; any necessary joins (e.g., to incorporate vitals or additional fields) should
#' be completed prior to calling this function.
#' 
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#' 
#' @section Practical Tips:
#' 
#' Ensure data are pre-processed, with missing values coded as `NA`, before passing
#' into the function.
#' Prepare necessary joins (e.g., for vitals) in advance; this function does not perform joins.
#' 
#' @param df <['tidy-select'][dplyr_tidy_select]> A data frame or tibble containing the dataset. Each row should represent a unique patient encounter.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> The column containing the date and time of the incident. This must be a `Date` or `POSIXct` type.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> The column containing the patient's date of birth, formatted as `Date` or `POSIXct`.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> The column containing EMS response codes, which should include 911 response codes.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> The column containing the primary impression codes or descriptions related to the situation.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> The column containing secondary impression codes or descriptions related to the situation.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> The column containing the Glasgow Coma Scale (GCS) score.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> The column containing the AVPU (alert, verbal, pain, unresponsive) scale value.
#' @param evitals_29_col <['tidy-select'][dplyr_tidy_select]> The column containing the stroke scale score achieved during assessment.
#' @param evitals_30_col <['tidy-select'][dplyr_tidy_select]> The column containing stroke scale type descriptors (e.g., FAST, NIH, etc.).
#' @param ... Additional arguments passed to `dplyr::summarize()` function for further customization of results.
#' 
#' @section Features:
#' * **Stroke Case Identification**: Identifies potential stroke cases based on primary and secondary impression codes.
#' * **911 Response Filtering**: Filters records to include only those flagged with 911 response codes in `eresponse_05_col`.
#' * **Glasgow Coma Scale and AVPU Processing**: Analyzes and includes Glasgow Coma Scale (GCS) scores and AVPU levels from vital signs.
#' * **Stroke Scale Type and Values Assessment**: Extracts and processes stroke scale information (e.g., FAST, NIH) from the specified columns.
#' * **Encounter Aggregation**: Aggregates data by unique patient encounters to provide a summary view of stroke case proportions.
#'         
#' @return A tibble summarizing results for the total population with the following columns:
#' `pop`: Population type (All).
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
stroke_01 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      evitals_23_col,
                      evitals_26_col,
                      evitals_29_col,
                      evitals_30_col,
                      ...) {
  
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
    cli::cli_h1("Stroke-01")
    
    # header
    cli::cli_h2("Gathering Records for Stroke-01")
    
    # gather the population of interest
    stroke_01_populations <- stroke_01_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    vitals_table = vitals_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_23_col = {{ evitals_23_col }},
                                                    evitals_26_col = {{ evitals_26_col }},
                                                    evitals_29_col = {{ evitals_29_col }},
                                                    evitals_30_col = {{ evitals_30_col }}
                                                    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Stroke-01")
    
    # summarize
    stroke.01 <- stroke_01_populations$initial_population |> 
      summarize_measure(measure_name = "Stroke-01",
                        population_name = "All",
                        numerator_col = STROKE_SCALE,
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
    
    return(stroke.01)
    
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
    cli::cli_h1("Stroke-01")
    
    # header
    cli::cli_h2("Gathering Records for Stroke-01")
    
    # gather the population of interest
    stroke_01_populations <- stroke_01_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  evitals_23_col = {{ evitals_23_col }},
                                                  evitals_26_col = {{ evitals_26_col }},
                                                  evitals_29_col = {{ evitals_29_col }},
                                                  evitals_30_col = {{ evitals_30_col }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Stroke-01")
    
    # summarize
    stroke.01 <- stroke_01_populations$initial_population |> 
      summarize_measure(measure_name = "Stroke-01",
                        population_name = "All",
                        numerator_col = STROKE_SCALE,
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
    
    return(stroke.01)
    
  }
    
}
