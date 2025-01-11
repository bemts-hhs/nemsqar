#' Pediatrics-03B Calculation
#'
#' @description
#' 
#' The function calculates a pediatric metric focused on EMS
#' responses, specifically targeting responses that involve patients under 18
#' years of age, where certain weight-based medications were administered. This
#' function filters EMS data to identify relevant 911 responses and further narrows
#' down the dataset to cases involving children, calculating the proportion of
#' cases with documented weight among those where weight-based medications were
#' administered.
#'
#' @section Data Assumptions:
#' Assume data are already loaded
#' - The data must be a data.frame or tibble that contains 
#' - eMedications.03 is a column where each cell contains all values entered for each respective incident.
#' If that is not possible, the user can pass the full set of eMedications.03 values.
#' - eMedications.04 is the full list of medication administration routes present in the data.
#' - The function can calculate an age in years using the incident date and the patient DOB.
#' - The incident date and the patient DOB are Date or POSIXct data types.
#' - If the incident date and patient DOB are not passed, the function will use the system-generated
#'   patient age and age units.
#' - This function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' - The function assumes that the eResponse.05 column has the codes in it, text
#' can be present, too, for reference
#' - The function assumes that the eResponse.05 column has the codes in it; text can be present, too, for reference.
#' - The function assumes that eDisposition.18 is a list column or a column that 
#' has all text descriptors for additional transport mode descriptors. These can be separated 
#' by commas or other characters as long as all eResponse.18 values are present in one cell for 
#' each unique eRecord.01 value. Codes can be present but will be ignored by the function.
#' - For the argument transport_disposition_cols, this argument can receive the unquoted 
#' column names of eDisposition.12 and eDisposition.30. One or both can be entered, and 
#' the function will evaluate them. These columns are used to create a `transport` variable 
#' that is used to filter the table down further. As such, these columns eDisposition.12 and 
#' eDisposition.30 can be list columns that contain all values from each unique incident 
#' entered for each field. These can be comma-separated values all in one cell to make the table tidy, 
#' or the user can pass all values for each column.
#' - The first argument is a data.frame or a tibble, no joining is done.
#' - Grouping by specific attributes (e.g., region) can be performed inside this function 
#' by utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame or tibble containing emergency response records. Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing only ePatient and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing only the eResponse fields needed for this measure's calculations. Default is `NULL`.
#' @param exam_table A data.frame or tibble containing only the eExam fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the eMedications fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column for unique EMS record identifiers.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing the EMS response codes.
#' @param eexam_01_col <['tidy-select'][dplyr_tidy_select]> Column containing documented weight information.
#' @param eexam_02_col <['tidy-select'][dplyr_tidy_select]> Another column for weight documentation, if applicable.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column indicating medication administration.
#' @param emedications_04_col <['tidy-select'][dplyr_tidy_select]> Column listing medications administered.
#' @param ... Additional parameters for the `dplyr::summarize` output.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where patient weight was documented.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where patient weight was documented.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
pediatrics_03b <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           exam_table = NULL,
                           medications_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
                           ...) {

  if(all(
    !is.null(patient_scene_table), 
    !is.null(response_table), 
    !is.null(exam_table),
    !is.null(medications_table)
  )
  
  && is.null(df)
  
  ) {
  
  # Start timing the function execution
  start_time <- Sys.time()
    
  # header
  cli::cli_h1("Pediatrics-03b")
  
  # header
  cli::cli_h2("Gathering Records for Pediatrics-03b")
  
  # gather the population of interest
  pediatrics03b_populations <- pediatrics_03b_population(patient_scene_table = patient_scene_table,
                                                         response_table = response_table,
                                                         exam_table = exam_table,
                                                         medications_table = medications_table,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")
  
  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")

  # summary
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_name = "Peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
                                      ...)
  
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
  
  return(pediatrics.03b)
  
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(exam_table),
      is.null(medications_table)
    )
    
    && !is.null(df)
    
  ) 
  
  # utilize a dataframe to analyze the data for the measure analytics
  
  {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Pediatrics-03b")
    
    # header
    cli::cli_h2("Gathering Records for Pediatrics-03b")
    
  pediatrics03b_populations <- pediatrics_03b_population(df = df,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")
  
  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")

  # summary
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_name = "Peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
                                      ...)

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
  
  return(pediatrics.03b)
  
  }
  
}
