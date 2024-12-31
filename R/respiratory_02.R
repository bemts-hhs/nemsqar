#' Respiratory-02 Calculation
#'
#' The `respiratory_02` function calculates metrics for pediatric and adult
#' respiratory populations based on pre-defined criteria, such as low oxygen
#' saturation and specific medication or procedure codes. It returns a summary
#' table of the overall, pediatric, and adult populations, showing counts and
#' proportions.
#'
#' @section Data Assumptions:
#' Assume data are already loaded The data must be a dataframe or tibble that
#' contains emedications.03 and eprocedures.03 as columns where each cell
#' contains all values entered for each respective incident. These are not list
#' columns but can be comma separated values in each cell, and must contain all
#' medications for each incident. The table must also have the erecord.01 values
#' to attempt a best estimate of a unique ID by concatenating the erecord.01,
#' incident date, and patient DOB. The table can include all values entered for
#' evitals.12 in the table.  This will cause row explosion, but the function
#' will find if the patient had sp02 < 90 among any values entered. This
#' function assumes you have the following fields: eRecord.01, eresponse.05
#' (must contain code), evitals.12 (all values), emedications.03 (all with
#' code), eprocedures.03 (with code) this function will calculate age in years
#' for the end user. this function also assumes that rows that are missing any
#' value are NA, not the not known / not recorded values common to ImageTrend or
#' the value codes that correspond to "not values". the first argument is a
#' dataframe, no joining is done in the function. any needed table joins must be
#' done prior to running the function to ensure required columns are present.
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#'
#' @param df A data frame containing incident data with each row representing an observation.
#' @param patient_scene_table A data.frame or tibble containing at least epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing only the eprocedures fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name for eRecord.01, used to form a unique patient ID.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> integer Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for response codes (e.g., incident type).
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name for oxygen saturation (SpO2) values.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column name for medication codes.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column name for procedure codes.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return Returns a tibble summarizing the overall and age-grouped respiratory-02 metrics, formatted for ease of interpretation.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
respiratory_02 <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           vitals_table = NULL,
                           medications_table = NULL,
                           procedures_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col,
                           ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) && is.null(df)
    
  ) {
    
    # header
    cli::cli_h1("Respiratory-02")
    
    # header
    cli::cli_h2("Gathering Records for Respiratory-02")
    
  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           vitals_table = vitals_table,
                           procedures_table = procedures_table,
                           medications_table = medications_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )
  
  # create a separator
  cli::cli_text("\n")
  
  # header for calculations
  cli::cli_h2("Calculating Respiratory-02")

  # summary
  respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                      adult_population = respiratory_02_populations$adults,
                                      peds_population = respiratory_02_populations$peds,
                                      measure_name = "Respiratory-02",
                                      numerator_col = OXYGEN,
                                      ...)

  # create a separator
  cli::cli_text("\n")
  
  return(respiratory.02)
  
    
    } else if(
      
        all(
          is.null(patient_scene_table), 
          is.null(response_table), 
          is.null(vitals_table), 
          is.null(medications_table),
          is.null(procedures_table)
        ) && !is.null(df)
        
    # utilize a dataframe to analyze the data for the measure analytics

    ) {
  
  # header
  cli::cli_h1("Respiratory-02")
  
  # header
  cli::cli_h2("Gathering Records for Respiratory-02")
  
  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           df = df,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )
  
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Respiratory-02")

    # summary
    respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                   adult_population = respiratory_02_populations$adults,
                                   peds_population = respiratory_02_populations$peds,
                                   measure_name = "Respiratory-02",
                                   numerator_col = OXYGEN,
                                   ...)

    # create a separator
    cli::cli_text("\n")
    
    return(respiratory.02)
      
    }
        
}
