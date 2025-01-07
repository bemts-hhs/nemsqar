#' Respiratory-01 Calculation
#'
#' The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.
#'
#' @section Assumptions:
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the primary/secondary impression fields have the
#' ICD-10 code in them.  The text description can be present, too, for reference.
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that vitals in the vital signs columns are likely the
#' first vital signs, or are a list column.  This will give an indication of whether
#' or not any vitals were taken.
#' the esituation_12 is best as a list column of the secondary impressions entered
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame containing incident data with each row representing an observation.
#' @param patient_scene_table A data.frame or tibble containing at least epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing at least the esituation fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Unique Patient ID
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the Incident Date field.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for epatient.17.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for 911 response codes (e.g., 2205001, 2205003, 2205009).
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column name for primary impression codes related to respiratory distress.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column name for secondary impression codes related to respiratory distress.
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name for the first vital sign measurement.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> Column name for the second vital sign measurement.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return Returns a data frame summarizing the proportion of cases with complete vital sign data, divided by population
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
respiratory_01 <- function(df = NULL,
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
                           esituation_11_col,
                           esituation_12_col,
                           evitals_12_col,
                           evitals_14_col,
                           ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table),
      !is.null(vitals_table)
    ) 
    
    && is.null(df)
    
  ) {
  
  # Start timing the function execution
  start_time <- Sys.time()
    
  # header
  cli::cli_h1("Respiratory-01")
  
  # header
  cli::cli_h2("Gathering Records for Respiratory-01")
  
  # gather the population of interest
  respiratory_01_populations <- respiratory_01_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           situation_table = situation_table,
                           vitals_table = vitals_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           esituation_11_col = {{ esituation_11_col }},
                           esituation_12_col = {{ esituation_12_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           evitals_14_col = {{ evitals_14_col }}
                           )
  
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Respiratory-01")

    # summary
    respiratory.01 <- results_summarize(total_population = respiratory_01_populations$initial_population,
                                   adult_population = respiratory_01_populations$adults,
                                   peds_population = respiratory_01_populations$peds,
                                   measure_name = "Respiratory-01",
                                   numerator_col = VITALS_CHECK,
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
    
    return(respiratory.01)
  
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(vitals_table)
    )
    
    && !is.null(df)
    
  ) 
  
  # utilize a dataframe to analyze the data for the measure analytics
  
  {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Respiratory-01")
    
    # header
    cli::cli_h2("Gathering Records for Respiratory-01")
    
    # gather the population of interest
    respiratory_01_populations <- respiratory_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col}},
      epatient_15_col = {{ epatient_15_col}},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      esituation_11_col = {{ esituation_11_col }},
      esituation_12_col = {{ esituation_12_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }}
    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Respiratory-01")
    
    # summary
    respiratory.01 <- results_summarize(total_population = respiratory_01_populations$initial_population,
                                        adult_population = respiratory_01_populations$adults,
                                        peds_population = respiratory_01_populations$peds,
                                        measure_name = "Respiratory-01",
                                        numerator_col = VITALS_CHECK,
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
    
    return(respiratory.01)
    
  }
  
}
