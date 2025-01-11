#' Hypoglycemia-01
#'
#' The `hypoglycemia_01` function calculates the NEMSQA measure evaluating how often
#' hypoglycemic patients with altered mental status receive hypoglycemia treatment.
#'
#' @section Data Assumptions:
#' The `df` and `*_table` arguments should be a data.frame(s) or tibble(s) with the following assumptions:
#' - The data are already loaded.
#' - The function can calculate an age in years using the incident date and the patient DOB.
#' - The incident date and the patient DOB are Date or POSIXct data types.
#' - If the incident date and patient DOB are not passed, the function will use the system-generated
#'   patient age and age units.
#' - Any missing values are encoded as `NA`, not the "Not Known"/"Not Recorded" text values
#'   or NEMSIS "not value" codes commonly reported by ePCR vendors.
#' - The vitals field may be the full list of values for each field or the lowest estimated
#'   eVitals.18 and all estimated patient AVPU in eVitals.26.
#' - The function assumes that the primary and secondary impression fields (eSituation.11
#'   and eSituation.12) have the ICD-10 codes and/or text descriptions in them. 
#' - The function assumes that the eResponse.05 fields have the NEMSIS codes in them,
#'   and the text will be checked, too.
#' - The function assumes that the eMedications.03 and eProcedures.03 fields contain
#'   all medications/procedures and that it contains the text description of the
#'   name of the medication. The codes can be included, and will be checked. 
#'   ALL medications and procedures are in one field per record, as either a comma-separated list
#'   in each cell, or the complete set of values can be passed as well.
#' - The eSituation.12 (Secondary Impression) field is best as a list column of the secondary
#'   impressions. No joining is done.
#' - Any joins to get vitals, etc., will need to be done outside the function.
#' - Grouping by specific attributes (e.g., region) can be performed inside this function by
#'   utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame or tibble containing emergency response records. Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least ePatient and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the eResponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing at least the eSituation fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing at least the eVitals fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing at least the eMedications fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A data.frame or tibble containing at least the eProcedures fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column representing the unique record identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that contains the incident date. This defaults to `NULL` as it is optional 
#' in case not available due to PII restrictions.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that contains the patient's date of birth. This defaults to `NULL` as it is optional 
#' in case not available due to PII restrictions.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's numeric age agnostic of unit.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's age unit ("Years", "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing response type codes.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column for primary impression fields, containing ICD-10 codes.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column for secondary impression fields, containing ICD-10 codes.
#' @param evitals_18_col <['tidy-select'][dplyr_tidy_select]> Column for blood glucose levels.
#' @param evitals_23_cl <['tidy-select'][dplyr_tidy_select]> Column for Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> Column for AVPU alertness levels.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column for administered medications.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column for procedures performed.
#' @param ... Additional arguments for summarization, passed to the summarize function.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where specific hypoglycemia best practices were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where specific hypoglycemia best practices were administered.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
hypoglycemia_01 <- function(df = NULL,
                            patient_scene_table = NULL,
                            response_table = NULL,
                            situation_table = NULL,
                            vitals_table = NULL,
                            medications_table = NULL,
                            procedures_table = NULL,
                            erecord_01_col,
                            incident_date_col = NULL,
                            patient_DOB_col = NULL,
                            epatient_15_col,
                            epatient_16_col,
                            eresponse_05_col,
                            esituation_11_col,
                            esituation_12_col,
                            evitals_18_col,
                            evitals_23_cl,
                            evitals_26_col,
                            emedications_03_col,
                            eprocedures_03_col,
                            ...) {
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table),
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) 
    
    && is.null(df)
    
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # header
    cli::cli_h1("Hypoglycemia-01")
    
    # header
    cli::cli_h2("Gathering Records for Hypoglycemia-01")
    
    # gather the population of interest
    hypoglycemia_01_populations <- hypoglycemia_01_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           situation_table = situation_table,
                           vitals_table = vitals_table,
                           medications_table = medications_table,
                           procedures_table = procedures_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           esituation_11_col = {{ esituation_11_col }},
                           esituation_12_col = {{ esituation_12_col }},
                           evitals_18_col = {{ evitals_18_col }},
                           evitals_23_cl = {{ evitals_23_cl }},
                           evitals_26_col = {{ evitals_26_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )

    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Hypoglycemia-01")
    
    # summary
    hypoglycemia.01 <- results_summarize(total_population = hypoglycemia_01_populations$initial_population,
                                         adult_population = hypoglycemia_01_populations$adults,
                                         peds_population = hypoglycemia_01_populations$peds,
                                         measure_name = "Hypoglycemia-01",
                                         numerator_col = TREATMENT,
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
    
    return(hypoglycemia.01)  
    
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table)
    )
    
    && !is.null(df)
  )
  
  # Start timing the function execution
  start_time <- Sys.time()
  
  # header
  cli::cli_h1("Hypoglycemia-01")
  
  # header
  cli::cli_h2("Gathering Records for Hypoglycemia-01")
  
    # gather the population of interest
    hypoglycemia_01_populations <- hypoglycemia_01_population(
                                                  df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col}},
                                                  epatient_15_col = {{ epatient_15_col}},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  evitals_18_col = {{ evitals_18_col }},
                                                  evitals_23_cl = {{ evitals_23_cl }},
                                                  evitals_26_col = {{ evitals_26_col }},
                                                  emedications_03_col = {{ emedications_03_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Hypoglycemia-01")

    # summary
    hypoglycemia.01 <- results_summarize(total_population = hypoglycemia_01_populations$initial_population,
                                         adult_population = hypoglycemia_01_populations$adults,
                                         peds_population = hypoglycemia_01_populations$peds,
                                         measure_name = "Hypoglycemia-01",
                                         numerator_col = TREATMENT,
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
    
    return(hypoglycemia.01)  
}
