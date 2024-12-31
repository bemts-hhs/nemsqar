#' Hypoglycemia-01
#'
#' The `hypoglycemia_01` function calculates the NEMSQA measure evaluating how often
#' hypoglycemic patients with altered mental status receive hypoglycemia treatment.
#'
#' @section Data Assumptions:
#' The `df` argument should be a dataframe or tibble with the following assumptions:
#' - The data is already loaded.
#' - The data has one row per patient/incident and one column for each feature/field.
#' - The function will calculate an age in years using the incident date and the patient DOB.
#' - The incident date and the patient DOB are Date or POSIXct data types.
#' - Any missing values are encoded as `NA`, not the "Not Known"/"Not Recorded" text values
#'   or NEMSIS "not value" codes commonly reported by ePCR vendors.
#' - The vitals field may be the full list of values for each field or the lowest estimated
#'   eVitals.18 (must include the "Low Blood Glucose" flag) and lowest estimated patient AVPU
#'   in eVitals.2.
#' - The function assumes that the primary and secondary impression fields (eSituation.11
#'   and eSituation.12) have the ICD-10 codes in them. The test description can be present,
#'   too, for reference.
#' - The function assumes that the eResponse.05 fields have the NEMSIS codes in them,
#'   although text can be also be present for reference.
#' - The function assumes that the eMedications_03 and eProcedures_03 fields contain
#'   all medications/procedures and that it contains the text description of the
#'   generic name of the medication. The codes can be included for reference, but will
#'   not be checked. ALL medications and prodcedures are in one field per record, as either
#'   a list column or a comma-separated list.
#' - The eSituation.12 (Secondary Impression) field is best as a list column of the secondary
#'   impressions. No joining is done.
#' - Any joins to get vitals, etc., will need to be done outside the function.
#' - Grouping by specific attributes (e.g., region) can be performed inside this function by
#'   utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame or tibble containing emergency response records.
#' @param patient_scene_table A data.frame or tibble containing at least epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing at least the esituation fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing at least the emedications fields needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing at least the eprocedures fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column representing the unique record identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
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
    
    return(hypoglycemia.01)  
}
