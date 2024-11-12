#' Seizure-02 
#'
#' Calculates the NEMSQA Seizure-02 Measure.
#' 
#' #' Calculates age-based seizure metrics for a dataset. This function filters data 
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
#' Grouping for specific calculations (e.g., by region) can be done within the function.
#' 
#' @section Practical Tips
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
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @import tidyverse, rlang, scales
#' 
#' @export
#'
seizure_02 <- function(df,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           esituation_11_col,
                           esituation_12_col,
                           emedications_03_col,
                           ...) {

  # provide better error messaging if df is missing
  if (missing(df)) {
    cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn seizure_02}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn seizure_02}."
      )
    )
    
  }
  
  # Ensure df is a data frame or tibble
  if (!is.data.frame(df) && !is_tibble(df)) {
    cli_abort(
      c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
      )
    )
  }
  
  # use quasiquotation on the date variables to check format
  incident_date <- enquo(incident_date_col)
  patient_DOB <- enquo(patient_DOB_col)
  
  if ((!is.Date(df[[as_name(incident_date)]]) &
       !is.POSIXct(df[[as_name(incident_date)]])) ||
      (!is.Date(df[[as_name(patient_DOB)]]) &
       !is.POSIXct(df[[as_name(patient_DOB)]]))) {
    
    cli_abort(
      "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
    )
    
  }
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary/secondary impression fields
  epilepsy_pattern <- "epilepsy.*?with status epilepticus|G40\\.\\d{1,3}"
  
  # medication values for seizure_02
  
  medication_pattern = "3322|6960|203128|6470|diazepam|midazolam|midazolam hydrochloride|lorazepam"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  # filter the table to get the initial population regardless of age
  initial_population <- df |>
  
    # create the age in years variable
    
    mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365,
    
    # create the respiratory distress variable
    seizure = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
      pattern = epilepsy_pattern,
      x = .,
      ignore.case = T
    )),
    
    # create the 911 variable
    call_911 = grepl(
      pattern = codes_911,
      x = {{eresponse_05_col}},
      ignore.case = T
    ),
    
    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & {{epatient_16_col}} == "Years",
    system_age_minor1 = {{epatient_15_col}} < 18 & {{epatient_15_col}} >= 2 & {{epatient_16_col}} == "Years",
    system_age_minor2 = {{epatient_15_col}} >= 24 & {{epatient_16_col}} == "Months",
    system_age_minor = system_age_minor1 | system_age_minor2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    ) |>
    
    dplyr::filter(
      
      # Identify Records that have seizure documentation defined above
      seizure,
      
      # filter down to 911 calls
      call_911
      
    ) |>
    
    # check to see if target meds were passed
    mutate(meds_check = if_else(grepl(pattern = medication_pattern, x = {{emedications_03_col}}, ignore.case = T), 1, 0)
           )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # get the summary of results
  
  # all
  total_population <- initial_population |>
    summarize(
      measure = "Seizure-02",
      pop = "All",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # adults
  adult_population <- adult_pop |>
    summarize(
      measure = "Seizure-02",
      pop = "Adults",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # peds
  peds_population <- peds_pop |>
    summarize(
      measure = "Seizure-02",
      pop = "Peds",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  seizure.02 <- bind_rows(adult_population, peds_population, total_population)
  
  seizure.02
  
  
}
