#' Syncope-01:
#' 
#' The `syncope_01` function processes EMS dataset to identify potential syncope (fainting) cases
#' based on specific criteria and calculates related ECG measures. This function filters data for 
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
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column containing the incident date, used to calculate age.
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
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.
#' 
#' @export
#'
syncope_01 <- function(df,
                       incident_date_col,
                       patient_DOB_col,
                       epatient_15_col,
                       epatient_16_col,
                       eresponse_05_col,
                       esituation_09_col,
                       esituation_10_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_04_col,
                       ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn syncope_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn syncope_01}."
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
  # filter down the primary / other associated symptoms
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # primary and secondary provider impression values
  syncope_pattern <- "R(55|40.4)|Syncope and collapse|Transient alteration of awareness"
  
  # ECG pattern
  ecg_pattern <- "12 Lead-Left Sided \\(Normal\\)|12 Lead-Right Sided|15 Lead|18 Lead"
  
  # minor values
  
  minor_values <- "days|hours|minutes|months"

  # filter the table to get the initial population regardless of age
  initial_population <- df |>
    
    # create the age in years variable
    mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
      
      # create the syncope variable using primary / associated symptoms
      syncope1 = if_any(c({{esituation_09_col}}, {{esituation_10_col}}), ~ grepl(
        pattern = syncope_pattern,
        x = .,
        ignore.case = T
      )),
      
      # create the syncope variable using primary / secondary impressions
      syncope2 = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
        pattern = syncope_pattern,
        x = .,
        ignore.case = T
      )),
      
      # final syncope variable
      syncope = syncope1 | syncope2,
      
      # create the 911 variable
      call_911 = grepl(
        pattern = codes_911,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),
      
      # create the ECG variable
      ecg_present = if_else(grepl(pattern = ecg_pattern, x = {{evitals_04_col}}, ignore.case = T), 1, 0),
      
      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
      system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}),
      system_age_minor = system_age_minor1 | system_age_minor2,
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18
    ) |> 
    
    filter(
      
      # only records with a syncope dx
      syncope,
      
      # only 911 calls
      call_911
           )


  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # get the summary of results
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Adult",
                      ecg_present,
                      ...)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Peds",
                      ecg_present,
                      ...)
  # summary
  syncope.01 <- dplyr::bind_rows(adult_population, peds_population)
  
  syncope.01
  
}
