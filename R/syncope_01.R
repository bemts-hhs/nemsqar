#' Title
#'
#' @param df 
#' @param incident_date_col 
#' @param patient_DOB_col 
#' @param epatient_15_col 
#' @param epatient_16_col 
#' @param eresponse_05_col 
#' @param esituation_09_col 
#' @param esituation_10_col 
#' @param esituation_11_col 
#' @param esituation_12_col 
#' @param evitals_04_col 
#' @param ... 
#'
#' @return
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

syncope_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/syncope01_Export.csv") |> 
  clean_names(case = "screaming_snake", sep_out = "_")

syncope_01_clean <- syncope_01_data |> 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(string = ., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

syncope_01_clean |> 
  syncope_01(incident_date_col = INCIDENT_DATE,
             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
             esituation_09_col = SITUATION_PRIMARY_SYMPTOM_E_SITUATION_09,
             esituation_10_col = SITUATION_OTHER_ASSOCIATED_SYMPTOMS_LIST_E_SITUATION_10,
             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
             evitals_04_col = PATIENT_ECG_TYPE_LIST_E_VITALS_04
             )
