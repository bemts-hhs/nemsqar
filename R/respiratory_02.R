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
#' @param df  Data frame or tibble containing EMS incident data.
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
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.
#' 
#' @export
respiratory_02 <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col,
                           ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
      )
    )
  }
  
  # Ensure df is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
      )
    )
  }
  
  # use quasiquotation on the date variables to check format
  incident_date <- rlang::enquo(incident_date_col)
  patient_DOB <- rlang::enquo(patient_DOB_col)
  
  if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))) {
    cli::cli_abort(
      "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
    )
  }
  
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  # not values for meds
  not_med <- "8801001|8801003|8801009|8801019|8801027"
  
  # not values for procedures
  not_proc <- "8801001|8801023|8801003|8801019|8801027"
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  core_data <- df |> 
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{ incident_date_col }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{ patient_DOB_col }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{ erecord_01_col }},
                                             INCIDENT_DATE_MISSING,
                                             PATIENT_DOB_MISSING, 
                                             sep = "-"
                  ))
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ evitals_12_col }},
                     {{ emedications_03_col }},
                     {{ eprocedures_03_col }}
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    patient_age_in_days = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
    system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & {{ epatient_16_col }} == "Hours",
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & {{ epatient_16_col }} == "Minutes",
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years >= 18,
    calc_age_minor = patient_age_in_years < 18 & patient_age_in_days >= 1
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # pulse oximetry
  
  pulse_oximetry_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_12_col }}) |> 
    dplyr::filter(
      
      {{ evitals_12_col }} < 90
      
    ) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  # vitals check
  
  oxygen_med_data <- core_data |> 
    dplyr::select(Unique_ID, {{ emedications_03_col }}) |> 
    dplyr::filter(
      
      grepl(pattern = "7806", x = {{ emedications_03_col }}) &
        !grepl(pattern = not_med, x = {{ emedications_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  oxygen_proc_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eprocedures_03_col }}) |> 
    dplyr::filter(
      
      grepl(pattern = "57485005", x = {{ eprocedures_03_col }}, ignore.case = T) &
        !grepl(pattern = not_proc, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(PULSE_OXIMETRY = Unique_ID %in% pulse_oximetry_data,
                  OXYGEN1 = Unique_ID %in% oxygen_med_data,
                  OXYGEN2 = Unique_ID %in% oxygen_proc_data,
                  OXYGEN = OXYGEN1 | OXYGEN2,
                  CALL_911 = Unique_ID %in% call_911_data
    ) |> 
    dplyr::filter(
      
      PULSE_OXIMETRY,
      
      CALL_911
      
    )
  
  # get population 1 for respiratory-02, peds
  respiratory_02_peds <- initial_population |>
    dplyr::filter(
      
      (system_age_minor & !system_age_minor_exclusion) | calc_age_minor
      
    )
  
  # get population 2 for respiratory-02, adults
  respiratory_02_adults <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # calculations for peds
  peds_calculation <- respiratory_02_peds |>
    dplyr::summarize(
      measure = "Respiratory-02",
      pop = "Peds",
      numerator = sum(OXYGEN, na.rm = T),
      denominator = dplyr::n(),
      prop = sum(numerator, na.rm = T) / dplyr::n(),
      prop_label = pretty_percent(prop, n_decimal = 0.01),
      ...
    )
  
  # calculations for adults
  adults_calculation <- respiratory_02_adults |>
    dplyr::summarize(
      measure = "Respiratory-02",
      pop = "Adults",
      numerator = sum(OXYGEN, na.rm = T),
      denominator = dplyr::n(),
      prop = sum(numerator, na.rm = T) / dplyr::n(),
      prop_label = pretty_percent(prop, n_decimal = 0.01),
      ...
    )
  
  # bind rows of calculations for final table
  respiratory.02 <- dplyr::bind_rows(peds_calculation, adults_calculation)
  
  respiratory.02
  
}