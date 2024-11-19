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
#'
#'
#' @param df A data frame or tibble containing emergency response records.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column representing the unique record identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> POSIXct or Date column representing the date of the incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> POSIXct or Date column representing the patient's date of birth.
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
hypoglycemia_01 <- function(df,
                            erecord_01_col,
                            incident_date_col,
                            patient_DOB_col,
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
  
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary/secondary impression fields
  hypoglycemia_treatment_codes <- "4832|4850|377980|376937|372326|237653|260258|309778|1795610|1795477|1794567|1165823|1165822|1165819"
  
  # hypoglycemia procedures
  
  hypoglycemia_procedure_codes <- "225285007|710925007"
  
  # code(s) for altered mental status
  altered_mental_status <- "\\b(?:R41.82)\\b|Altered Mental Status, unspecified"
  
  # codes for diabetes via primary and secondary impression
  
  diabetes_codes <- "\\b(?:E13.64|E16.2)\\b|Other specified diabetes mellitus with hypoglycemia|Hypoglycemia, unspecified"
  
  # AVPU responses
  
  avpu_responses <- "Unresponsive|Verbal|Painful|3326003|3326005|3326007"
  
  # days, hours, minutes, months
  
  minor_values <- "days|hours|minutes|months"
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  core_data <- df |> 
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{  incident_date_col  }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{  patient_DOB_col  }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{  erecord_01_col  }},
                                             INCIDENT_DATE_MISSING,
                                             PATIENT_DOB_MISSING, 
                                             sep = "-"
                  ))
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  final_data <- core_data |> 
    dplyr::select(-c({{  eresponse_05_col  }},
                     {{  esituation_11_col  }},
                     {{  esituation_12_col }},
                     {{  evitals_18_col  }},
                     {{  evitals_23_cl  }},
                     {{  evitals_26_col  }},
                     {{  emedications_03_col  }},
                     {{  eprocedures_03_col  }}
                     
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,
    patient_age_in_days_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age check
    system_age_adult = {{  epatient_15_col }} >= 18 & {{ epatient_16_col  }} == "Years", 
    system_age_minor1 = {{  epatient_15_col }} < 18  & {{ epatient_16_col  }} == "Years", 
    system_age_minor2 = !is.na({{ epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor3 = !({{ epatient_15_col}} < 1 & {{epatient_16_col }} == "Days") &
      !({{ epatient_15_col}} < 24 & {{epatient_16_col }} == "Hours") &
      !({{ epatient_15_col}} < 120 & {{epatient_16_col }} == "Minutes"),
    system_age_minor = (system_age_minor1 | system_age_minor2) & system_age_minor3, 
    
    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18, 
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_days_col >= 1
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # altered mental status
  
  altered_data1 <- core_data |> 
    dplyr::select(Unique_ID, {{  esituation_11_col  }}) |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  altered_data2 <- core_data |> 
    dplyr::select(Unique_ID, {{  esituation_12_col  }}) |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  # AVPU
  
  AVPU_data <- core_data |> 
    dplyr::select(Unique_ID, {{  evitals_26_col  }}) |> 
    dplyr::filter(grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T)
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # GCS
  
  GCS_data <- core_data |> 
    dplyr::select(Unique_ID, {{  evitals_23_cl  }}) |> 
    dplyr::filter({{ evitals_23_cl }} < 15) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # diabetes data
  
  diabetes_data1 <- core_data |> 
    dplyr::select(Unique_ID, {{  esituation_11_col  }}) |> 
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  diabetes_data2 <- core_data |> 
    dplyr::select(Unique_ID, {{  esituation_12_col  }}) |> 
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  # blood glucose
  
  blood_glucose_data <- core_data |> 
    dplyr::select(Unique_ID, {{  evitals_18_col  }}) |> 
    dplyr::filter({{ evitals_18_col }} < 60) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{  eresponse_05_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # correct treatment
  
  correct_treatment_data1 <- core_data |> 
    dplyr::select(Unique_ID, {{  emedications_03_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ emedications_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  correct_treatment_data2 <- core_data |> 
    dplyr::select(Unique_ID, {{  eprocedures_03_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(GCS = Unique_ID %in% GCS_data,
                  AVPU = Unique_ID %in% AVPU_data,
                  CALL_911 = Unique_ID %in% call_911_data,
                  ALTERED1 = Unique_ID %in% altered_data1,
                  ALTERED2 = Unique_ID %in% altered_data2,
                  ALTERED = ALTERED1 | ALTERED2,
                  DIABETES1 = Unique_ID %in% diabetes_data1,
                  DIABETES2 = Unique_ID %in% diabetes_data2,
                  DIABETES = DIABETES1 | DIABETES2,
                  BLOOD_GLUCOSE = Unique_ID %in% blood_glucose_data,
                  TREATMENT1 = Unique_ID %in% correct_treatment_data1,
                  TREATMENT2 = Unique_ID %in% correct_treatment_data2,
                  TREATMENT = TREATMENT1 | TREATMENT2
    ) |> 
    dplyr::filter(
      
      (DIABETES & (GCS | AVPU)) |
        
        (ALTERED & BLOOD_GLUCOSE) & 
        
        CALL_911,
      
      system_age_minor3
      
    )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # summarize
  
  # summary
  hypoglycemia.01  <- results_summarize(total_population = initial_population,
                                        adult_population = adult_pop,
                                        peds_population = peds_pop,
                                        measure_name = "Hypoglycemia-01",
                                        numerator_col = TREATMENT,
                                        ...)
  
  hypoglycemia.01
  
}
