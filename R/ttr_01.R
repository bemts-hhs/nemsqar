#' TTR-01 Measure Calculation
#'
#' This function calculates the TTR_01 measure, which evaluates the completeness of pain scale documentation for patients experiencing traumatic injury. It determines the total population, adult population, and pediatric population meeting the criteria for the TTR_01 measure. **Note:** This function assumes the input dataset contains the *initial* vital signs for respiratory rate, systolic blood pressure (SBP), and total Glasgow Coma Scale (GCS) score, respectively.
#'
#' @param df A data frame or tibble containing the dataset to analyze.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> A column specifying unique patient records.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> A column indicating the incident date. Must be of class `Date` or `POSIXct`.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient's date of birth. Must be of class `Date` or `POSIXct`.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient’s age in numeric form.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> A column specifying the unit of patient age (e.g., "Years", "Days").
#' @param esituation_02_col <['tidy-select'][dplyr_tidy_select]> A column containing information about the nature of the patient’s condition (e.g., injury type).
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> A column specifying the type of response (e.g., 911 codes).
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> A column specifying transport disposition for the patient.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> A column containing respiratory rate data from initial vital signs.
#' @param evitals_06_col <['tidy-select'][dplyr_tidy_select]> A column containing systolic blood pressure (SBP) data from initial vital signs.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> A column containing total Glasgow Coma Scale (GCS) scores from initial vital signs.
#' @param ... Additional arguments passed to the `summarize_measure` function.
#'
#' @details The function performs the following steps:
#' - Validates input data for proper formats and types.
#' - Creates unique IDs for patient incidents to maintain row distinctness.
#' - Filters records based on specific criteria, including injury status, response type (911), 
#'   and transport type.
#' - Separately identifies adult and pediatric populations based on system and calculated age.
#' - Summarizes the TTR_01 measure for the entire population, adults, and pediatrics.
#'
#' @section Features: 
#'  
#' - Handles missing or invalid date formats with error messaging.
#' - Incorporates quasiquotation for flexible column referencing.
#' - Creates reusable dimension tables for efficient filtering and summarization.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where the respiratory rate, SBP, and GCS vitals were taken.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where the respiratory rate, SBP, and GCS vitals were taken.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @note 
#' - Ensure the input dataset contains initial vital signs for each required vital signs. 
#' - Not values should not be used, values missing a response should be blank so R interprets those as `NA` values.
#' - Date columns (`incident_date_col` and `patient_DOB_col`) must be properly formatted before calling this function.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
trauma_08 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      transport_disposition_col,
                      earrest_01_col,
                      evitals_06_col,
                      evitals_07_col,
                      evitals_10_col,
                      evitals_12_col,
                      evitals_14_col,
                      evitals_23_col,
                      evitals_26_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn trauma_08}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn trauma_08}."
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
  
  # Create objects that are filter helpers throughout the function
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # define transports
  no_transport_responses <- "4230009|patient refused transport|no transport|4230013|without transport|4212025|No Treatment/Transport Required|4212021|4212019|4212015|itDisposition\\.112\\.112|itDisposition\\.112\\.107|itDisposition\\.112\\.110|itDisposition\\.112\\.109"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
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
                     {{ transport_disposition_col }},
                     {{ earrest_01_col }},
                     {{ evitals_06_col }},
                     {{ evitals_07_col }},
                     {{ evitals_10_col }},
                     {{ evitals_12_col }},
                     {{ evitals_14_col }},
                     {{ evitals_23_col }},
                     {{ evitals_26_col }}
                     
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    
    # system age check
    system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years", 
    system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years", 
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2, 
    
    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18, 
    calc_age_minor = patient_age_in_years_col < 18
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # no transports
  
  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(pattern = no_transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # SBP, DBP, HR, RR, GCS, AVPU taken
  
  vitals_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }},
                  {{ evitals_12_col }}, {{ evitals_14_col }}, {{ evitals_23_col }}, {{ evitals_26_col }}
                  ) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      dplyr::if_all(c({{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }},
                      {{ evitals_12_col }}, {{ evitals_14_col }}, {{ evitals_23_col }}, {{ evitals_26_col }}), ~ !is.na(.))
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(CALL_911 = Unique_ID %in% call_911_data,
                  NO_TRANSPORT = Unique_ID %in% transport_data,
                  VITALS = Unique_ID %in% vitals_data
    ) |> 
    dplyr::filter(
      CALL_911,
      NO_TRANSPORT
    )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # summarize
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "TTR_01",
                      population_name = "Adult",
                      VITALS,
                      ...)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "TTR_01",
                      population_name = "Peds",
                      VITALS,
                      ...) 
  # summary
  ttr.01 <- dplyr::bind_rows(total_population, adult_population, peds_population)
  
  ttr.01
  
  
}
