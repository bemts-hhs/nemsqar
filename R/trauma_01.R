#' Trauma-01
#'
#' This function processes EMS data to calculate the Trauma-01 performance measure, 
#' which evaluates the percentage of trauma patients assessed for pain using a numeric scale. 
#' The function filters and summarizes the data based on specified inclusion criteria.
#'
#' @param df <['tidy-select'][dplyr_tidy_select]> A data frame or tibble containing EMS records.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name representing the EMS record ID.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name for the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name for the patient's age in numeric format.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for the unit of age (e.g., "Years", "Months").
#' @param esituation_02_col <['tidy-select'][dplyr_tidy_select]> Column name indicating if the situation involved an injury.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> Column name for the Glasgow Coma Scale (GCS) total score.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> Column name for AVPU (Alert, Voice, Pain, Unresponsive) status.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for the type of EMS response (e.g., 911 call).
#' @param edisposition_28_col <['tidy-select'][dplyr_tidy_select]> Column name for patient care disposition details.
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> Column name for transport disposition details.
#' @param evitals_27_col <['tidy-select'][dplyr_tidy_select]> Column name for the pain scale assessment.
#' @param ... Additional arguments passed to the `summarize_measure` function for custom summarization.
#'#'
#' @details The function performs the following steps:
#' - Validates input data for proper formats and types.
#' - Creates unique IDs for patient incidents to maintain row distinctness.
#' - Filters records based on specific criteria, including injury status, GCS or AVPU values, 
#' response type (911), patient care disposition, and transport type.
#' - Separately identifies adult and pediatric populations based on system and calculated age.
#' - Summarizes the Trauma-01 measure for the entire population, adults, and pediatrics.
#'
#' @features 
#' - Handles missing or invalid date formats with error messaging.
#' - Incorporates quasiquotation for flexible column referencing.
#' - Creates reusable dimension tables for efficient filtering and summarization.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where a pain scale was administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where a pain scale was administered.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @export
#'
trauma_01 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      evitals_23_col,
                      evitals_26_col,
                      eresponse_05_col,
                      edisposition_28_col,
                      transport_disposition_col,
                      evitals_27_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn trauma_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn trauma_01}."
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
  
  # injury values
  possible_injury <- "Yes|9922005"
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # avpu not values
  avpu_values <- "Alert|3326001"
  
  # patient care provided
  care_provided <- "4228001|Patient Evaluated and Care Provided"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
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
  dplyr::select(-c({{ esituation_02_col }},
                      {{ evitals_23_col }},
                      {{ evitals_26_col }},
                      {{ eresponse_05_col }},
                      {{ edisposition_28_col }},
                      {{ transport_disposition_col }},
                      {{ evitals_27_col }}

  )) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,
      
      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years", 
      system_age_minor1 = ({{ epatient_15_col }} < 18 & {{ epatient_15_col }} >= 2) & {{ epatient_16_col }} == "Years", 
      system_age_minor2 = {{ epatient_15_col }} >= 24 & {{ epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2, 
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18, 
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
      )

###_____________________________________________________________________________
### dimension tables
### each dimension table is turned into a vector of unique IDs
### that are then utilized on the fact table to create distinct variables
### that tell if the patient had the characteristic or not for final
### calculations of the numerator and filtering
###_____________________________________________________________________________

# GCS

GCS_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_23_col }}) |> 
  dplyr::filter({{ evitals_23_col }} == 15) |> 
  distinct(Unique_ID) |> 
  pull(Unique_ID)

# AVPU

AVPU_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_26_col }}) |> 
  dplyr::filter(grepl(pattern = avpu_values, 
                      x = {{ evitals_26_col }}, 
                      ignore.case = T)
                ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# possible injury

possible_injury_data <- core_data |> 
  dplyr::select(Unique_ID, {{ esituation_02_col }}) |> 
  dplyr::filter(grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# patient care provided

patient_care_data <- core_data |> 
  dplyr::select(Unique_ID, {{ edisposition_28_col }}) |> 
  dplyr::filter(grepl(pattern = care_provided, x = {{ edisposition_28_col }}, ignore.case = T)) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# 911 calls

call_911_data <- core_data |> 
  dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# transports

  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
# pain scale
  
  pain_scale_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_27_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      !is.na({{ evitals_27_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)

# assign variables to final data

  initial_population <- final_data |> 
  dplyr::mutate(GCS = Unique_ID %in% GCS_data,
         AVPU = Unique_ID %in% AVPU_data,
         CALL_911 = Unique_ID %in% call_911_data,
         TRANSPORT = Unique_ID %in% transport_data,
         INJURY = Unique_ID %in% possible_injury_data,
         PATIENT_CARE = Unique_ID %in% patient_care_data,
         PAIN_SCALE = Unique_ID %in% pain_scale_data
         ) |> 
  dplyr::filter(
    INJURY, 
    (GCS | AVPU),
      CALL_911,
    (PATIENT_CARE & TRANSPORT)
  )
  
# Adult and Pediatric Populations

# filter adult
adult_pop <- initial_population |>
  dplyr::filter(system_age_adult | calc_age_adult)

# filter peds
peds_pop <- initial_population |>
  dplyr::filter(system_age_minor | calc_age_minor)

# summarize

# total population

total_population <- initial_population |> 
  summarize_measure(measure_name = "Trauma-01",
                    population_name = "All",
                    PAIN_SCALE,
                    ...)

# adults
adult_population <- adult_pop |>
  summarize_measure(measure_name = "Trauma-01",
                    population_name = "Adult",
                    PAIN_SCALE,
                    ...)

# peds
peds_population <- peds_pop |>
  summarize_measure(measure_name = "Trauma-01",
                    population_name = "Peds",
                    PAIN_SCALE,
                    ...) 
# summary
trauma.01 <- dplyr::bind_rows(total_population, adult_population, peds_population)

trauma.01
  
  
}
