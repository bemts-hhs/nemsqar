#' TBI Screening Function
#'
#' This function screens for potential traumatic brain injury (TBI) cases based on specific criteria
#' in a patient dataset. It produces a subset of the data with calculated variables for TBI identification.
#' 
#' @section Data Assumptions:
#' 
#' - Date columns are already formatted as `Date` or `POSIXct`.
#' - `esituation_12_col` is a list column with all values entered for a 
#' specific record separated by commas.
#' - `evitals_23_col` is the lowest GCS score.
#' - `evitals_26_col` contains all AVPU responses entered.
#' - `evitals_12_col`, `evitals_16_col`, and `evitals_06_col` can be all the values entered for these
#' fields, or the initial value for each.  The latter will probably work best.
#'
#' @param df A data frame or tibble containing the patient data.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the patient’s unique record ID.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the patient’s age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the patient’s age unit (e.g., years, months).
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with response codes for the type of EMS call.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the primary provider impression.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the secondary provider impression.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with AVPU (alert, verbal, painful, unresponsive) values.
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with the transport disposition.
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with pulse oximetry values.
#' @param evitals_16_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with ETCO2 values.
#' @param evitals_06_col <['tidy-select'][dplyr_tidy_select]> Column name in \code{df} with systolic blood pressure (SBP) values.
#' @param ... Additional parameters passed to \code{\link[dplyr]{mutate}} or other dplyr functions.
#'
#' @return A tibble summarizing results for three population groups (Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (Adults, Peds).
#' `numerator`: Count of incidents where SP02, ETCO2, and SBP were all measured.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where SP02, ETCO2, and SBP were all measured.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @details
#' The function performs data screening and processing to identify potential TBI cases by applying the following steps:
#' - Ensures required columns are present and correctly formatted (e.g., date columns).
#' - Creates unique IDs based on patient identifiers and incident dates.
#' - Filters and selects patients who meet criteria for TBI indicators (e.g., Glasgow Coma Scale < 15, specific AVPU values, and TBI injury codes).
#' - Generates derived variables for age classification, system-calculated age check, and other indicators.
#' - Returns a filtered data frame suitable for TBI case identification.
#'
#' @note
#' This function relies on columns formatted as \code{Date} or \code{POSIXct} for dates and expects numeric or categorical 
#' values for other health indicators.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
tbi_01 <- function(df,
                   erecord_01_col,
                   incident_date_col,
                   patient_DOB_col,
                   epatient_15_col,
                   epatient_16_col,
                   eresponse_05_col,
                   esituation_11_col,
                   esituation_12_col,
                   evitals_23_col,
                   evitals_26_col,
                   transport_disposition_col,
                   evitals_12_col,
                   evitals_16_col,
                   evitals_06_col,
                   ...
                   ) 
{
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn tbi_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn tbi_01}."
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
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # header
  cli::cli_h1("Calculating TBI-01")
  
  # initiate the progress bar process
  progress_bar <- cli::cli_progress_bar(
    "Running `tbi_01()`",
    total = 16,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  progress_bar
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # avpu not values
  avpu_values <- "3326003|3326005|3326007|Verbal|Painful|Unresponsive"
  
  # TBI injuries
  tbi_injuries <- "\\b(?:S02|S04\\.4|S06|S06\\.X9|S06\\.0|S07\\.1|S09\\.90|T74\\.4)\\b"
  
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
  
  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
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

cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
final_data <- core_data |> 
  dplyr::select(-c({{ eresponse_05_col }}, 
                   {{ evitals_23_col }},
                   {{ evitals_26_col }},
                   {{ esituation_11_col }},
                   {{ esituation_12_col }},
                   {{ transport_disposition_col }},
                   {{ evitals_12_col }},
                   {{ evitals_16_col }},
                   {{ evitals_06_col }}
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

cli::cli_progress_update(set = 4, id = progress_bar, force = T)

# GCS

GCS_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_23_col }}) |> 
  dplyr::filter({{ evitals_23_col }} < 15) |> 
  distinct(Unique_ID) |> 
  pull(Unique_ID)

cli::cli_progress_update(set = 5, id = progress_bar, force = T)

# AVPU

AVPU_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_26_col }}) |> 
  dplyr::filter(grepl(pattern = avpu_values, 
                      x = {{ evitals_26_col }}, 
                      ignore.case = T)
                ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

cli::cli_progress_update(set = 6, id = progress_bar, force = T)

# vitals

vitals_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_12_col }}, {{ evitals_06_col }}, {{ evitals_16_col }}) |> 
  dplyr::distinct() |> 
  dplyr::filter(
    
    dplyr::if_all(c({{ evitals_12_col }}, {{ evitals_06_col }}, {{ evitals_16_col }}), ~ !is.na(.))
    
  ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

cli::cli_progress_update(set = 7, id = progress_bar, force = T)

# provider impression 1

provider_impression_data1 <- core_data |> 
  dplyr::select(Unique_ID, {{ esituation_11_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(
    
    grepl(pattern = tbi_injuries, x = {{ esituation_11_col }}, ignore.case = T)
    
    ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

cli::cli_progress_update(set = 8, id = progress_bar, force = T)

# provider impression 2

provider_impression_data2 <- core_data |> 
  dplyr::select(Unique_ID, {{ esituation_12_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(
    
    grepl(pattern = tbi_injuries, x = {{ esituation_12_col }}, ignore.case = T)
    
    ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

cli::cli_progress_update(set = 9, id = progress_bar, force = T)

# 911 calls

call_911_data <- core_data |> 
  dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

cli::cli_progress_update(set = 10, id = progress_bar, force = T)

# transports

  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)

  cli::cli_progress_update(set = 11, id = progress_bar, force = T)
  
# assign variables to final data

  initial_population <- final_data |> 
  dplyr::mutate(GCS = Unique_ID %in% GCS_data,
         AVPU = Unique_ID %in% AVPU_data,
         PROVIDER_IMPRESSION1 = Unique_ID %in% provider_impression_data1,
         PROVIDER_IMPRESSION2 = Unique_ID %in% provider_impression_data2,
         PROVIDER_IMPRESSION = PROVIDER_IMPRESSION1 | PROVIDER_IMPRESSION2,
         CALL_911 = Unique_ID %in% call_911_data,
         TRANSPORT = Unique_ID %in% transport_data,
         VITALS_CHECK = Unique_ID %in% vitals_data
         ) |> 
  dplyr::filter(
    (GCS | AVPU) & 
      PROVIDER_IMPRESSION & 
      CALL_911 & 
      TRANSPORT
  )
  
# Adult and Pediatric Populations

  cli::cli_progress_update(set = 12, id = progress_bar, force = T)
  
# filter adult
adult_pop <- initial_population |>
  dplyr::filter(system_age_adult | calc_age_adult)

cli::cli_progress_update(set = 13, id = progress_bar, force = T)

# filter peds
peds_pop <- initial_population |>
  dplyr::filter(system_age_minor | calc_age_minor)

# summarize

cli::cli_progress_update(set = 14, id = progress_bar, force = T)

# adults
adult_population <- adult_pop |>
  summarize_measure(measure_name = "TBI-01",
                    population_name = "Adult",
                    VITALS_CHECK,
                    ...)

cli::cli_progress_update(set = 15, id = progress_bar, force = T)

# peds
peds_population <- peds_pop |>
  summarize_measure(measure_name = "TBI-01",
                    population_name = "Peds",
                    VITALS_CHECK,
                    ...) 

cli::cli_progress_update(set = 16, id = progress_bar, force = T)

# summary
tbi.01 <- dplyr::bind_rows(adult_population, peds_population)

cli::cli_progress_done()

tbi.01
  
}
