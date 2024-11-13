#' Title
#'
#' @param df 
#' @param erecord_01_col 
#' @param incident_date_col 
#' @param patient_DOB_col 
#' @param epatient_15_col 
#' @param epatient_16_col 
#' @param eresponse_05_col 
#' @param esituation_11_col 
#' @param esituation_12_col 
#' @param evitals_23_col 
#' @param evitals_26_col 
#' @param transport_disposition_col 
#' @param evitals_12_col 
#' @param evitals_16_col 
#' @param evitals_06_col 
#' @param ... 
#'
#' @return
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

# GCS

GCS_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_23_col }}) |> 
  dplyr::filter({{ evitals_23_col }} < 15) |> 
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

# sp02

sp02_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_12_col }}) |> 
  dplyr::filter(!is.na({{ evitals_12_col }})) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# ETCO2

etco2_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_16_col }}) |> 
  dplyr::filter(!is.na({{ evitals_16_col }})) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# SBP

sbp_data <- core_data |> 
  dplyr::select(Unique_ID, {{ evitals_06_col }}) |> 
  dplyr::filter(!is.na({{ evitals_06_col }})) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::arrange(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# provider impression 1

provider_impression_data1 <- core_data |> 
  dplyr::select(Unique_ID, {{ esituation_11_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(
    
    grepl(pattern = tbi_injuries, x = {{ esituation_11_col }}, ignore.case = T)
    
    ) |> 
  dplyr::distinct(Unique_ID) |> 
  dplyr::pull(Unique_ID)

# provider impression 2

provider_impression_data2 <- core_data |> 
  dplyr::select(Unique_ID, {{ esituation_12_col }}) |> 
  dplyr::distinct(Unique_ID, .keep_all = T) |> 
  dplyr::filter(
    
    grepl(pattern = tbi_injuries, x = {{ esituation_12_col }}, ignore.case = T)
    
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

# transports

  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)

# assign variables to final data

  initial_population <- final_data |> 
  dplyr::mutate(GCS = Unique_ID %in% GCS_data,
         AVPU = Unique_ID %in% AVPU_data,
         PROVIDER_IMPRESSION1 = Unique_ID %in% provider_impression_data1,
         PROVIDER_IMPRESSION2 = Unique_ID %in% provider_impression_data2,
         PROVIDER_IMPRESSION = PROVIDER_IMPRESSION1 | PROVIDER_IMPRESSION2,
         CALL_911 = Unique_ID %in% call_911_data,
         TRANSPORT = Unique_ID %in% transport_data,
         PULSE_OXIMETRY = Unique_ID %in% sp02_data,
         ETCO2 = Unique_ID %in% etco2_data,
         SBP = Unique_ID %in% sbp_data,
         VITALS_CHECK = if_else(PULSE_OXIMETRY & ETCO2 & SBP, 1, 0)
         ) |> 
  dplyr::filter(
    GCS | AVPU,
      PROVIDER_IMPRESSION,
    CALL_911,
    TRANSPORT
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
  summarize_measure(measure_name = "TBI-01",
                    population_name = "Adult",
                    VITALS_CHECK,
                    ...)

# peds
peds_population <- peds_pop |>
  summarize_measure(measure_name = "TBI-01",
                    population_name = "Peds",
                    VITALS_CHECK,
                    ...) 
# summary
tbi.01 <- dplyr::bind_rows(adult_population, peds_population)

tbi.01
  
}
