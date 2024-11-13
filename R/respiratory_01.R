#' Respiratory-01 Calculation
#'
#' The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.
#'
#' @section Assumptions:
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the primary/secondary impression fields have the
#' ICD-10 code in them.  The text description can be present, too, for reference.
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that vitals in the vital signs columns are likely the
#' first vital signs, or are a list column.  This will give an indication of whether
#' or not any vitals were taken.
#' the esituation_12 is best as a list column of the secondary impressions entered
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame containing incident data with each row representing an observation.
#' @param erecord_01_col Unique Patient ID
#' @param incident_date_col Date or POSIXct Column name for the Incident Date field.
#' @param patient_DOB_col Date or POSIXct Column name for epatient.17.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column name for 911 response codes (e.g., 2205001, 2205003, 2205009).
#' @param esituation_11_col Column name for primary impression codes related to respiratory distress.
#' @param esituation_12_col Column name for secondary impression codes related to respiratory distress.
#' @param evitals_12_col Column name for the first vital sign measurement.
#' @param evitals_14_col Column name for the second vital sign measurement.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return Returns a data frame summarizing the proportion of cases with complete vital sign data, divided by population
#' 
#' #' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.  (Alyssa Green's)[https://www.linkedin.com/in/alyssa-green-1a7aa4218] original code base was 
#' used to develop this function.
#' 
#' @export
#'
respiratory_01 <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           esituation_11_col,
                           esituation_12_col,
                           evitals_12_col,
                           evitals_14_col,
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

  # get codes as a regex to filter primary impression fields
  resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"

  # minor values
  minor_values <- "days|hours|minutes|months"

  # filter the table to get the initial population regardless of age
  initial_population <- df |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # create the respiratory distress variable
      respiratory_distress = dplyr::if_any(c({{ esituation_11_col }}, {{ esituation_12_col }}), ~ grepl(
        pattern = resp_codes,
        x = .,
        ignore.case = T
      )),

      # create the 911 variable
      call_911 = grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      ),

      # system age checks
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor = ({{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years") |
        (!is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T)),
      # calculated age checks
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18
    ) |>
    dplyr::filter(

      # Identify Records that have Respiratory Distress Codes defined above
      respiratory_distress,

      # filter down to 911 calls
      call_911
    ) |>
    # check to see if target vitals were captured
    dplyr::mutate(vitals_check = dplyr::if_else(!is.na({{ evitals_12_col }}) &
      !is.na({{ evitals_14_col }}), 1, 0)) |>
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{ incident_date_col }}, as.Date("1984-09-01")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{ incident_date_col }}, as.Date("1984-05-01")),
                  Unique_ID = stringr::str_c({{erecord_01_col}}, INCIDENT_DATE_MISSING, PATIENT_DOB_MISSING, sep = "-")) |>
    dplyr::distinct(Unique_ID, .keep_all = T)

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
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "All",
                      vitals_check,
                      ...)


  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "Adults",
                      vitals_check,
                      ...)

  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "Peds",
                      vitals_check,
                      ...)

  # summary
  resp_01 <- dplyr::bind_rows(adult_population, peds_population, total_population)

  resp_01
}
