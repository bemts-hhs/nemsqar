#' Safety-01 Calculation
#'
#' The `safety_01` function calculates the proportion of 911 responses where
#' "lights and sirens" were not used in an EMS dataset. It generates age-based
#' population summaries, calculating the count and proportion of "lights and
#' sirens" responses among all incidents, and within adult and pediatric groups.
#'
#' @section Details:
#' This function assumes that:
#' - The EMS dataset uses a row-per-observation format.
#' - Missing values are NA.
#' - The column eresponse.05 contains standardized response codes.
#' - The eresponse.24 column contains a list of textual descriptors.
#'
#' @section Features:
#' - Age Calculation: Computes patient age in years by subtracting patient_DOB_col from incident_date_col.
#' - Filter for 911 Responses: Filters records based on response codes in eresponse.05.
#' - Lights and Sirens Filter: Assigns a value of 1 if "lights and sirens" are not used, and 0 otherwise.
#' - Population Grouping: Separates data into adult (age ≥ 18) and pediatric (age < 18) populations.
#'
#'
#' @param df A data frame or tibble containing EMS data.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct column indicating the date of the incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct column for the patient’s date of birth
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column containing age.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column for age units.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing response mode codes (e.g., 911 response codes).
#' @param eresponse_24_col <['tidy-select'][dplyr_tidy_select]> Column detailing additional response descriptors as text.
#' @param ... arguments passed on to summarize.
#'
#' @return Returns a tibble summarizing the Safety-01 metric for all populations, adult, and pediatric groups.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
safety_01 <- function(df,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eresponse_24_col,
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

  # get codes as a regex to find lights and siren responses
  no_lights_and_sirens <- "No Lights or Sirens|2224019"

  # filter the table to get the initial population regardless of age
  initial_population <- df |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # 911 variable
      call_911 = grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      ),

      # no lights and sirens check
      no_ls_check = dplyr::if_else(grepl(pattern = no_lights_and_sirens, x = {{ eresponse_24_col }}), 1, 0),

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & {{ epatient_16_col }} == "Years",
      system_age_minor2 = {{ epatient_15_col }} >= 24 & {{ epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    ) |>
    # filter down to 911 calls

    dplyr::filter(
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

  # all
  total_population <- initial_population |>
    summarize_measure(measure_name = "Safety-01",
                      population_name = "All",
                      no_ls_check,
                      ...)

  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Safety-01",
                      population_name = "Adult",
                      no_ls_check,
                      ...)

  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Safety-01",
                      population_name = "Peds",
                      no_ls_check,
                      ...)

  # summary
  safety.01 <- dplyr::bind_rows(adult_population, peds_population, total_population)

  safety.01
}
