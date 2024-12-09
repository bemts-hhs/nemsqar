#' Asthma-01
#'
#' Calculates the NEMSQA Asthma-01 measure.
#'
#' Calculates key statistics related to asthma-related incidents in an EMS dataset,
#' specifically focusing on cases where 911 was called for respiratory distress,
#' and certain medications were administered. This function segments the data by
#' age into adult and pediatric populations, computing the proportion of cases that
#' received beta-agonist treatment.
#'
#' @section Data Assumptions:
#'
#' This function assumes that:
#' Data are already loaded. The data needs to be a data.frame or tibble where each row is
#' one observation (patient) and each is a feature (field) or distinct datasets
#' that can be references as unique columns.
#'
#' Age in years will be calculated using the patient date of birth and incident
#' date. These fields must have valid Date or POSIXct data types.
#'
#' When values are missing, they are coded as NA, not the "not known"/"not
#' recorded" values common to ImageTrend or the NEMSIS codes that correspond to
#' "not values".
#'
#' The primary and secondary impression fields (eSituation.11 and eSituation.12)
#' have the ICD-10 codes present in them. These fields may optionally contain
#' text for reference. Similarly, this function assumes that the eResponse.05
#' column has NEMSIS codes present, but text can also be included for reference.
#'
#' The eMedications.03 field contains all medications administered and contains
#' a text description of the medication using the generic name. The RxNORM code
#' may also be included for reference, but will not be checked.
#'
#' The secondary impressions field (eSituation.12) is best prepared as a
#' comma-separated list of all values in a single string.
#'
#' @section Practical Tips:
#'
#' The first argument is the dataframe prepared as above. No joining is done.
#' Any joins to get vitals, etc. will need to be done outside of this function.
#'
#' @section Features:
#' * Filters for asthma-related incidents (ICD-10 codes starting with 'J45' and
#' 'J98.01').
#' * Distinguishes between adults (age ≥ 18) and pediatric patients (age 2–17).
#' Calculates age in years based on incident date and patient date of birth.
#' Formats proportions as percentages with customizable decimal precision.
#'
#' @section Value:
#' A summarized tibble with counts and proportions of beta-agonist treatment
#' among 911 respiratory distress calls, segmented by population group.
#'
#' @param df A data.frame or tibble containing EMS data where each row represents
#' an observation, and columns represent features.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column representing the EMS record unique identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eResponse.05.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eSituation.11.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eSituation.12 values as a single comma-separated list.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eMedications.03 values as a single comma-separated list.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
asthma_01 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      emedications_03_col,
                      ...) {

  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn asthma_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn asthma_01}."
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
  cli::cli_h1("Calculating Asthma-01")
  
  # initiate the progress bar process
  progress_bar <- cli::cli_progress_bar(
    "Running `asthma_01()`",
    total = 5,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  progress_bar
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"

  # get codes as a regex to filter primary/secondary impression fields
  beta_agonist <- "albuterol|ipratropium|levalbuterol|metaproterenol"

  # codes for asthma or acute bronchospasm
  asthma_codes <- "\\b(?:J45|J98.01)\\b"

  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
  # filter the table to get the initial population ages >= 2 years
  initial_population <- df |>

    # create the age in years variable

    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365) |>

    # filter down to 911 calls

    dplyr::filter(grepl(
      pattern = codes_911,
      x = {{ eresponse_05_col }},
      ignore.case = T
    ),

    # Identify Records that have specified asthma

    dplyr::if_any(c({{ esituation_11_col}}, {{esituation_12_col }}), ~ grepl(
      pattern = asthma_codes,
      x = .,
      ignore.case = T
    ))) |>

    # check to ensure beta agonist was used
    dplyr::mutate(beta_agonist_check = dplyr::if_else(grepl(
      pattern = beta_agonist,
      x = {{ emedications_03_col }},
      ignore.case = TRUE
    ), 1, 0))

  cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
  # Adult and Pediatric Populations

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(patient_age_in_years_col >= 18)

  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(patient_age_in_years_col < 18, patient_age_in_years_col >= 2)

  # get the summary of results
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # summary
  asthma.01 <- results_summarize(total_population = initial_population,
                                 adult_population = adult_pop,
                                 peds_population = peds_pop,
                                 measure_name = "Asthma-01",
                                 numerator_col = beta_agonist_check,
                                 ...)

  cli::cli_progress_done()

  asthma.01


}
