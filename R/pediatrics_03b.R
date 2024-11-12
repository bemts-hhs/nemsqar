#' Pediatrics-03B Measure Calculation
#'
#' The `pediatrics_03b` function calculates a pediatric metric focused on EMS
#' responses, specifically targeting responses that involve patients under 18
#' years of age, where certain weight-based medications were administered. This
#' function filters EMS data to identify relevant 911 responses and further narrows
#' down the dataset to cases involving children, calculating the proportion of
#' cases with documented weight among those where weight-based medications were
#' administered.
#'
#' @section Data Assumptions:
#' Assume data are already loaded
#' The data must be a dataframe or tibble that contains emedications.03
#' as a column where each cell contains all values entered for each respective incident.
#' This is not a list column but can be comma separated values in each cell, and must contain
#' all medications for each incident.
#' emedications.04 is the full list of medication administration routes present in the data
#' can the function will roll up these values so that the table is distinct with each row being
#' 1 observation, and each column a feature.
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that edisposition.18 is a list column or a column that has all
#' text descriptors for additional transport mode descriptors.  These can be separated
#' by commas or other characters as long as all eresponse.18 values are present
#' in one cell for each unique erecord.01 value.  Codes can be present
#' but will be ignored by the function.
#' for the argument transport_disposition_cols, this argument can receive the unquoted
#' column names of edisposition.12 and edisposition.30.  One or both can be entered and
#' the function will evaluate them.  These columns are used to create a `transport`
#' variable that is used to filter the table down furhter.  AS such, these columns
#' edisposition.12 and edisposition.30 must be list columns that and/or contain all values
#' from each unique incident entered for each field.  These can be comma separated values
#' all in one cell to make the table tidy.
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#'
#' @param df Data frame or tibble containing EMS records.
#' @param erecord_01_col Column for unique EMS record identifiers.
#' @param incident_date_col Date or POSIXct Column indicating the date of the EMS incident.
#' @param patient_DOB_col Date or POSIXct Column specifying patient date of birth.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column containing the EMS response codes.
#' @param eexam_01_col Column containing documented weight information.
#' @param eexam_02_col Another column for weight documentation, if applicable.
#' @param emedications_03_col Column indicating medication administration.
#' @param emedications_04_col Column listing medications administered.
#' @param ... Additional parameters for the `dplyr::summarize` output.
#'
#' @return A summary tibble with columns for measure name, population, numerator, denominator, and proportion.
#' 
#' @section Credit:
#' 
#' This function was developed by (Nicolas Foss, Ed.D., MS)[nicolas.foss@hhs.iowa.gov] at the Bureau of Emergency Medical and Trauma 
#' Services, Division of Public Health, Iowa HHS.  (Peter Geissert's)[https://www.linkedin.com/in/peter-geissert-ba7607ba] original code base was
#' used to develop this function.
#' 
#' @export
#'
pediatrics_03b <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
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

  # non-weight-based medications
  non_weight_based_meds <- "Inhalation|Topical|9927049|9927009"

  # minor values
  minor_values <- "days|hours|minutes|months"

  # filter the table to get the initial population regardless of age, only 911 responses
  initial_population_0 <- df |>
    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # check to see if non-weight-based meds
      non_weight_based = grepl(
        pattern = non_weight_based_meds,
        x = {{ emedications_04_col }},
        ignore.case = TRUE
      ),
      call_911 = grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      ),
      meds_not_missing = !is.na({{ emedications_03_col }}),
      system_age_check = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years" |
        !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
      calc_age_check = patient_age_in_years_col < 18
    ) |>
    dplyr::filter(
      # only 911 calls
      call_911,

      # only rows where meds are passed
      meds_not_missing,

      # age filter
      system_age_check | calc_age_check,

      # exclude non-weight based meds
      !non_weight_based
    )

  initial_population_1 <- initial_population_0 |>
    dplyr::mutate(
      # check if weight was documented
      documented_weight = dplyr::if_else(!is.na({{ eexam_01_col }}) |
        !is.na({{ eexam_02_col }}), 1, 0),
    )

  # second filtering process, make the table distinct by rolling up emedications.04
  # based on a unique identifier
  initial_population <- initial_population_1 |>
    dplyr::mutate(Unique_ID = stringr::str_c({{ erecord_01_col }}, {{ incident_date_col }}, {{ patient_DOB_col }}, sep = "-")) |>
    dplyr::distinct(Unique_ID, .keep_all = T)

  # get the summary of results, already filtered down to the target age group for the measure

  # peds
  pediatrics.03b <- initial_population |>
    summarize_measure(measure_name = "Pediatrics-03b",
                      population_name = "Peds",
                      documented_weight,
                      ...)

  # summary
  pediatrics.03b
}
