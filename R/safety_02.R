#' Safety-01 Calculation
#'
#' The `safety_02` function calculates the Safety-02 metric, evaluating the
#' proportion of emergency medical calls involving transport where no lights and
#' sirens were used. This function categorizes the population into adult and
#' pediatric groups based on their age, and summarizes results with a total
#' population count as well.
#'
#' @section Details:
#'
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
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
#' @param df A data frame where each row is an observation, and each column represents a feature.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name representing the date of the incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column giving response codes, identifying 911 responses.
#' @param edisposition_18_col <['tidy-select'][dplyr_tidy_select]> Column giving transport mode descriptors, including possible lights-and-sirens indicators.
#' @param edisposition_28_col <['tidy-select'][dplyr_tidy_select]> Column giving patient evaluation and care categories for the EMS response.
#' @param transport_disposition_cols <['tidy-select'][dplyr_tidy_select]> One or more unquoted column names (such as edisposition.12, edisposition.30) containing transport disposition details.
#' @param ... Additional arguments for summary calculation, if needed.
#'
#' @return A data frame summarizing the Safety-02 metric with calculated values for three populations:
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
safety_02 <- function(df,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      edisposition_18_col,
                      edisposition_28_col,
                      transport_disposition_cols,
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

  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # header
  cli::cli_h1("Calculating Safety-02")
  
  # initiate the progress bar process
  progress_bar <- cli::cli_progress_bar(
    "Running `safety_02()`",
    total = 7,
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

  # patient evaluation care
  patient_care <- "4228001|Patient Evaluated and Care Provided"

  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"

  # get codes as a regex to find lights and siren responses
  no_lights_and_sirens <- "4218015|No Lights or Sirens"

  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
  # filter the table to get the initial population regardless of age
  initial_population_0 <- df |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # transport variable
      transport = dplyr::if_else(dplyr::if_any(
        c({{ transport_disposition_cols }}),
        ~ grepl(
          pattern = transport_responses,
          x = .,
          ignore.case = T
        )
      ), TRUE, FALSE),

      # 911 variable
      call_911 = grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      ),

      # no lights and sirens check
      no_ls_check = dplyr::if_else(grepl(pattern = no_lights_and_sirens, x = {{ edisposition_18_col }}), 1, 0),

      # patient evaluation care provided
      patient_care_provided = grepl(pattern = patient_care, x = {{ edisposition_28_col }}, ignore.case = T),

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & {{ epatient_16_col }} == "Years",
      system_age_minor2 = {{ epatient_15_col }} >= 24 & {{ epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    )

  # get the initial population
  initial_population <- initial_population_0 |>
    dplyr::filter(

      # filter down to 911 calls
      call_911,

      # patient evaluated and care provided only
      patient_care_provided,

      # NEMSIS 3.5 transports only
      transport
    )

  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)

  cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)

  # get the summary of results

  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # all
  total_population <- initial_population |>
    summarize_measure(measure_name = "Safety-02",
                      population_name = "All",
                      no_ls_check,
                      ...)

  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Safety-02",
                      population_name = "Adults",
                      no_ls_check,
                      ...)

  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Safety-02",
                      population_name = "Peds",
                      no_ls_check,
                      ...)

  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # summary
  safety.02 <- dplyr::bind_rows(adult_population, peds_population, total_population)

  cli::cli_progress_done()
  
  safety.02
  
}
