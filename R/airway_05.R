#' Airway-01
#'
#' Calculates the NEMSQA Airway-05 measure.
#'
#' Calculates the proportion of endotracheal intubation attempts with adequate oxygenation.
#'
#'
#' @param df A dataframe or tibble contianing EMS data where each row represents
#' an observation and columns represent features.
#' @param patient_scene_table A data.frame or tibble containing at least epatient, escene, and earrest.01 fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param procedures_table A dataframe or tibble containing at least the eProcedures fields needed.
#' @param vitals_table A dataframe or tibble containing at least the eVitals fields needed.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column representing the EMS record unique identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's numeric age agnostic of unit.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's age unit ("Years", "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eResponse.05.
#' @param earrest_01_col  <['tidy-select'][dplyr_tidy_select]> Column representing whether or not the patient is in arrest.
#' @param evitals_01_col  <['tidy-select'][dplyr_tidy_select]> Date-time or POSIXct column containing vital signs date/time
#' @param evitals_06_col  <['tidy-select'][dplyr_tidy_select]> Numeric column containing systolic blood pressure values
#' @param evitals_12_col  <['tidy-select'][dplyr_tidy_select]> Numeric column containing pulse oximetry values.
#' @param eprocedures_01_col  <['tidy-select'][dplyr_tidy_select]> Date-time or POSIXct column for procedures
#' @param eprocedures_02_col  <['tidy-select'][dplyr_tidy_select]> Column that indicates procedure prior to arrival.
#' @param eprocedures_03_col  <['tidy-select'][dplyr_tidy_select]> Column containing procedure codes with or without procedure names.
#' @param eprocedures_05_col  <['tidy-select'][dplyr_tidy_select]> Column containing a count for how many times procedure was attempted.
#' @param eprocedures_06_col  <['tidy-select'][dplyr_tidy_select]> Column indicating whether or not procedure was successful.
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
#' @export
#'
airway_01 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      earrest_01_col,
                      eresponse_05_col,
                      evitals_01_col,
                      evitals_06_col,
                      evitals_12_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      ...) {

  # Either ALL tables are fulfilled or just df.

  if(

    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(procedures_table),
      !is.null(vitals_table)
    )

    &&

    !is.null(df)

  ) {

    cli::cli_abort("{.fn airway_05} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")

  }


  # ensure that df or all table arguments are fulfilled
  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(procedures_table),
      is.null(vitals_table)
    )

    && is.null(df)

  ) {

    cli::cli_abort("{.fn airway_05} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")

  }

  # ensure all *_col arguments are fulfilled
  if(

    any(

      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(earrest_01_col),
      missing(eresponse_05_col),
      missing(evitals_01_col),
      missing(evitals_06_col),
      missing(evitals_12_col),
      missing(eprocedures_01_col),
      missing(eprocedures_02_col),
      missing(eprocedures_03_col),
      missing(eprocedures_05_col),
      missing(eprocedures_06_col)
    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn airway_01}.")

  }

  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")

  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))

  # initiate the progress bar process
  progress_bar_main <- cli::cli_progress_bar(
    "Running `airway_01()`",
    total = 1,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]\n"
  )

  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table),
        !is.null(response_table),
        !is.null(procedures_table),
        !is.null(vitals_table)
    ) && is.null(df)

  ) {

    # Ensure df is a data frame or tibble
    if (

      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||

          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) ||

          !(is.data.frame(procedures_table) && tibble::is_tibble(procedures_table)) ||

          !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table))

      )

    ) {

      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required for each of the *_table arguments."
        )
      )
    }

    # only check the date columns if they are in fact passed
    if(
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col)),
        !rlang::quo_is_null(rlang::enquo(evitals_01_col)),
        !rlang::quo_is_null(rlang::enquo(eprocedures_01_col)),
      )
    )

    {
      # use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_DOB <- rlang::enquo(patient_DOB_col)
      evitals_01 <- rlang::enquo(evitals_01_col)
      eprocedures_01 <- rlang::enquo(eprocedures_01)

      if (
        any(
          ( !lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])),

          (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_DOB)]]) &
           !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_DOB)]])),

          (!lubridate::is.Date(vitals_table[[rlang::as_name(evitals_01_col)]]) &
           !lubridate::is.POSIXct(vitals_table[[rlang::as_name(evitals_01_col)]])),

          (!lubridate::is.Date(procedures_table[[rlang::as_name(eprocedures_01_col)]]) &
          !lubridate::is.POSIXct(procedures_table[[rlang::as_name(eprocedures_01_col)]]))
          )
        ) {

        cli::cli_abort(
          "For the variables {.var incident_date_col}, {.var patient_DOB_col}, {.var evitals_01_col}, {.var eprocedures_01_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your these vitals to class {.cls Date} or similar class."
        )

      }
    }

    # header
    cli::cli_h1("Airway-01")

    # header
    cli::cli_h2("Gathering Records for Airway-01")

  #############################################################################
  #                                                                           #
  #     Get Population Level Information from tables and columns              #
  #                                                                           #
  #############################################################################

  airway_05_population <- airway_05_population(patient_scene_table = patient_scene_table,
                                               response_table = response_table,
                                               procedures_table = procedures_table,
                                               vitals_table = vitals_table,
                                               erecord_01_col = {{ erecord_01_col }},
                                               incident_date_col = {{ incident_date_col }},
                                               patient_DOB_col = {{ patient_DOB_col }},
                                               epatient_15_col = {{ epatient_15_col }},
                                               epatient_16_col = {{ epatient_16_col }},
                                               earrest_01_col = {{ earrest_01_col }},
                                               eresponse_05_col = {{ eresponse_05_col }},
                                               evitals_01_col = {{ evitals_01_col }},
                                               evitals_06_col = {{ evitals_06_col }},
                                               evitals_12_col = {{ evitals_12_col }},
                                               eprocedures_01_col = {{ eprocedures_01_col }},
                                               eprocedures_02_col = {{ eprocedures_02_col }},
                                               eprocedures_03_col = {{ eprocedures_03_col }},
                                               eprocedures_05_col = {{ eprocedures_05_col }},
                                               eprocedures_06_col = {{ eprocedures_06_col }}
                                               )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-05")

    # summary
    airway.05 <- results_summarize(total_population = airway_05_population()$initial_population,
                                   adult_population = airway_05_population($adults,
                                   peds_population = airway_05_population($peds,
                                   measure_name = "Airway-05",
                                   numerator_col = measure_performance,
                                   ...)

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {

      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(airway.05)


}}
