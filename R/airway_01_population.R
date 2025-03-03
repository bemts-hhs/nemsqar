#' @title Airway-01 Population
#'
#' @description
#'
#' This function processes and analyzes the dataset to generate the populations
#' of interest needed to perform calculations to obtain performance data.
#'
#' @param df A dataframe or tibble contianing EMS data where each row represents
#'   an observation and columns represent features.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient, escene, and earrest.01 fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations.
#' @param arrest_table A data.frame or tibble containing at least the earrest
#'   fields needed for this measure's calculations.
#' @param procedures_table A dataframe or tibble containing at least the
#'   eProcedures fields needed.
#' @param vitals_table A dataframe or tibble containing at least the eVitals
#'   fields needed.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient's numeric age agnostic
#'   of unit.
#' @param epatient_16_col Column representing the patient's age unit ("Years",
#'   "Months", "Days", "Hours", or "Minutes").
#' @param eresponse_05_col Column that contains eResponse.05.
#' @param earrest_01_col Column representing whether or not the patient is in
#'   arrest.
#' @param evitals_01_col Date-time or POSIXct column containing vital signs
#'   date/time
#' @param evitals_06_col Numeric column containing systolic blood pressure
#'   values
#' @param evitals_12_col Numeric column containing pulse oximetry values.
#' @param eprocedures_01_col Date-time or POSIXct column for procedures
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col Column containing procedure codes with or without
#'   procedure names.
#' @param eprocedures_05_col Column containing a count for how many times
#'   procedure was attempted.
#' @param eprocedures_06_col Column indicating whether or not procedure was
#'   successful.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
#'
#' @examples
#'
#' # If you are sourcing your data from a SQL database connection
#' # or if you have your data in several different tables,
#' # you can pass table inputs versus a single data.frame or tibble
#'
#' # create tables to test correct functioning
#'
#'   # patient table
#'   patient_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'     "2025-01-01", "2025-06-01")), 2),
#'     patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'                                 "2023-01-01", "1970-06-01")), 2),
#'     epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
#'     epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2)
#'
#'   )
#'
#'   # response table
#'   response_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eresponse_05 = rep(2205001, 10)
#'
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 22:59:00",
#'     "2025-01-05 11:58:00", "2025-02-01 18:57:00", "2025-01-01 04:58:00",
#'     "2025-06-01 12:57:00", "2025-01-01 23:05:00", "2025-01-05 12:04:00",
#'     "2025-02-01 19:03:00", "2025-01-01 05:02:00", "2025-06-01 13:01:00")),
#'     evitals_06 = rep(c(90, 100, 102, 103, 104), 2),
#'     evitals_12 = rep(c(90, 91, 92, 93, 94), 2)
#'
#'   )
#'
#' # arrest table
#'   arrest_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     earrest_01 = rep("No", 10)
#'   )
#'
#'   # procedures table
#'   procedures_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
#'     "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
#'     "2025-06-01 13:00:00")), 2),
#'     eprocedures_02 = rep("No", 10),
#'     eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
#'                            673005), 2),
#'     eprocedures_05 = rep(1, 10),
#'     eprocedures_06 = rep(9923003, 10)
#'
#'   )
#'
#' # Run the function
#' result <- airway_01_population(df = NULL,
#'          patient_scene_table = patient_table,
#'          procedures_table = procedures_table,
#'          vitals_table = vitals_table,
#'          arrest_table = arrest_table,
#'          response_table = response_table,
#'          erecord_01_col = erecord_01,
#'          incident_date_col = incident_date,
#'          patient_DOB_col = patient_dob,
#'          epatient_15_col = epatient_15,
#'          epatient_16_col = epatient_16,
#'          eresponse_05_col = eresponse_05,
#'          eprocedures_01_col = eprocedures_01,
#'          eprocedures_02_col = eprocedures_02,
#'          eprocedures_03_col = eprocedures_03,
#'          eprocedures_05_col = eprocedures_05,
#'          eprocedures_06_col = eprocedures_06,
#'          earrest_01_col = earrest_01,
#'          evitals_01_col = evitals_01,
#'          evitals_06_col = evitals_06,
#'          evitals_12_col = evitals_12
#'          )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS
#'
#' @export
#'
airway_01_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      arrest_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
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
                      eprocedures_06_col
                      ) {


  # Check for tables or DF

  if(
    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(procedures_table),
      !is.null(vitals_table)
    ) && !is.null(df)
  ) {

    cli::cli_abort("{.fn airway_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")

  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(arrest_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn airway_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
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

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn airway_01_population}.")

  }

  ###### CODES ######

  procedures_code <- paste("673005|Indirect laryngoscopy",
                           "49077009|Flexible fiberoptic laryngoscopy",
                           "78121007|Direct laryngoscopy",
                           "112798008|Insertion of endotracheal tube",
                           "16883004|Endotracheal intubation, emergency procedure",
                           "182682004|Emergency laryngeal intubation",
                           "232674004|Orotracheal intubation",
                           "232677006|Tracheal intubation using rigid bronchoscope",
                           "232678001|Orotracheal fiberoptic intubation",
                           "232679009|Nasotracheal intubation",
                           "232682004|Nasotracheal fiberoptic intubation",
                           "232680007|Nasal intubation awake",
                           "241689008|Intubation, Rapid Sequence Intubation (RSI)",
                           "304341005|Awake intubation",
                           "397892004|Retrograde intubation",
                           "429161001|Insertion of endotracheal tube using laryngoscope",
                           "450601000124103|Orotracheal intubation using bougie device",
                           "1141752008|Flexible video intubation laryngoscope",
                           "285696003|Fiberoptic laryngoscope",
                           "420311007|Flexible fiberoptic laryngoscope",
                           "421100004|Rigid fiberoptic laryngoscope",
                           "44738004|Laryngoscope device",
                           "469919007|Flexible video laryngoscope",
                           "700640001|Rigid intubation laryngoscope",
                           "701054002|Flexible fiberoptic intubation laryngoscope",
                           "706013009|Intubation laryngoscope",
                           "734928009|Rigid non-bladed video intubation laryngoscope",
                           "879788006|Channeled video intubation laryngoscope",
                           sep="|")

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  year_values <- "2516009|years"

  day_values <- "days|2516001"

  hour_values <- "hours|2516003"

  minute_values <- "minutes|2516005"

  month_values <- "months|2516007"

  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")

  options(cli.progress_bar_style = list(
    complete = cli::col_green("\u25CF"),  # Black Circle
    incomplete = cli::col_br_white("\u2500")  # Light Horizontal Line
  ))

  # initiate the progress bar process
  progress_bar_population <- cli::cli_progress_bar(
    "Running `airway_01_population()`",
    total = 19,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

  ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING #######

  if(all(
    is.null(patient_scene_table),
    is.null(response_table),
    is.null(arrest_table),
    is.null(procedures_table),
    is.null(vitals_table)
  ) && !is.null(df)

  # utilize a dataframe to analyze the data for the measure analytics

  ) {

    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
          "i" = "The passed object is of class {.val {class(df)}}."
        )
      )
    }

    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_DOB_col)

      if (
        (!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }

    # Use quasiquotation on the vitals procedures datetime fields
    vitals_datetime <- rlang::enquo(evitals_01_col)
    procedures_datetime <- rlang::enquo(eprocedures_01_col)

      # Validate the datetime fields in the df
      if ((!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {

        cli::cli_abort(
          "For the variables {.var eprocedures_01_col} and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col} {.var evitals_01_col} to class {.cls Date} or a similar class."
        )

      }

    # make tables from df
    # patient
    patient_scene_table <- df |>
      dplyr::select(-{{ evitals_01_col }},
                    -{{ evitals_06_col }},
                    -{{ evitals_12_col }},
                    -{{ earrest_01_col }},
                    -{{ eprocedures_01_col }},
                    -{{ eprocedures_02_col }},
                    -{{ eprocedures_03_col }},
                    -{{ eprocedures_05_col }},
                    -{{ eprocedures_06_col }},
                    -{{ eresponse_05_col }}
                    ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response
    response_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ eresponse_05_col }}
                    ) |>
      dplyr::distinct()

    # arrest
    arrest_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ earrest_01_col }}
                    ) |>
      dplyr::distinct()

    # vitals
    vitals_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ evitals_01_col }},
                    {{ evitals_06_col }},
                    {{ evitals_12_col }}
                    ) |>
      dplyr::distinct()

    # procedures
    procedures_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ eprocedures_01_col }},
                    {{ eprocedures_02_col }},
                    {{ eprocedures_03_col }},
                    {{ eprocedures_05_col }},
                    {{ eprocedures_06_col }}
                    ) |>
      dplyr::distinct()

    if (all(is.na(procedures_table[[rlang::as_name(rlang::enquo(eprocedures_03_col))]]))) {
      cli::cli_warn("eprocedures_03_col is entirely missing. Returning an empty result.")
      return(dplyr::tibble())
    }


  } else if( # else continue with the tables passed to the applicable arguments

    all(
    !is.null(patient_scene_table),
    !is.null(response_table),
    !is.null(arrest_table),
    !is.null(procedures_table),
    !is.null(vitals_table)
  ) && is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(procedures_table) || tibble::is_tibble(procedures_table),
        is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table),
        is.data.frame(arrest_table) || tibble::is_tibble(arrest_table)

      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn airway_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
      )

    }

    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {

      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_DOB_col)

      if (
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_dob)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_dob)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }

    # Use quasiquotation on the vitals, airway, and procedures datetime fields
    vitals_datetime <- rlang::enquo(evitals_01_col)
    procedures_datetime <- rlang::enquo(eprocedures_01_col)

    # Validate the datetime fields in the patient_scene_table
    if ((!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
         !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
        (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
         !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {

      cli::cli_abort(
        "For the variables {.var eprocedures_01_col} and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col} and {.var evitals_01_col} to class {.cls Date} or a similar class."
      )

    }

    # get distinct tables when passed to table arguments
    # patient
    patient_scene_table <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response
    response_table <- response_table |>
      dplyr::select({{ erecord_01_col }},
                    {{ eresponse_05_col }}
                    ) |>
      dplyr::distinct()

    # arrest
    arrest_table <- arrest_table |>
      dplyr::select({{ erecord_01_col }},
                    {{ earrest_01_col }}
                    ) |>
      dplyr::distinct()

    # vitals
    vitals_table <- vitals_table |>
      dplyr::select({{ erecord_01_col }},
                    {{ evitals_01_col }},
                    {{ evitals_06_col }},
                    {{ evitals_12_col }}
                    ) |>
      dplyr::distinct()

    # procedures
    procedures_table <- procedures_table |>
      dplyr::select({{ erecord_01_col }},
                    {{ eprocedures_01_col }},
                    {{ eprocedures_02_col }},
                    {{ eprocedures_03_col }},
                    {{ eprocedures_05_col }},
                    {{ eprocedures_06_col }}
                    ) |>
      dplyr::distinct()

    if (all(is.na(procedures_table[[rlang::as_name(rlang::enquo(eprocedures_03_col))]]))) {
      cli::cli_warn("eprocedures_03_col is entirely missing. Returning an empty result.")
      return(dplyr::tibble())
    }

  }

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

  # Get first intubation procedure & time intervals for vitals
  suppressWarnings( # needed due to ranking procedure and missing procedure times)

  procedures_table |>
    dplyr::filter(!is.na({{ eprocedures_03_col }})) |>
    dplyr::mutate(
      non_missing_procedure_time = !is.na({{ eprocedures_01_col }}), # Procedure date/time not null
      not_performed_prior = !grepl(pattern = "9923003|Yes", x = {{ eprocedures_02_col }}, ignore.case = TRUE) | is.na({{ eprocedures_02_col }}), # Procedure PTA is not Yes
      target_procedures = grepl(pattern = procedures_code, x = {{ eprocedures_03_col }}, ignore.case = TRUE) # Procedure name/code in list
    ) |>
    dplyr::mutate(
      vitals_range_start = {{ eprocedures_01_col }} - lubridate::dminutes(3),
      vitals_range_end = {{ eprocedures_01_col }} + lubridate::dminutes(5),
      range_bounds_before = lubridate::interval(vitals_range_start, {{ eprocedures_01_col }}),
      range_bounds_after = lubridate::interval({{ eprocedures_01_col }}, vitals_range_end),
      successful_procedure = grepl(pattern = "9923003|Yes", x = {{ eprocedures_06_col }}, ignore.case = TRUE),
      {{ eprocedures_05_col }} := dplyr::if_else(is.na({{ eprocedures_05_col }}), 1, {{ eprocedures_05_col }}),
      first_attempt = {{ eprocedures_05_col }} == 1,
      first_successful_attempt = first_attempt & successful_procedure

                  )) -> procedures_ordered

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

  # conditionally perform age in years calculations
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

  # Add calculated age in years
  patient_data <- patient_scene_table |>
    dplyr::mutate(
      CLEANED_AGE_UNITS = dplyr::case_when(
        tolower({{ epatient_16_col }}) %in% c("seconds", "minutes", "hours", "days", "weeks", "months", "years") ~
          tolower({{ epatient_16_col }}),
        TRUE ~ "years" # Replace invalid units with NA
      ),
      {{ patient_DOB_col }} := dplyr::if_else(
        is.na({{ patient_DOB_col }}) &
          !is.na({{ epatient_15_col }}) &
          !is.na(CLEANED_AGE_UNITS),
        {{ incident_date_col }} - dplyr::case_when(
          CLEANED_AGE_UNITS == "years"  ~ lubridate::dyears({{ epatient_15_col }}),
          CLEANED_AGE_UNITS == "months" ~ lubridate::dmonths({{ epatient_15_col }}),
          CLEANED_AGE_UNITS == "weeks"  ~ lubridate::dweeks({{ epatient_15_col }}),
          CLEANED_AGE_UNITS == "days"   ~ lubridate::ddays({{ epatient_15_col }}),
          CLEANED_AGE_UNITS == "hours"  ~ lubridate::dhours({{ epatient_15_col }}) / 24, # Convert to days
          CLEANED_AGE_UNITS == "minutes" ~ lubridate::dminutes({{ epatient_15_col }}) / (24 * 60), # Convert to days
          CLEANED_AGE_UNITS == "seconds" ~ lubridate::dseconds({{ epatient_15_col }}) / (24 * 3600) # Convert to days
        ),
        {{ patient_DOB_col }}
      )
    ) |>
    dplyr::mutate(patient_age_in_years = as.numeric(difftime({{ incident_date_col }},
                                                             {{ patient_DOB_col }},
                                                             units = "days")/365)) |>
    dplyr::mutate(patient_age_in_years = dplyr::case_when(!is.na(patient_age_in_years) ~ patient_age_in_years,
                                                          grepl(pattern = year_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ {{ epatient_15_col }},

                                                          grepl(pattern = month_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ {{ epatient_15_col }} / 12,

                                                          grepl(pattern = day_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ {{ epatient_15_col }} / 365,

                                                          grepl(pattern = hour_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ {{ epatient_15_col }} / (365*24),

                                                          grepl(pattern = minute_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ {{ epatient_15_col }} / (365*24*60)
                                                          ))

  } else if ( # condition where the user does not pass the incident date nor the patient DOB
    all(
      rlang::quo_is_null(rlang::enquo(incident_date_col)),
      rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

    # Add calculated age in years
    patient_data <- patient_scene_table |>
      dplyr::mutate(patient_age_in_years = dplyr::case_when(grepl(pattern = year_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ {{ epatient_15_col }},

                                                            grepl(pattern = month_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ {{ epatient_15_col }} / 12,

                                                            grepl(pattern = day_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ {{ epatient_15_col }} / 365,

                                                            grepl(pattern = hour_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ {{ epatient_15_col }} / (365*24),

                                                            grepl(pattern = minute_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ {{ epatient_15_col }} / (365*24*60)
                                                            ))

  }

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

  # Get 911 responses
  response_table |>
    dplyr::mutate(call_911 = grepl(pattern = codes_911,
                                   x = {{ eresponse_05_col }}, ignore.case = TRUE)) |>
    dplyr::filter(call_911) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }}) -> call_911_data

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

  # get arrest table with needed exclusion classifications
  arrest_table_filter <- arrest_table |>
    dplyr::mutate(exclude_pta_ca = !grepl(pattern = "3001003|Yes, Prior",
                                          x = {{ earrest_01_col }}, ignore.case = TRUE)) |>
    dplyr::filter(exclude_pta_ca) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

  # Initial manipulations of patient data for filters
  patient_data |>
    dplyr::mutate(call_911 = {{ erecord_01_col }} %in% call_911_data,
                  exclude_newborns = patient_age_in_years > 1/365,
                  adult_population = patient_age_in_years >= 18,
                  pedi_population = patient_age_in_years < 18,
                  exclude_pta_ca = {{ erecord_01_col }} %in% arrest_table_filter,
                  patient_age_in_years_1_9 = patient_age_in_years >= 1 & patient_age_in_years <= 9,
                  patient_age_in_years_10_plus = patient_age_in_years >= 10,
                  patient_age_in_years_multiplied = dplyr::if_else(patient_age_in_years < 10,
                                                                   floor(patient_age_in_years) * 2, 1),
                  patient_age_in_days = patient_age_in_years * 365,
                  patient_age_28_days_1_year = patient_age_in_days < 365 & patient_age_in_days > 28,
                  patient_age_days_28 = patient_age_in_days < 28,
                  SBP_threshold = dplyr::if_else(patient_age_in_years_10_plus, 90,
                                                 dplyr::if_else(patient_age_in_years_1_9, 70 + patient_age_in_years_multiplied,
                                                                dplyr::if_else(patient_age_28_days_1_year, 70,
                                                                               dplyr::if_else(patient_age_days_28, 60, 90)
                                                                )
                                                 )
                  )) -> patient_data

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # Numerator
  # utilized the procedures table with ranked, non-missing eprocedures.03
  # which will return mostly non-missing eprocedures.01
  ###_____________________________________________________________________________

  # get record IDs for the vitals table
  procedures_ID <- procedures_ordered |>
    dplyr::filter(target_procedures) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # get applicable vitals
  # only those that match patients in the
  # procedures table with the target procedures
  vitals_table_filter <- vitals_table |>
    dplyr::filter({{ erecord_01_col }} %in% procedures_ID,
                  !is.na({{ evitals_06_col }}),
                  !is.na({{ evitals_12_col }})
                  )

  ###_____________________________________________________________________________
  # this is the initial table for calculating
  # setup process
  ###_____________________________________________________________________________

  suppressWarnings( # warnings can pop up related to left join relationships

    procedures_ordered |>
    dplyr::filter(target_procedures) |>
    dplyr::left_join(patient_data |>
                       dplyr::select({{ erecord_01_col }},
                                     matches("patient_age"),
                                     SBP_threshold
                                     ), by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::left_join(vitals_table, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::mutate(within_range_before = do.call(lubridate::`%within%`, list({{ evitals_01_col }}, range_bounds_before)),
                  within_range_after = do.call(lubridate::`%within%`, list({{ evitals_01_col }}, range_bounds_after))

    )) -> calculating_table

  ###_____________________________________________________________________________
  # break calculations up into different tables and then join
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # numerator part 1 for all patients
  ###_____________________________________________________________________________

  suppressWarnings( # in case max() does not find non-missing groups

    calculating_table |>
      dplyr::summarize(

        # numerator 1 part 1 for all patients with pulse oximetry >= 90
        # numerator 2 part 1 for all patients with pulse oximetry >= 90
        numerator1_all_spo2 = max(first_successful_attempt & within_range_before & {{ evitals_12_col }} >= 90, na.rm = TRUE),
        numerator2_all_spo2 = max(first_successful_attempt & within_range_after & {{ evitals_12_col }} >= 90, na.rm = TRUE),

        .by = c({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }})
      )) -> computing_population_dev1
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # numerator 1 part 2 for age specific SBP
  ###_____________________________________________________________________________

  suppressWarnings( # in case max() does not find non-missing groups

    calculating_table |>
      dplyr::summarize(
        # start calculation of numerator 1 part 2, age specific SBP
        numerator1_sbp = max(first_successful_attempt & within_range_before & {{ evitals_06_col }} >= SBP_threshold, na.rm = TRUE),
        numerator2_sbp = max(first_successful_attempt & within_range_after & {{ evitals_06_col }} >= SBP_threshold, na.rm = TRUE),

        .by = c({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }})
  )) -> computing_population_dev2
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # numerator 2, SBP check for all ages
  ###_____________________________________________________________________________

  suppressWarnings(

    calculating_table |>
      dplyr::summarize(
        # start numerator 2 part 2 calculations, sbp >= 90 among all patients
        numerator1_all_sbp = max(first_successful_attempt & within_range_before & {{ evitals_06_col }} >= 90, na.rm = TRUE),
        numerator2_all_sbp = max(first_successful_attempt & within_range_after & {{ evitals_06_col }} >= 90, na.rm = TRUE),

        .by = c({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }})
      )) -> computing_population_dev3
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # join the computations to finalize calculations
  ###_____________________________________________________________________________

  computing_population_dev1 |>

    dplyr::left_join(computing_population_dev2, by = dplyr::join_by({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col}})
                     ) |>

    dplyr::left_join(computing_population_dev3, by = dplyr::join_by({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col}})

                     ) -> computing_population_dev
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # finalize numerator calculations
  ###_____________________________________________________________________________

  computing_population_dev |>
      dplyr::mutate(across(matches("numerator"), ~ dplyr::if_else(is.na(.) | is.infinite(.), 0, .)),

                    # final numerator 1 part 1
                    numerator_all_spo2 = as.integer((numerator1_all_spo2 + numerator2_all_spo2) > 1),

                    # final numerator 1 part 2
                    numerator_sbp_age = as.integer((numerator1_sbp + numerator2_sbp) > 1),

                    # final numerator 1
                    numerator_1 = as.integer((numerator_all_spo2 + numerator_sbp_age) > 1),

                    # numerator 2 part 2 final
                    numerator_all_sbp = as.integer((numerator1_all_sbp + numerator2_all_sbp) > 1),

                    # numerator 2 final
                    numerator_2 = as.integer((numerator_all_spo2 + numerator_all_sbp) > 1)

                    ) -> computing_population_dev
  ###_____________________________________________________________________________

    cli::cli_progress_update(set = 14, id = progress_bar_population, force = TRUE)

    ###_____________________________________________________________________________
    # final join for the computing population
    # will have the same number of rows as the
    # initial procedures table if ran through dplyr::distinct()
    ###_____________________________________________________________________________

    computing_population <- suppressWarnings( # warnings can pop up related to left join relationships

      procedures_ordered |>
      dplyr::left_join(computing_population_dev, by = dplyr::join_by({{ erecord_01_col }},
                                                                     {{ eprocedures_01_col }},
                                                                     {{ eprocedures_03_col }})
                       ) |>
        dplyr::left_join(patient_data, by = dplyr::join_by({{ erecord_01_col }}))

    )

    ###_____________________________________________________________________________

    cli::cli_progress_update(set = 15, id = progress_bar_population, force = TRUE)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        target_procedures,

        call_911

      )

    cli::cli_progress_update(set = 16, id = progress_bar_population, force = TRUE)

    # get the adult population
    adult_pop <- initial_population |>
      dplyr::filter(adult_population,
                    not_performed_prior,
                    non_missing_procedure_time,
                    exclude_pta_ca
                    ) |>

      # get the first procedure
      # some patients have the same time on more than one procedure's data
      dplyr::filter({{ eprocedures_01_col }} == min({{ eprocedures_01_col }}, na.rm = TRUE),
                    .by = {{ erecord_01_col }}
                    ) |>

      # upon investigation, once you get the earliest airway time, the
      # performance on all procedures with that time will be the same
      # so to get the correct count of first airways, we run dplyr::distinct
      # down to the erecord.01 level
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    cli::cli_progress_update(set = 17, id = progress_bar_population, force = TRUE)

    # get the peds population
    peds_pop <- initial_population |>
      dplyr::filter(pedi_population,
                    not_performed_prior,
                    non_missing_procedure_time,
                    exclude_pta_ca,
                    exclude_newborns
                    ) |>

      # get the first procedure
      # some patients have the same time on more than one procedure's data
      dplyr::filter({{ eprocedures_01_col }} == min({{ eprocedures_01_col }}, na.rm = TRUE),
                    .by = {{ erecord_01_col }}
                    ) |>

      # upon investigation, once you get the earliest airway time, the
      # performance on all procedures with that time will be the same
      # so to get the correct count of first airways, we run dplyr::distinct
      # down to the erecord.01 level
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    cli::cli_progress_update(set = 18, id = progress_bar_population, force = TRUE)

    # get total number of procedures examined
    n_procedures <- procedures_ordered |>
      dplyr::filter(!is.na({{ eprocedures_03_col }})) |>
      dplyr::distinct({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }}) |>
      nrow()

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Invasive airway procedures",
                 "Successful invasive airway procedures",
                 "First attempt successful invasive airway procedures",
                 "911 calls",
                 "Excluded cardiac arrests",
                 "Excluded newborns",
                 "All initial population successful intubation with no hypoxia",
                 "All initial population successful intubation with no hypotension",
                 "Initial population ages >= 10 yrs successful intubation with no hypoxia/hypotension",
                 "Initial population ages 1-9 yrs successful intubation with no hypoxia/hypotension",
                 "Initial population ages < 1 yrs & > 28 days successful intubation with no hypoxia/hypotension",
                 "Initial population ages < 28 days successful intubation with no hypoxia/hypotension",
                 "All initial population successful intubation with no hypoxia or hypoxia/hypotension",
                 "Adults successful intubation no hypoxia or hypotension",
                 "Peds successful intubation no hypoxia or hypotension",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial Population",
                 "Total procedures in dataset"
      ),
      count = c(
        sum(procedures_ordered$target_procedures, na.rm = TRUE),
        sum(procedures_ordered$target_procedures & procedures_ordered$successful_procedure, na.rm = TRUE),
        sum(procedures_ordered$first_successful_attempt & procedures_ordered$target_procedures, na.rm = TRUE),
        length(call_911_data),
        sum(patient_data$exclude_pta_ca == FALSE, na.rm = TRUE),
        sum(patient_data$exclude_newborns == FALSE, na.rm = TRUE),
        sum(initial_population$numerator_all_spo2, na.rm = TRUE),
        sum(initial_population$numerator_all_sbp, na.rm = TRUE),
        sum(initial_population$numerator_1 & initial_population$patient_age_in_years_10_plus, na.rm = TRUE),
        sum(initial_population$numerator_1 & initial_population$patient_age_in_years_1_9, na.rm = TRUE),
        sum(initial_population$numerator_1 & initial_population$patient_age_28_days_1_year, na.rm = TRUE),
        sum(initial_population$numerator_1 & initial_population$patient_age_days_28, na.rm = TRUE),
        sum(initial_population$numerator_1, na.rm = TRUE),
        sum(adult_pop$numerator_1, na.rm = TRUE),
        sum(peds_pop$numerator_1, na.rm = TRUE),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        n_procedures
      )
    )

  cli::cli_progress_update(set = 19, id = progress_bar_population, force = TRUE)

  # Get populations
  airway.01.population <- list(
    filter_process = filter_counts,

    adults = adult_pop,

    peds = peds_pop,

    initial_population = initial_population,

    computing_population = computing_population

  )

  cli::cli_progress_done(id = progress_bar_population)

  return(airway.01.population)

}
