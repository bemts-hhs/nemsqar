#' @title Airway-18 Populations
#'
#' @description
#'
#' This function processes and analyzes the dataset to generate the populations
#' of interest needed to perform calculations to obtain performance data.
#'
#' @param df A data frame or tibble containing the dataset to be processed.
#'   Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only ePatient
#'   and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eResponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A data frame or tibble containing only the
#'   eProcedures fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param airway_table A data frame or tibble containing only the eAirway fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the eVitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name containing the unique patient record
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name for patient information (exact purpose
#'   unclear).
#' @param epatient_16_col Column name for patient information (exact purpose
#'   unclear).
#' @param eresponse_05_col Column name for emergency response codes.
#' @param eprocedures_01_col Column name for procedure times or other related
#'   data.
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col Column name for procedure codes.
#' @param eprocedures_06_col Column name for procedure success codes.
#' @param eairway_02_col Column name for airway procedure data (datetime).
#'   Default is `NULL`.
#' @param eairway_04_col Column name for airway procedure data. Default is
#'   `NULL`.
#' @param evitals_01_col Column name for vital signs data (datetime).
#' @param evitals_16_col Column name for additional vital signs data.
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
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 23:02:00",
#'     "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
#'     "2025-06-01 13:01:00", "2025-01-01 23:02:00",
#'     "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
#'     "2025-06-01 13:06:00")),
#'     evitals_16 = rep(c(5, 6, 7, 8, 9), 2)
#'
#'   )
#'
#'   # airway table
#'   airway_table <- tibble::tibble(
#'   erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'   eairway_02 = rep(lubridate::as_datetime(c("2025-01-01 23:05:00",
#'     "2025-01-05 12:02:00", "2025-02-01 19:03:00", "2025-01-01 05:04:00",
#'     "2025-06-01 13:06:00")), 2),
#'   eairway_04 = rep(4004019, 10)
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
#'     eprocedures_06 = rep(9923003, 10)
#'
#'   )
#'
#' # Run the function
#' result <- airway_18_population(df = NULL,
#'          patient_scene_table = patient_table,
#'          procedures_table = procedures_table,
#'          vitals_table = vitals_table,
#'          response_table = response_table,
#'          airway_table = airway_table,
#'          erecord_01_col = erecord_01,
#'          incident_date_col = incident_date,
#'          patient_DOB_col = patient_dob,
#'          epatient_15_col = epatient_15,
#'          epatient_16_col = epatient_16,
#'          eresponse_05_col = eresponse_05,
#'          eprocedures_01_col = eprocedures_01,
#'          eprocedures_02_col = eprocedures_02,
#'          eprocedures_03_col = eprocedures_03,
#'          eprocedures_06_col = eprocedures_06,
#'          evitals_01_col = evitals_01,
#'          evitals_16_col = evitals_16,
#'          eairway_02_col = eairway_02,
#'          eairway_04_col = eairway_04
#'          )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
#' @author Nicolas Foss, Ed.D., MS, Samuel Kordik, BBA, BS
#'
#' @export
#'
airway_18_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      airway_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_06_col,
                      eairway_02_col = NULL,
                      eairway_04_col = NULL,
                      evitals_01_col,
                      evitals_16_col) {

    # Ensure that not all table arguments AND the df argument are fulfilled
    # User must pass either `df` or all table arguments, but not both

    if (
      any(
        !is.null(patient_scene_table),
        !is.null(procedures_table),
        !is.null(vitals_table),
        !is.null(airway_table),
        !is.null(response_table)
      ) &&
      !is.null(df)
    ) {
      cli::cli_abort("{.fn airway_18_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
    }

    # Ensure that df or all table arguments are fulfilled

    if (
      all(
        is.null(patient_scene_table),
        is.null(procedures_table),
        is.null(vitals_table),
        is.null(airway_table),
        is.null(response_table)
      ) &&
      is.null(df)
    ) {
      cli::cli_abort("{.fn airway_18_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
    }

    # Ensure all *_col arguments are fulfilled

    if (
      any(
        missing(erecord_01_col),
        missing(incident_date_col),
        missing(patient_DOB_col),
        missing(epatient_15_col),
        missing(epatient_16_col),
        missing(eresponse_05_col),
        missing(eprocedures_01_col),
        missing(eprocedures_02_col),
        missing(eprocedures_03_col),
        missing(eprocedures_06_col),
        missing(eairway_02_col),
        missing(eairway_04_col),
        missing(evitals_01_col),
        missing(evitals_16_col)
      )
    ) {
      cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn airway_18_population}.")
    }

    # 911 codes for eResponse.05
    codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

    # endotracheal intubation attempts
    endotracheal_intubation <- "673005|Indirect laryngoscopy|49077009|Flexible fiberoptic laryngoscopy|78121007|Direct laryngoscopy|112798008|Insertion of endotracheal tube|16883004|Endotracheal intubation, emergency procedure|182682004|Emergency laryngeal intubation|232674004|Orotracheal intubation|232677006|Tracheal intubation using rigid bronchoscope|232678001|Orotracheal fiberoptic intubation|232679009|Nasotracheal intubation|232682004|Nasotracheal fiberoptic intubation|232680007|Nasal intubation awake|241689008|Rapid sequence induction|304341005|Awake intubation|397892004|Retrograde intubation|418613003|Tracheal intubation through a laryngeal mask airway|429705000|Intubation, combitube|424979004|Laryngeal mask airway insertion|427753009|Insertion of esophageal tracheal double lumen supraglottic airway|429161001|Insertion of endotracheal tube using laryngoscope|450601000124103|Orotracheal intubation using bougie device|450611000124|Insertion of Single Lumen Supraglottic Airway Device|1141752008|Flexible video intubation laryngoscope |285696003|Fiberoptic laryngoscope |420311007|Flexible fiberoptic laryngoscope |421100004|Rigid fiberoptic laryngoscope |44738004|Laryngoscope device |469919007|Flexible video laryngoscope |700640001|Rigid intubation laryngoscope |701054002|Flexible fiberoptic intubation laryngoscope |706013009|Intubation laryngoscope |734928009|Rigid non-bladed video intubation laryngoscope |879788006|Channeled video intubation laryngoscope "

    # waveform ETCO2
    waveform_etc02_codes <- "4004019|Waveform ETCO2"

    # answer yes!
    yes_code <- "9923003|Yes"

    # minor values
    minor_values <- "days|hours|minutes|months"

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

    # Initialize the progress bar
    progress_bar_population <- cli::cli_progress_bar(
      "Running `airway_18_population()`",
      total = 13,
      type = "tasks",
      clear = FALSE,
      format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
    )

    # use quasiquotation to validate the
    # airway placement confirmation method column
    airway_confirmation_col <- rlang::enquo(eairway_04_col)

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

    ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING #######

    if (all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(airway_table),
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

      # Use quasiquotation on the vitals, airway, and procedures datetime fields
      vitals_datetime <- rlang::enquo(evitals_01_col)
      procedures_datetime <- rlang::enquo(eprocedures_01_col)
      airway_datetime <- rlang::enquo(eairway_02_col)

      if (!rlang::quo_is_null(airway_datetime)) {

        # Validate the datetime fields in the df
        if ((!lubridate::is.Date(df[[rlang::as_name(airway_datetime)]]) &
             !lubridate::is.POSIXct(df[[rlang::as_name(airway_datetime)]])) ||
             (!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
             !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
            (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
             !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {

          cli::cli_abort(
            "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
          )

        }

      } else if (rlang::quo_is_null(airway_datetime)) {

        # Validate the datetime fields in the df
        if ((!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
             !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
            (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
             !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {

          cli::cli_abort(
            "For the variables {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
          )

        }
      }

      # make tables from df
      # patient
      patient_scene_table <- df |>
        dplyr::select(-{{ evitals_01_col }},
                      -{{ evitals_16_col }},
                      -{{ eairway_02_col }},
                      -{{ eairway_04_col }},
                      -{{ eprocedures_01_col }},
                      -{{ eprocedures_02_col }},
                      -{{ eprocedures_03_col }},
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

      # optionally use eairway fields
      if (

        all(
          !rlang::quo_is_null(airway_datetime),
          !rlang::quo_is_null(airway_confirmation_col)
        )

      ) {

      # airway
      airway_table <- df |>
        dplyr::select({{ erecord_01_col }},
                      {{ eairway_02_col }},
                      {{ eairway_04_col }}
        ) |>
        dplyr::distinct()

      }

      # vitals
      vitals_table <- df |>
        dplyr::select({{ erecord_01_col }},
                      {{ evitals_01_col }},
                      {{ evitals_16_col }}
        ) |>
        dplyr::distinct()

      # procedures
      procedures_table <- df |>
        dplyr::select({{ erecord_01_col }},
                      {{ eprocedures_01_col }},
                      {{ eprocedures_02_col }},
                      {{ eprocedures_03_col }},
                      {{ eprocedures_06_col }}
        ) |>
        dplyr::distinct()

      if (all(is.na(procedures_table[[rlang::as_name(rlang::enquo(eprocedures_03_col))]]))) {
        cli::cli_warn("eprocedures_03_col is entirely missing. Returning an empty result.")
        return(tibble::tibble())
      }


    } else if ( # else continue with the tables passed to the applicable arguments

      all(
        !is.null(patient_scene_table),
        !is.null(response_table),
        !is.null(airway_table),
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
          is.data.frame(airway_table) || tibble::is_tibble(airway_table)

        )

      ) {

        cli::cli_abort(
          "One or more of the tables passed to {.fn airway_18_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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
      airway_datetime <- rlang::enquo(eairway_02_col)

      if (!rlang::quo_is_null(airway_datetime)) {

        # Validate the datetime fields in the df
        if ((!lubridate::is.Date(airway_table[[rlang::as_name(airway_datetime)]]) &
             !lubridate::is.POSIXct(airway_table[[rlang::as_name(airway_datetime)]])) ||
            (!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
             !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
            (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
             !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {

          cli::cli_abort(
            "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
          )

        }

      } else if (rlang::quo_is_null(airway_datetime)) {

        # Validate the datetime fields in the df
        if ((!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
             !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
            (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
             !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {

          cli::cli_abort(
            "For the variables {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
          )

        }
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

      # airway
      airway_table <- airway_table |>
        dplyr::select({{ erecord_01_col }},
                      {{ eairway_02_col }},
                      {{ eairway_04_col }}
        ) |>
        dplyr::distinct()

      # vitals
      vitals_table <- vitals_table |>
        dplyr::select({{ erecord_01_col }},
                      {{ evitals_01_col }},
                      {{ evitals_16_col }}
        ) |>
        dplyr::distinct()

      # procedures
      procedures_table <- procedures_table |>
        dplyr::select({{ erecord_01_col }},
                      {{ eprocedures_01_col }},
                      {{ eprocedures_02_col }},
                      {{ eprocedures_03_col }},
                      {{ eprocedures_06_col }}
        ) |>
        dplyr::distinct()

      if (all(is.na(procedures_table[[rlang::as_name(rlang::enquo(eprocedures_03_col))]]))) {
        cli::cli_warn("eprocedures_03_col is entirely missing. Returning an empty result.")
        return(tibble::tibble())
      }


    }

    cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

      ###_____________________________________________________________________________
      ### calculations of the numerator and filtering
      ###_____________________________________________________________________________

        procedures_table |>
          dplyr::filter(!is.na({{ eprocedures_03_col }})) |>
          dplyr::mutate(
            non_missing_procedure_time = !is.na({{ eprocedures_01_col }}), # Procedure date/time not null
            not_performed_prior = !grepl(pattern = yes_code, x = {{ eprocedures_02_col }}, ignore.case = TRUE) | is.na({{ eprocedures_02_col }}), # Procedure PTA is not Yes
            target_procedures = grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = TRUE), # Procedure name/code in list
            successful_procedure = grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = TRUE)
          ) -> procedures_ordered

      cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

      # 911 calls
      call_911_data <- response_table |>
        dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = TRUE)

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

      ###_____________________________________________________________________________
      # fact table
      # the user should ensure that variables beyond those supplied for calculations
      # are distinct (i.e. one value or cell per patient)
      ###_____________________________________________________________________________

      # conditionally perform age in years calculations
      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
        )
      ) {

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
          dplyr::mutate(call_911 = {{ erecord_01_col }} %in% call_911_data,
                        patient_age_in_years = as.numeric(difftime({{ incident_date_col }},
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
          dplyr::mutate(call_911 = {{ erecord_01_col }} %in% call_911_data,
                        patient_age_in_years = dplyr::case_when(grepl(pattern = year_values,
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

      cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

      # get applicable vitals
      # only those that match patients in the
      # procedures table with the target procedures
      vitals_table_filter <- vitals_table |>
        dplyr::filter(!is.na({{ evitals_16_col }}),
                      {{ erecord_01_col }} %in% procedures_ID
                      )

      # get total waveform ETC02 measurements >= 5
      waveform_etc02 <- vitals_table_filter |>
        dplyr::filter({{ evitals_16_col }} >= 5) |>
        nrow()

      cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

      # optionally use eairway fields
      if (

        all(
          !rlang::quo_is_null(airway_datetime),
          !rlang::quo_is_null(airway_confirmation_col)
        )

      ) {

        # get applicable airway data
        # only those that match patients in the
        # procedures table with the target procedures
        airway_table_filter <- airway_table |>
          dplyr::filter(!is.na({{ eairway_04_col }}),
                        {{ erecord_01_col }} %in% procedures_ID
                        )

        # total cases where waveform ETC02 airway confirmation
        # is documented
        airway_etc02_confirmations <- airway_table_filter |>
          dplyr::filter(grepl(pattern = waveform_etc02_codes, x = {{ eairway_04_col }}, ignore.case = TRUE)) |>
          nrow()

        ###_____________________________________________________________________________
        # this is the initial table for calculating
        # setup process
        ###_____________________________________________________________________________

        # airway data
        suppressWarnings( # warnings can pop up related to left join relationships

          procedures_ordered |>
            dplyr::filter(target_procedures) |>
            dplyr::left_join(airway_table_filter, by = dplyr::join_by({{ erecord_01_col }})) |>
            dplyr::left_join(vitals_table_filter, by = dplyr::join_by({{ erecord_01_col }})) |>
            dplyr::mutate(waveform_etc02_used = grepl(pattern = waveform_etc02_codes, x = {{ eairway_04_col }}, ignore.case = TRUE),
                          airway_after_procedure = {{ eairway_02_col}} >= {{ eprocedures_01_col }},
                          airway_after_procedure_waveform = airway_after_procedure & waveform_etc02_used,
                          vitals_after_procedure = ({{ evitals_01_col }} >= {{ eprocedures_01_col }}),
                          waveform_etc02_5 = {{ evitals_16_col }} >= 5,
                          vitals_after_procedure_waveform = vitals_after_procedure & waveform_etc02_5
                            ) |>
            dplyr::summarize(

              waveform_etc02_used = max(waveform_etc02_used, na.rm = TRUE),
              airway_after_procedure = max(airway_after_procedure, na.rm = TRUE),
              airway_after_procedure_waveform = max(airway_after_procedure_waveform, na.rm = TRUE),
              vitals_after_procedure = max(vitals_after_procedure, na.rm = TRUE),
              waveform_etc02_5 = max(waveform_etc02_5, na.rm = TRUE),
              vitals_after_procedure_waveform = max(vitals_after_procedure_waveform, na.rm = TRUE),

              .by = c({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }})

            )) -> computing_population_dev

        # deal with NA or Inf values
        computing_population_dev <- computing_population_dev |>
          dplyr::mutate(dplyr::across(tidyselect::matches("after_procedure|waveform"), ~ dplyr::if_else(is.na(.) | is.infinite(.), 0, .)),
                        NUMERATOR = as.integer((vitals_after_procedure_waveform + airway_after_procedure_waveform) > 1)
                        )

      } else if ( # if no airway fields are passed

          all(
            rlang::quo_is_null(airway_datetime),
            rlang::quo_is_null(airway_confirmation_col)
          )

      ) {

        ###_____________________________________________________________________________
        # this is the initial table for calculating
        # setup process
        ###_____________________________________________________________________________

        # vitals data
        suppressWarnings( # warnings can pop up related to left join relationships

          procedures_ordered |>
            dplyr::filter(target_procedures) |>
            dplyr::left_join(vitals_table_filter, by = dplyr::join_by({{ erecord_01_col }})) |>
            dplyr::mutate(vitals_after_procedure = ({{ evitals_01_col }} >= {{ eprocedures_01_col }}),
                          waveform_etc02_5 = {{ evitals_16_col }} >= 5,
                          vitals_after_procedure_waveform = vitals_after_procedure & waveform_etc02_5
            ) |>
            dplyr::summarize(

              vitals_after_procedure_waveform = max(vitals_after_procedure_waveform, na.rm = TRUE),

              .by = c({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }})

            )) -> computing_population_dev

        # deal with NA or Inf values
        computing_population_dev <- computing_population_dev |>
          dplyr::mutate(vitals_after_procedure_waveform = dplyr::if_else(is.na(vitals_after_procedure_waveform) |
                                                                           is.infinite(vitals_after_procedure_waveform), 0, vitals_after_procedure_waveform),
                        NUMERATOR = as.integer(vitals_after_procedure_waveform > 0)
                        )
      }

      ###_____________________________________________________________________________
      # final join for the computing population
      # will have the same number of rows as the
      # initial procedures table if ran through dplyr::distinct()
      ###_____________________________________________________________________________

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

      computing_population <- suppressWarnings( # warnings can pop up related to left join relationships

        procedures_ordered |>
          dplyr::left_join(computing_population_dev, by = dplyr::join_by({{ erecord_01_col }},
                                                                         {{ eprocedures_01_col }},
                                                                         {{ eprocedures_03_col }})
          ) |>
          dplyr::left_join(patient_data, by = dplyr::join_by({{ erecord_01_col }}))

      )

      ###_____________________________________________________________________________

      cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

      # get the initial population
      initial_population <- computing_population |>
        dplyr::filter(

          target_procedures,

          successful_procedure,

          call_911

        )


      # Adult and Pediatric Populations

      cli::cli_progress_update(set = 10, id = progress_bar_population, force = TRUE)

        # filter adult
        adult_pop <- initial_population |>
          dplyr::filter(patient_age_in_years >= 18,
                        not_performed_prior,
                        non_missing_procedure_time
                        ) |>
          dplyr::filter({{ eprocedures_01_col }} == max({{ eprocedures_01_col }}, na.rm = TRUE),
                        .by = {{ erecord_01_col }}
                        ) |>
          dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

        cli::cli_progress_update(set = 11, id = progress_bar_population, force = TRUE)

        # filter peds
        peds_pop <- initial_population |>
          dplyr::filter(patient_age_in_years < 18,
                        not_performed_prior,
                        non_missing_procedure_time
                        ) |>
          dplyr::filter({{ eprocedures_01_col }} == max({{ eprocedures_01_col }}, na.rm = TRUE),
                        .by = {{ erecord_01_col }}
                        ) |>
          dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

      # summarize

      cli::cli_progress_update(set = 12, id = progress_bar_population, force = TRUE)

      # get total number of procedures examined
      n_procedures <- procedures_ordered |>
        dplyr::filter(!is.na({{ eprocedures_03_col }})) |>
        dplyr::distinct({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_03_col }}) |>
        nrow()


      # optionally use eairway fields
      if (

        all(
          !rlang::quo_is_null(airway_datetime),
          !rlang::quo_is_null(airway_confirmation_col)
        )

      ) {

        # summarize counts for populations filtered
        filter_counts <- tibble::tibble(
          filter = c("Invasive airway procedures",
                     "Successful invasive airway procedures",
                     "911 calls",
                     "Successful invasive airway procedures performed after EMS arrival",
                     "Waveform ETCO2 used",
                     "Airway device placement confirmed after airway procedure",
                     "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                     "Waveform ETCO2 >= 5",
                     "Successful invasive airway procedures with waveform ETCO2 confirmed post-procedure",
                     "Adults denominator",
                     "Peds denominator",
                     "Total procedures in dataset"
          ),
          count = c(
            sum(procedures_ordered$target_procedures, na.rm = TRUE),
            sum(procedures_ordered$target_procedures & procedures_ordered$successful_procedure, na.rm = TRUE),
            length(call_911_data),
            sum(procedures_ordered$target_procedures &
                  procedures_ordered$successful_procedure &
                  procedures_ordered$not_performed_prior, na.rm = TRUE),
            airway_etc02_confirmations,
            sum(computing_population_dev$airway_after_procedure_waveform, na.rm = TRUE),
            sum(computing_population_dev$vitals_after_procedure_waveform, na.rm = TRUE),
            waveform_etc02,
            sum(initial_population$NUMERATOR, na.rm = TRUE),
            nrow(adult_pop),
            nrow(peds_pop),
            n_procedures
          )
        )

      } else if (

          all(
            rlang::quo_is_null(airway_datetime),
            rlang::quo_is_null(airway_confirmation_col)
          )

        ) {


        # summarize counts for populations filtered
        filter_counts <- tibble::tibble(
          filter = c("Invasive airway procedures",
                     "Successful invasive airway procedures",
                     "911 calls",
                     "Successful invasive airway procedures performed after EMS arrival",
                     "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                     "Waveform ETCO2 >= 5",
                     "Successful invasive airway procedures with waveform ETCO2",
                     "Adults denominator",
                     "Peds denominator",
                     "Total procedures in dataset"
          ),
          count = c(
            sum(procedures_ordered$target_procedures, na.rm = TRUE),
            sum(procedures_ordered$target_procedures & procedures_ordered$successful_procedure, na.rm = TRUE),
            length(call_911_data),
            sum(procedures_ordered$target_procedures &
                  procedures_ordered$successful_procedure &
                  procedures_ordered$not_performed_prior, na.rm = TRUE),
            sum(computing_population_dev$vitals_after_procedure_waveform, na.rm = TRUE),
            waveform_etc02,
            sum(initial_population$NUMERATOR, na.rm = TRUE),
            nrow(adult_pop),
            nrow(peds_pop),
            n_procedures
          )
        )

      }

      # get the populations of interest

      cli::cli_progress_update(set = 13, id = progress_bar_population, force = TRUE)

      # gather data into a list for multi-use output
      airway.18.population <- list(
        filter_process = filter_counts,
        adults = adult_pop,
        peds = peds_pop,
        initial_population = initial_population,
        computing_population = computing_population
      )

      cli::cli_progress_done(id = progress_bar_population)

      return(airway.18.population)

    }
