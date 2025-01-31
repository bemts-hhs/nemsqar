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
#' @param patient_dob_col Column that contains the patient's date of birth. This
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
#' @param eprocedures_02_col Column that indicates procedure prior to arrival.
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
                      patient_dob_col = NULL,
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
      missing(patient_dob_col),
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
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)|911 Response"

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
    total = 13,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

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
      dplyr::distinct()

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


  } else if( # else continue with the tables passed to the applicable arguments

    all(
    !is.null(patient_scene_table),
    !is.null(response_table),
    !is.null(arrest_table),
    !is.null(procedures_table),
    !is.null(vitals_table)
  ) && is.null(df)

  ) {

    # get distinct tables when passed to table arguments
    # patient
    patient_scene_table <- patient_scene_table |>
      dplyr::distinct()

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

  }

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
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

    incident_date <- rlang::enquo(incident_date_col)
    patient_dob <- rlang::enquo(patient_dob_col)

    if (
      (!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
       !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
      (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_dob)]]) &
       !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_dob)]]))
    ) {
      cli::cli_abort(
        "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
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

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # Get first intubation procedure & time intervals for vitals
  procedures_table |>
    dplyr::mutate(
      non_missing_procedure_time = !is.na({{ eprocedures_01_col }}), # Procedure date/time not null
      not_performed_prior = !grepl(pattern = "9923003|Yes", x = {{ eprocedures_02_col }}), # Procedure PTA is not Yes
      target_procedures = grepl(pattern = procedures_code, x = {{ eprocedures_03_col }}) # Procedure name/code in list
    ) |>
    dplyr::group_by({{ erecord_01_col }}) |>
    dplyr::arrange({{ eprocedures_01_col }}) |>
    dplyr::mutate(procedure_order = rank({{ eprocedures_01_col }})) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      vitals_range_start = {{ eprocedures_01_col }} - lubridate::dminutes(3),
      vitals_range_end = {{ eprocedures_01_col }} + lubridate::dminutes(5),
      range_bounds_before = lubridate::interval(vitals_range_start, {{ eprocedures_01_col }}),
      range_bounds_after = lubridate::interval({{ eprocedures_01_col }}, vitals_range_end),
      successful_procedure = dplyr::if_else(all({{ eprocedures_05_col }} == 1,
                                                grepl(pattern = "9923003|Yes", x = {{ eprocedures_06_col }})), 1, 0)
                  ) |>
    dplyr::distinct() -> procedures_ordered

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # create an effective filter for the initial population
  procedures_filter <- procedures_ordered |>
    dplyr::filter(target_procedures,
                  non_missing_procedure_time,
                  not_performed_prior
                  ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # conditionally perform age in years calculations
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  # Add calculated age in years
  initial_population_dev <- patient_scene_table |>
    dplyr::mutate(patient_age_in_years = as.numeric(difftime({{ incident_date_col }},
                                                             {{ patient_dob_col }},
                                                             units = "days")/365)) |>
    dplyr::mutate(patient_age_in_years = dplyr::case_when(!is.na(patient_age_in_years) ~ patient_age_in_years,
                                                          grepl(pattern = year_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }}),

                                                          grepl(pattern = month_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / 12),

                                                          grepl(pattern = day_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / 365),

                                                          grepl(pattern = hour_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / (365*24)),

                                                          grepl(pattern = minute_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / (365*24*60))
                                                          ))

  } else if ( # condition where the user does not pass the incident date nor the patient DOB
    all(
      rlang::quo_is_null(rlang::enquo(incident_date_col)),
      rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

    # Add calculated age in years
    initial_population_dev <- patient_scene_table |>
      dplyr::mutate(patient_age_in_years = dplyr::case_when(grepl(pattern = year_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }}),

                                                            grepl(pattern = month_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / 12),

                                                            grepl(pattern = day_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / 365),

                                                            grepl(pattern = hour_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / (365*24)),

                                                            grepl(pattern = minute_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case = TRUE) ~ as.numeric({{ epatient_15_col }} / (365*24*60))
                                                            ))

  }

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # Get 911 responses
  response_table |>
    dplyr::mutate(call_911 = dplyr::if_else(grepl(pattern = codes_911,
                                                                  x = {{ eresponse_05_col }},
                                                                  ignore.case = TRUE), 1, 0)
                                  ) |>
    dplyr::select({{ erecord_01_col }}, call_911) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::pull({{ erecord_01_col }}) -> call_911_data

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # get arrest table with needed exclusion classifications
  arrest_table_filter <- arrest_table |>
    dplyr::mutate(exclude_pta_ca = dplyr::if_else(grepl(pattern = "3001003|Yes, Prior",
                                                        x = {{ earrest_01_col }}), 1, 0)
                  ) |>
    dplyr::select({{ erecord_01_col }}, exclude_pta_ca) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # Initial manipulations
  initial_population_dev |>
    dplyr::mutate(call_911 = dplyr::if_else({{ erecord_01_col }} %in% call_911_data, 1, 0),
                  exclude_newborns = dplyr::if_else(patient_age_in_years <= 1/365, 1, 0),
                  adult_population = dplyr::if_else(patient_age_in_years >= 18, 1, 0),
                  pedi_population = dplyr::if_else(patient_age_in_years < 18, 1, 0),
                  target_procedures = dplyr::if_else({{ erecord_01_col }} %in% procedures_filter, 1, 0),
                  exclude_pta_ca = dplyr::if_else({{ erecord_01_col }} %in% arrest_table_filter, 1, 0),
                  patient_age_in_years_1_9 = patient_age_in_years >= 1 & patient_age_in_years <= 9,
                  patient_age_in_years_10_plus = patient_age_in_years >= 10,
                  patient_age_in_years_multiplied = dplyr::if_else(patient_age_in_years < 10, patient_age_in_years * 2, 1),
                  patient_age_in_days = patient_age_in_years * 365,
                  patient_age_28_days_1_year = patient_age_in_days < 365 & patient_age_in_days > 28,
                  patient_age_days_28 = patient_age_in_days < 28
                  ) |>
    dplyr::mutate(include_population = dplyr::if_else(

      all(
        call_911 == 1,
        exclude_pta_ca == 0,
        exclude_newborns == 0,
        target_procedures == 1,
        ), 1, 0

      )) -> initial_population_dev

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # NEED TO WORK ON THIS IN ORDER TO CONSIDER ALL THE CONDITIONS FOR NUMERATOR 1
  # TOGETHER TO CALCULATE NUMERATOR 1 FOR ADULTS AND PEDS
  # AND THEN CALCULATE NUMERATOR 2 FOR ADULTS AND PEDS
  # THIS WILL LIKELY BE ONE ADDITIONAL LINE OF CODE THAT WILL
  # SUMMARIZE EVERYTHING ACROSS NUMERATOR 1, AND THEN THE SAME FOR NUMERATOR 2
  # THEN YOU CAN GAUGE PERFORMANCE FOR EACH SPECIFIC CONDITION WITHIN THE OUTPUT
  # CREATE A TEST FILE TO TEST THIS WHOLE SCRIPT ONCE YOU GET THE CALCULATIONS
  # TOGETHER

  # Numerator
  initial_population_dev |>
    dplyr::select({{ erecord_01_col }}, patient_age_in_years) |>
    dplyr::distinct() |>
    dplyr::left_join(vitals_table, by = dplyr::join_by({{ erecord_01_col }}), relationship = "many-to-many") |>
    dplyr::left_join(procedures_ordered, by = join_by({{ erecord_01_col }}), relationship = "many-to-many") |>
    dplyr::mutate(within_range_before = do.call(lubridate::`%within%`, {{ evitals_01_col }}, range_bounds_before),
                  within_range_after = do.call(lubridate::`%within%`, {{ evitals_01_col }}, range_bounds_after)
                  ) |>
    dplyr::mutate(

      # numerator 1 for all patients with pulse oximetry >= 90
      numerator1_all_spo2 = dplyr::if_else(

      procedure_order == 1 &

        successful_procedure &

        ( within_range_before & {{ evitals_12_col }} >= 90 ), 1, 0

      ),

      numerator2_all_spo2 = dplyr::if_else(

        procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_12_col }} >= 90 ), 1, 0

      ),

      # final numerator 1
      numerator_all_spo2 = dplyr::if_else((numerator1_all_spo2 + numerator2_all_spo2) > 1, 1, 0),

      # start calculation of numerator 2, ages >= 10
      numerator1_10 = dplyr::if_else(

        patient_age_in_years_10_plus &

        procedure_order == 1 &

          successful_procedure &

          ( within_range_before & {{ evitals_06_col }} >= 90 ), 1, 0

      ),

      numerator2_10 = dplyr::if_else(

        patient_age_in_years_10_plus &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_06_col }} >= 90 ), 1, 0

      ),

      # final numerator 2
      numerator_10 = dplyr::if_else((numerator1_10 + numerator2_10) > 1, 1, 0),

      # start calculation of numerator 3, ages 1-9 yrs
      numerator1_1_9 = dplyr::if_else(

        patient_age_in_years_1_9 &

        procedure_order == 1 &

          successful_procedure &

          ( within_range_before & {{ evitals_06_col }} >= (70 + patient_age_in_years_multiplied) ), 1, 0

      ),

      numerator2_1_9 = dplyr::if_else(

        patient_age_in_years_1_9 &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_06_col }} >= (70 + patient_age_in_years_multiplied) ), 1, 0

      ),

      # final numerator 3
      numerator_1_9 = dplyr::if_else((numerator1_1_9 + numerator2_1_9) > 1, 1, 0),

      # start calculation of numerator 4, ages < 1 yr & > 28 days or < 28 days
      numerator1_1_28 = dplyr::if_else(

        patient_age_28_days_1_year &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_before & {{ evitals_06_col }} >= 70), 1, 0

      ),

      numerator2_1_28 = dplyr::if_else(

        patient_age_28_days_1_year &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_06_col }} >= 70 ), 1, 0

      ),

      numerator3_1_28 = dplyr::if_else(

        patient_age_days_28 &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_before & {{ evitals_06_col }} >= 60 ), 1, 0

      ),

      numerator4_1_28 = dplyr::if_else(

        patient_age_days_28 &

          procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_06_col }} >= 60 ), 1, 0

      ),

      # final numerator 4
      numerator_1_28 = dplyr::if_else((numerator1_1_28 + numerator2_1_28) > 1 |
                                        (numerator3_1_28 + numerator4_1_28) > 1, 1, 0),

      # start numerator 5 calculations, sbp >= 90 among all patients
      numerator1_all_sbp = dplyr::if_else(

        procedure_order == 1 &

          successful_procedure &

          ( within_range_before & {{ evitals_06_col }} >= 90 ), 1, 0

      ),

      numerator2_all_sbp = dplyr::if_else(

        procedure_order == 1 &

          successful_procedure &

          ( within_range_after & {{ evitals_06_col }} >= 90 ), 1, 0

      ),

      # final numerator 5
      numerator_all_sbp = dplyr::if_else((numerator1_all_sbp + numerator2_all_sbp) > 1, 1, 0),


      ) -> computing_population_dev

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # Get successful performance across all numerator populations
  all_numerators <- computing_population_dev |>
    dplyr::select({{ erecord_01_col }}, matches("numerator_")) |>
    dplyr::summarize(across(matches("numerator_"), ~ max(.)),
                     .by = {{ erecord_01_col }}
                     ) |>
    dplyr::distinct({{ erecord_01_col }})

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # Numerator calculations continued
  computing_population_dev |>
    dplyr::mutate(sbp_threshold = dplyr::case_when(patient_age_in_years_10_plus ~ 90,
                                                   patient_age_in_years_1_9 ~ 70 + patient_age_in_years_multiplied,
                                                   patient_age_28_days_1_year ~ 70,
                                                   TRUE ~ 60
                                                   )) |>
    dplyr::mutate(no_hypoxia = dplyr::case_when(is.na({{ evitals_12_col }}) ~ 0,
                                  {{ evitals_12_col }} >= 90 ~ 1,
                                  TRUE ~ 0),
                  no_hypotension = dplyr::case_when(is.na({{ evitals_06_col }}) ~ 0,
                                      {{ evitals_06_col }} >= sbp_threshold ~ 1,
                                      TRUE ~ 0)
           ) |>
    dplyr::select({{ erecord_01_col }},
                  patient_age_in_years,
                  call_911,
                  exclude_newborns,
                  adult_population,
                  pedi_population,
                  target_procedures,
                  exclude_pta_ca,
                  patient_age_in_years_1_9,
                  patient_age_in_years_10_plus,
                  patient_age_in_years_multiplied,
                  patient_age_in_days,
                  patient_age_28_days_1_year,
                  patient_age_days_28,
                  sbp_threshold,
                  no_hypoxia,
                  no_hypotension
                  ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::left_join(all_numerators, by = dplyr::join_by({{ erecord_01_col }})) -> computing_population

  # YOU HAVE ATTEMPTED TO CALCULATE THE NUMERATOR FOR THE POPULATION
  # YOU NEED TO NOW TEST YOUR METHODS ABOVE
  # WHAT YOU NEED TO DO IS SUMMARIZE OVERALL ADULT PERFORMANCE
  # OVERALL PEDS PERFORMANCE, AND THEN GET PERFORMANCE BY EACH OF THE DIFFERENT
  # NUMERATORS
  # YOU HAVE THE NUMERATOR CODED WRONG ABOVE, NUMERATOR 1 IS ALL THE CONDITIONS
  # AND NUMERATOR 2 IS ALL THE CONDITIONS IN THERE, AND THEN YOU SPLIT THAT
  # BY ADULTS AND PEDS
  # CHANGE NUMERATOR CODE ABOVE TO BE `AND` CLAUSES AMONG ALL THE CONDITIONS

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # Numerator calculations continued
    computing_population |>
    dplyr::mutate(successful_no_hypoxia = as.numeric(no_hypoxia & successful_procedure),
           successful_no_hypotension = as.numeric(no_hypotension & successful_procedure),
           measure_performance = as.numeric(no_hypoxia & no_hypotension & successful_procedure)) |>

    dplyr::summarize(measure_performance = max(measure_performance, na.rm = TRUE),
                     no_hypoxia = max(no_hypoxia, na.rm = TRUE),
                     no_hypotension = max(no_hypotension, na.rm = TRUE),
                     successful_no_hypoxia = max(successful_no_hypoxia, na.rm = TRUE),
                     successful_no_hypotension = max(successful_no_hypotension, na.rm = TRUE),
                     .by = {{ erecord_01_col }}
                     ) -> calculated_numerator

    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # Work with calculated data
  calculated_data |>
    dplyr::mutate(adult_measure_performance = as.numeric(measure_performance & adult_population),
                  adult_successful_no_hypoxia = as.numeric(successful_no_hypoxia & adult_population),
                  adult_successful_no_hypotension = as.numeric(successful_no_hypotension & adult_population),
                  pedi_measure_performance = as.numeric(measure_performance & pedi_population),
                  pedi_successful_no_hypoxia = as.numeric(successful_no_hypoxia & pedi_population),
                  pedi_successful_no_hypotension = as.numeric(successful_no_hypotension & pedi_population),
                  adult_successful_intubation = as.numeric(successful_procedure & adult_population),
                  pedi_successful_intubation = as.numeric(successful_procedure & pedi_population)
                  ) |>
    dplyr::summarize(`Initial Population` = sum(procedure_order, na.rm = TRUE),
                     `911 calls` = sum(call_911, na.rm = TRUE),
                     `Excluded cardiac arrests prior to arrival` = sum(exclude_pta_ca, na.rm = TRUE),
                     `Excluded newborns` = sum(exclude_newborns, na.rm = TRUE),
                     `Denominator population` = sum(include_population, na.rm = TRUE),
                     `Adults denominator` = sum(adult_population, na.rm = TRUE),
                     `Peds denominator` = sum(pedi_population, na.rm = TRUE),
                     `Successful intubations` = sum(successful_procedure, na.rm = TRUE),
                     `Adult successful intubations` = sum(adult_successful_intubation, na.rm = TRUE),
                     `Pedi successful intubations` = sum(pedi_successful_intubation, na.rm = TRUE),
                     `Successful without hypoxia` = sum(successful_no_hypoxia, na.rm = TRUE),
                     `Successful without hypotension` = sum(successful_no_hypotension, na.rm = TRUE),
                     `Measure met` = sum(measure_performance, na.rm = TRUE),
                     `Adult successful without hypoxia` = sum(adult_successful_no_hypoxia, na.rm = TRUE),
                     `Adult successful without hypotension` = sum(adult_successful_no_hypotension, na.rm = TRUE),
                     `Adult measure met` = sum(adult_measure_performance, na.rm = TRUE),
                     `Pedi successful without hypoxia` = sum(pedi_successful_no_hypoxia, na.rm = TRUE),
                     `Pedi successful without hypotension` = sum(pedi_successful_no_hypotension, na.rm = TRUE),
                     `Pedi measure met` = sum(pedi_measure_performance, na.rm = TRUE)
                     ) |>
    tidyr::pivot_longer(tidyselect::everything(), names_to = "filter", values_to = "count") -> filter_counts

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # Get populations
  airway.01.population <- list(
    filter_process = filter_counts,

    adults = calculated_data |> filter(adult_population == 1) |>
      dplyr::select(-contains("pedi"), -contains("adult")),

    peds = calculated_data |> filter(pedi_population == 1) |>
      dplyr::select(-contains("pedi"), -contains("adult")),

    initial_population = calculated_data
  )


  cli::cli_progress_done(id = progress_bar_population)

  return(airway.01.population)

}
