#' @title Airway-05 Population
#'
#' @description
#'
#' This function processes and analyzes the dataset to generate the populations
#' of interest needed to perform calculations to obtain performance data.
#'
#' @param df A dataframe or tibble contianing EMS data where each row represents
#'   an observation and columns represent features. Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient, escene, and earrest.01 fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param arrest_table A data.frame or tibble containing at least the earrest
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A dataframe or tibble containing at least the
#'   eProcedures fields needed. Default is `NULL`.
#' @param vitals_table A dataframe or tibble containing at least the eVitals
#'   fields needed. Default is `NULL`.
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
#' @param evitals_12_col Numeric column containing pulse oximetry values.
#' @param eprocedures_01_col Date-time or POSIXct column for procedures
#' @param eprocedures_02_col Column that indicates procedure prior to arrival.
#' @param eprocedures_03_col Column containing procedure codes with or without
#'   procedure names.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were
#'  administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @author Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS
#'
#' @export
#'
airway_05_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      arrest_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col,
                      patient_dob_col,
                      epatient_15_col,
                      epatient_16_col,
                      earrest_01_col,
                      eresponse_05_col,
                      evitals_01_col,
                      evitals_12_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col
                      ) {

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

  # Check for tables or DF

  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(arrest_table)
    ) && !is.null(df)
  ) {

    cli::cli_abort("{.fn airway_05_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")

  }

  # MISSING DATA TABLES, use df

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
      missing(evitals_12_col),
      missing(eprocedures_01_col),
      missing(eprocedures_02_col),
      missing(eprocedures_03_col)
    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn airway_01_population}.")

  }

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
    "Running `airway_05_population()`",
    total = 10,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

  ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING #######

  if(
    all(
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
    # patient scene table
    patient_scene_table <- df |>
      dplyr::select(-{{ evitals_01_col }},
                    -{{ evitals_12_col }},
                    -{{ eprocedures_01_col }},
                    -{{ eprocedures_02_col }},
                    -{{ eprocedures_03_col }},
                    -{{ eresponse_05_col }}
                    ) |>
      dplyr::distinct()

    # arrest
    arrest_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ earrest_01_col }}
                    ) |>
      dplyr::distinct()

    # response table
    response_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ eresponse_05_col }}
                    ) |>
      dplyr::distinct()

    # vitals table
    vitals_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ evitals_01_col }},
                    {{ evitals_12_col }}
                    ) |>
      dplyr::distinct()

    # procedures table
    procedures_table <- df |>
      dplyr::select({{ erecord_01_col }},
                    {{ eprocedures_01_col }},
                    {{ eprocedures_02_col }},
                    {{ eprocedures_03_col }}
                    )

  } # else continue with the tables passed to the applicable arguments

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

  # Get intubation procedure & time intervals for vitals
  procedures_table |>
    dplyr::group_by({{ erecord_01_col }}) |>
    dplyr::filter(!is.na({{ eprocedures_01_col }}), # Procedure date/time not null
           !grepl(pattern = "9923003|Yes", x = {{ eprocedures_02_col }}), # Procedure PTA is not Yes
           grepl(pattern = procedures_code, x = {{ eprocedures_03_col }}) # Procedure name/code in list
    ) |>
    dplyr::mutate(vitals_range_start = {{ eprocedures_01_col }} - lubridate::dminutes(3),
                  vitals_range_end = {{ eprocedures_01_col }}
                  ) |>
    dplyr::mutate(vitals_time_interval = lubridate::interval(vitals_range_start,
                                                             vitals_range_end))

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- patient_scene_table |>
    dplyr::left_join(procedures_table, by = dplyr::join_by({{ erecord_01_col }}))

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # conditionally perform age in years calculations
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  # Add calculated age in years
  initial_population <- initial_population |>
    dplyr::mutate(patient_age_in_years = as.numeric(difftime({{ incident_date_col }},
                                                             {{ patient_dob_col }},
                                                             units = "days")/365)) |>
    dplyr::mutate(patient_age_in_years = dplyr::case_when(!is.na(patient_age_in_years) ~ patient_age_in_years,
                                                          grepl(pattern = year_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case=TRUE) ~ as.numeric({{ epatient_15_col }}),

                                                          grepl(pattern = month_values,
                                                                x = {{ epatient_16_col }},
                                                                ignore.case=TRUE) ~ as.numeric({{ epatient_15_col }} / 12),

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
    initial_population <- initial_population |>
      dplyr::mutate(patient_age_in_years = dplyr::case_when(grepl(pattern = year_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case=TRUE) ~ as.numeric({{ epatient_15_col }}),

                                                            grepl(pattern = month_values,
                                                                  x = {{ epatient_16_col }},
                                                                  ignore.case=TRUE) ~ as.numeric({{ epatient_15_col }} / 12),

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
    dplyr::mutate(call_911 = dplyr::if_else(grepl(
      pattern = codes_911,
      x = {{ eresponse_05_col }},
      ignore.case =
        TRUE
    ), 1, 0)) |>
    dplyr::select({{ erecord_01_col }}, call_911) |>
    dplyr::distinct() -> filtered_responses

  # update the initial population
  initial_population <- initial_population |>
    dplyr::left_join(filtered_responses, by = dplyr::join_by({{ erecord_01_col }}))

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # get arrest table with  needed exclusion classifications
  arrest_table_filter <- arrest_table |>
    dplyr::mutate(exclude_pta_ca = dplyr::if_else(

      grepl(pattern = "3001003|Yes, Prior", x = {{ earrest_01_col }}), 1, 0

                                                  )
                  )

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # Initial counts
  initial_population |>
    dplyr::left_join(arrest_table_filter, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::mutate(exclude_newborns = dplyr::if_else(patient_age_in_years <= 1/365,
                                                    1,
                                                    0),
                  adult_population = dplyr::if_else(patient_age_in_years >= 18, 1, 0),
                  pedi_population = dplyr::if_else(patient_age_in_years < 18, 1, 0)

                  ) |>
    dplyr::mutate(include_population = dplyr::if_else(all(call_911 == 1,
                                                          exclude_pta_ca == 0,
                                                          exclude_newborns == 0),
                                                      1,
                                                      0)) -> initial_population

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # Numerator
  vitals_table |>
    dplyr::left_join(initial_population, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::group_by({{ erecord_01_col }}) |>
    dplyr::filter({{ evitals_01_col }} <= vitals_range_end,
                  {{ evitals_01_col }} >= vitals_range_start) -> vf

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # Numerator calculations continued
  vf |>
    dplyr::mutate(no_hypoxia = dplyr::case_when(is.na({{ evitals_12_col }}) ~ 0,
                                  {{ evitals_12_col }} >= 94 ~ 1,
                                  TRUE ~ 0)
                  ) -> vf

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # Numerator calculations continued
    vf |>
    dplyr::mutate(successful_no_hypoxia = as.numeric(no_hypoxia & successful_procedure),
           measure_performance = as.numeric(no_hypoxia & successful_procedure)) |>
    dplyr::summarise(measure_performance = max(measure_performance, na.rm = TRUE),
                     no_hypoxia = max(no_hypoxia, na.rm = TRUE),
                     successful_no_hypoxia = max(successful_no_hypoxia, na.rm = TRUE)) -> calculated_numerator

    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # complete the initial population setup
  initial_population |> dplyr::left_join(calculated_numerator, by = dplyr::join_by({{ erecord_01_col }})) -> calculated_data

  # Remove the calculated_numerator intermediate object
  rm(calculated_numerator)

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # Work with calculated data
  calculated_data |>
    dplyr::ungroup() |>
    dplyr::mutate(adult_measure_performance = as.numeric(measure_performance & adult_population),
                  adult_successful_no_hypoxia = as.numeric(successful_no_hypoxia & adult_population),
                  pedi_measure_performance = as.numeric(measure_performance & pedi_population),
                  pedi_successful_no_hypoxia = as.numeric(successful_no_hypoxia & pedi_population),
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
                     `Measure met` = sum(measure_performance, na.rm = TRUE),
                     `Adult successful without hypoxia` = sum(adult_successful_no_hypoxia, na.rm = TRUE),
                     `Adult measure met` = sum(adult_measure_performance, na.rm = TRUE),
                     `Pedi successful without hypoxia` = sum(pedi_successful_no_hypoxia, na.rm = TRUE),
                     `Pedi measure met` = sum(pedi_measure_performance, na.rm = TRUE)
                     ) |>
    tidyr::pivot_longer(everything(), names_to = "filter", values_to = "count") -> filter_counts

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

  return(airway.05.population)

}
