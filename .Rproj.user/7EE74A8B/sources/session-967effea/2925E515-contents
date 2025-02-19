#' @title Respiratory-01 Populations
#'
#' @description
#'
#' The `respiratory_01_population` function filters and analyzes data related to
#' emergency 911 respiratory distress incidents, providing the adult, pediatric,
#' and initial populations. This function uses specific data columns for 911
#' response codes, primary and secondary impressions, and vital signs to filter
#' a dataset down to the populations of interest.
#'
#' @param df A data frame containing incident data with each row representing an
#'   observation.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing at least the
#'   esituation fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals
#'   fields needed for this measure's calculations.
#' @param erecord_01_col Unique Patient ID
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column giving the
#'   calculated age value.
#' @param epatient_16_col Column giving the
#'   provided age unit value.
#' @param eresponse_05_col Column name for
#'   911 response codes (e.g., 2205001, 2205003, 2205009).
#' @param esituation_11_col Column name for
#'   primary impression codes related to respiratory distress.
#' @param esituation_12_col Column name for
#'   secondary impression codes related to respiratory distress.
#' @param evitals_12_col Column name for
#'   the first vital sign measurement.
#' @param evitals_14_col Column name for
#'   the second vital sign measurement.
#'
#' @return Returns a data frame summarizing the proportion of cases with
#'   complete vital sign data, divided by population
#'
#' @examples
#' # create tables to test correct functioning
#'
#' # patient table
#' patient_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
#'   patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
#'   epatient_15 = c(25, 5, 2, 2, 55),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Years", "Years")
#'
#' )
#'
#' # response table
#' response_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eresponse_05 = rep(2205001, 5)
#'
#' )
#'
#' # situation table
#' situation_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   esituation_11 = c(rep("J80", 3), rep("I50.9", 2)),
#'   esituation_12 = c(rep("J80", 2), rep("I50.9", 3))
#' )
#'
#' # vitals table
#' vitals_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   evitals_12 = c(60, 59, 58, 57, 56),
#'   evitals_14 = c(16, 15, 14, 13, 12)
#'
#' )
#'
#' # Run the function
#' result <- respiratory_01_population(patient_scene_table = patient_table,
#'                               response_table = response_table,
#'                               situation_table = situation_table,
#'                               vitals_table = vitals_table,
#'                               erecord_01_col = erecord_01,
#'                               incident_date_col = incident_date,
#'                               patient_DOB_col = patient_dob,
#'                               epatient_15_col = epatient_15,
#'                               epatient_16_col = epatient_16,
#'                               eresponse_05_col = eresponse_05,
#'                               esituation_11_col = esituation_11,
#'                               esituation_12_col = esituation_12,
#'                               evitals_12_col = evitals_12,
#'                               evitals_14_col = evitals_14
#'                              )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
respiratory_01_population <- function(df = NULL,
                                      patient_scene_table = NULL,
                                      response_table = NULL,
                                      situation_table = NULL,
                                      vitals_table = NULL,
                                      erecord_01_col,
                                      incident_date_col = NULL,
                                      patient_DOB_col = NULL,
                                      epatient_15_col,
                                      epatient_16_col,
                                      eresponse_05_col,
                                      esituation_11_col,
                                      esituation_12_col,
                                      evitals_12_col,
                                      evitals_14_col
                                      ) {


  if(

    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table)
    )

    &&

    !is.null(df)

  ) {

    cli::cli_abort("{.fn respiratory_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # ensure all *_col arguments are fulfilled
  if(

    any(

      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(evitals_12_col),
      missing(evitals_14_col)
    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn respiratory_01_population}.")

  }

  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table)
    )

    && is.null(df)

  ) {

    cli::cli_abort("{.fn respiratory_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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
    "Running `respiratory_01_population()`",
    total = 13,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"

  # get codes as a regex to filter primary impression fields
  resp_codes <- "(?:I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9)"

  # days, hours, minutes, months

  minor_values <- "days|2516001|hours|2516003|minutes|2516005|months|2516007"

  year_values <- "2516009|years"

  day_values <- "days|2516001"

  hour_values <- "hours|2516003"

  minute_values <- "minutes|2516005"

  month_values <- "months|2516007"

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table)
    )

    && is.null(df)

  ) {

    # Ensure df is a data frame or tibble
    if (

      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||

          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) ||

          !(is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||

          !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table))

      )

    ) {

      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required for each of the *_table arguments."
        )
      )
    }

    # Only check the date columns if they are in fact passed
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      # Use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_DOB_col)

      # Convert quosures to names and check the column classes
      incident_date_name <- rlang::as_name(incident_date)
      patient_dob_name <- rlang::as_name(patient_dob)

      if ((!lubridate::is.Date(patient_scene_table[[incident_date_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[incident_date_name]])) ||
          (!lubridate::is.Date(patient_scene_table[[patient_dob_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[patient_dob_name]]))) {

        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or a similar class."
        )
      }
    }

    progress_bar_population

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {


    final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,

    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,

    # calculated age checks
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18
    )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      ))

    {

      final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(

        # system age checks
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor = system_age_minor1 | system_age_minor2

        )


    }

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # respiratory distress 1
  respiratory_distress_data1 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_11_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # respiratory distress 2
  respiratory_distress_data2 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_12_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # vitals check 1
  vitals_check1 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      !is.na({{ evitals_12_col }})

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # vitals check 2
  vitals_check2 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_14_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      !is.na({{ evitals_14_col }})

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(RESPIRATORY_DISTRESS1 = {{ erecord_01_col }} %in% respiratory_distress_data1,
                  RESPIRATORY_DISTRESS2 = {{ erecord_01_col }} %in% respiratory_distress_data2,
                  RESPIRATORY_DISTRESS = RESPIRATORY_DISTRESS1 | RESPIRATORY_DISTRESS2,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  VITALS_CHECK1 = {{ erecord_01_col }} %in% vitals_check1,
                  VITALS_CHECK2 = {{ erecord_01_col }} %in% vitals_check2,
                  VITALS_CHECK = VITALS_CHECK1 & VITALS_CHECK2
                  )

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
  dplyr::filter(

      RESPIRATORY_DISTRESS,

      CALL_911
    )

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # Adult and Pediatric Populations

  if(

    # use the system generated and calculated ages

    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    ))

  {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)

    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }

  # summarize

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Respiratory Distress",
               "Pulse Oximetry and Respiratory Rate taken",
               "911 calls",
               "Adults denominator",
               "Peds denominator",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$RESPIRATORY_DISTRESS, na.rm = T),
      sum(computing_population$VITALS_CHECK, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # get the population of interest
  respiratory.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(respiratory.01.population)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table)
    )

    && !is.null(df)

  )

  # utilize a dataframe to analyze the data for the measure analytics

  {

    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
          "i" = "The passed object is of class {.val {class(df)}}."
        )
      )
    }

    # only check the date columns if they are in fact passed
    if(
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    )

    {

      # use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_DOB_col)

      if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))) {

        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
        )

      }
    }

    progress_bar_population

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

    ###_____________________________________________________________________________
    # from the full dataframe with all variables
    # create one fact table and several dimension tables
    # to complete calculations and avoid issues due to row
    # explosion
    ###_____________________________________________________________________________

    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)


    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {


      final_data <- df |>
        dplyr::select(-c({{ eresponse_05_col }},
                         {{ esituation_11_col }},
                         {{ esituation_12_col }},
                         {{ evitals_12_col }},
                         {{ evitals_14_col }}
                         )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) / 365,

        # system age checks
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor = system_age_minor1 | system_age_minor2,

        # calculated age checks
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18
        )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      ))

    {

      final_data <- df |>
        dplyr::select(-c({{ eresponse_05_col }},
                         {{ esituation_11_col }},
                         {{ esituation_12_col }},
                         {{ evitals_12_col }},
                         {{ evitals_14_col }}
                         )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(

        # system age checks
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor = system_age_minor1 | system_age_minor2

        )


    }

    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________

    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

    # respiratory distress 1
    respiratory_distress_data1 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_11_col }}, ignore.case = T)) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # respiratory distress 2
    respiratory_distress_data2 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_12_col }}, ignore.case = T)) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # vitals check 1
    vitals_check1 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        !is.na({{ evitals_12_col }})

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

    # vitals check 2
    vitals_check2 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_14_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        !is.na({{ evitals_14_col }})

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

    # 911 calls
    call_911_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

    # assign variables to final data
    computing_population <- final_data |>
      dplyr::mutate(RESPIRATORY_DISTRESS1 = {{ erecord_01_col }} %in% respiratory_distress_data1,
                    RESPIRATORY_DISTRESS2 = {{ erecord_01_col }} %in% respiratory_distress_data2,
                    RESPIRATORY_DISTRESS = RESPIRATORY_DISTRESS1 | RESPIRATORY_DISTRESS2,
                    CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    VITALS_CHECK1 = {{ erecord_01_col }} %in% vitals_check1,
                    VITALS_CHECK2 = {{ erecord_01_col }} %in% vitals_check2,
                    VITALS_CHECK = VITALS_CHECK1 & VITALS_CHECK2
                    )

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        RESPIRATORY_DISTRESS,

        CALL_911
      )

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # Adult and Pediatric Populations

    if(

      # use the system generated and calculated ages

      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))

      )
    ) {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult | calc_age_adult)

      cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor | calc_age_minor)

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      ))

    {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult)

      cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor)

    }

    # summarize

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Respiratory Distress",
                 "Pulse Oximetry and Respiratory Rate taken",
                 "911 calls",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$RESPIRATORY_DISTRESS, na.rm = T),
        sum(computing_population$VITALS_CHECK, na.rm = T),
        sum(computing_population$CALL_911, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

    # get the population of interest
    respiratory.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(respiratory.01.population)

  }

}
