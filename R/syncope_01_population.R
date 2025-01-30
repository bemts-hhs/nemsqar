#' @title Syncope-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Syncope-01, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories to identify potential syncope (fainting) cases
#' based on specific criteria and calculates related ECG measures. This function
#' segments the data by age into adult and pediatric populations.
#'
#' @param df Main data frame containing EMS records.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient age (numeric).
#' @param epatient_16_col Column for the patient age units (e.g., "Years",
#'   "Months").
#' @param eresponse_05_col Column containing response type codes, specifically
#'   911 codes.
#' @param esituation_09_col Column with primary symptoms associated with the
#'   patient encounter.
#' @param esituation_10_col Column with other associated symptoms.
#' @param esituation_11_col Column for primary impression code.
#' @param esituation_12_col Column for secondary impression codes.
#' @param evitals_04_col Column with ECG information if available.
#'
#' @return #' A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
syncope_01_population <- function(df = NULL,
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
                       esituation_09_col,
                       esituation_10_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_04_col) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn syncope_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn syncope_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure all *_col arguments are fulfilled

  if (
    any(
      missing(erecord_01_col),
      missing(eresponse_05_col),
      missing(esituation_09_col),
      missing(esituation_10_col),
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(evitals_04_col)
    )
  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn syncope_01_population}.")
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
  progress_bar_population <- cli::cli_progress_bar(
    "Running `syncope_01_population()`",
    total = 10,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # dplyr::filter incident data for 911 response codes and the corresponding primary/secondary impressions
  # dplyr::filter down the primary / other associated symptoms

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # primary and secondary provider impression values
  syncope_pattern <- "(?:R(?:55|40.4))|Syncope and collapse|Transient alteration of awareness"

  # ECG pattern
  ecg_pattern <- "12 Lead-Left Sided \\(Normal\\)|12 Lead-Right Sided|15 Lead|18 Lead|3304007|3304009|3304011|3304013"

  # minor values
  minor_values <- "days|hours|minutes|months"

  year_values <- "2516009|years"

  day_values <- "days|2516001"

  hour_values <- "hours|2516003"

  minute_values <- "minutes|2516005"

  month_values <- "months|2516007"

  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
        is.data.frame(situation_table) || tibble::is_tibble(situation_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn syncope_02_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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
      patient_DOB <- rlang::enquo(patient_DOB_col)

      if (
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_DOB)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_DOB)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }

    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {

  # dplyr::filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}),
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18
    )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

      # dplyr::filter the table to get the initial population regardless of age
      final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        # create the age in years variable
        dplyr::mutate(

        # system age check
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}),
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

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

    # 911 calls
    call_911_data <- response_table |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # create the syncope variable using primary / associated symptoms
    syncope_data_1 <- situation_table |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_09_col }}, {{ esituation_10_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_09_col}}, {{esituation_10_col}}), ~ grepl(
          pattern = syncope_pattern,
          x = .,
          ignore.case = T
        ))

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # create the syncope variable using primary / secondary impressions
    syncope_data_2 <- situation_table |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}, {{ esituation_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
          pattern = syncope_pattern,
          x = .,
          ignore.case = T
        ))

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

    # ECG variable
    ecg_data <- vitals_table |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_04_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = ecg_pattern, x = {{evitals_04_col}}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |>
      dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    SYNCOPE1 = {{ erecord_01_col }} %in% syncope_data_1,
                    SYNCOPE2 = {{ erecord_01_col }} %in% syncope_data_2,
                    SYNCOPE = SYNCOPE1 | SYNCOPE2,
                    ECG_PERFORMED = {{ erecord_01_col }} %in% ecg_data
                    )

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(


        # syncope patients
        SYNCOPE,

        # 911 calls
        CALL_911

      )

    # Adult and Pediatric Populations

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Syncope cases",
                 "ECG performed",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$SYNCOPE, na.rm = T),
        sum(computing_population$ECG_PERFORMED, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    syncope.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(syncope.01.population)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    !is.null(df)

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
      patient_DOB <- rlang::enquo(patient_DOB_col)

      if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))) {

        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
        )

      }
    }

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

    ###_____________________________________________________________________________
    # from the full dataframe with all variables
    # create one fact table and several dimension tables
    # to complete calculations and avoid issues due to row
    # explosion
    ###_____________________________________________________________________________

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {

      # dplyr::filter the table to get the initial population regardless of age
      final_data <- df |>
        dplyr::select(-c(
          {{ eresponse_05_col }},
          {{ esituation_09_col }},
          {{ esituation_10_col }},
          {{ esituation_11_col }},
          {{ esituation_12_col }},
          {{ evitals_04_col }}
        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        # create the age in years variable
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) / 365,

        # system age check
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}),
        system_age_minor = system_age_minor1 | system_age_minor2,

        # calculated age check
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18
        )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

      # dplyr::filter the table to get the initial population regardless of age
      final_data <- df |>
        dplyr::select(-c(
          {{ eresponse_05_col }},
          {{ esituation_09_col }},
          {{ esituation_10_col }},
          {{ esituation_11_col }},
          {{ esituation_12_col }},
          {{ evitals_04_col }}
        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        # create the age in years variable
        dplyr::mutate(

          # system age check
          system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}),
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

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

    # 911 calls
    call_911_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # create the syncope variable using primary / associated symptoms
    syncope_data_1 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_09_col }}, {{ esituation_10_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_09_col}}, {{esituation_10_col}}), ~ grepl(
          pattern = syncope_pattern,
          x = .,
          ignore.case = T
        ))

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # create the syncope variable using primary / secondary impressions
    syncope_data_2 <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}, {{ esituation_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
          pattern = syncope_pattern,
          x = .,
          ignore.case = T
        ))

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

    # ECG variable
    ecg_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_04_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = ecg_pattern, x = {{evitals_04_col}}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |>
      dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    SYNCOPE1 = {{ erecord_01_col }} %in% syncope_data_1,
                    SYNCOPE2 = {{ erecord_01_col }} %in% syncope_data_2,
                    SYNCOPE = SYNCOPE1 | SYNCOPE2,
                    ECG_PERFORMED = {{ erecord_01_col }} %in% ecg_data
      )

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(


        # syncope patients
        SYNCOPE,

        # 911 calls
        CALL_911

      )

    # Adult and Pediatric Populations

    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult | calc_age_adult)

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor | calc_age_minor)

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult)

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor)

    }

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Syncope cases",
                 "ECG performed",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$SYNCOPE, na.rm = T),
        sum(computing_population$ECG_PERFORMED, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    syncope.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(syncope.01.population)

  }

}
