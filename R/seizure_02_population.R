#' @title Seizure-02 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Seizure-02, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to asthma-related incidents in an EMS
#' dataset, specifically focusing on cases where 911 was called for respiratory
#' distress, and certain medications were administered. This function segments
#' the data by age into adult and pediatric populations.
#'
#' @param df A data frame where each row is an observation, containing all
#'   necessary columns for analysis.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the
#'   emedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name for patient age in numeric form.
#' @param epatient_16_col Column name for age unit (e.g., `"Years"` or
#'   `"Months"`).
#' @param eresponse_05_col Column name for response codes; "911" call codes are
#'   filtered.
#' @param esituation_11_col Column name for primary impressions.
#' @param esituation_12_col Column name for secondary impressions.
#' @param emedications_03_col Column name for medications administered; ideally
#'   a list column or string with comma-separated values.
#'
#' @return
#' #' A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#'
#' @examples
#'
#' # create tables to test correct functioning
#'
#'   # patient table
#'   patient_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     incident_date = as.Date(c("2025-01-01", "2025-01-05",
#'                               "2025-02-01", "2025-01-01",
#'                               "2025-06-01")
#'                               ),
#'     patient_dob = as.Date(c("2000-01-01", "2020-01-01",
#'                             "2023-02-01", "2023-01-01",
#'                             "1970-06-01")
#'                             ),
#'     epatient_15 = c(25, 5, 2, 2, 55),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Years", "Years")
#'
#'   )
#'
#'   # response table
#'   response_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     eresponse_05 = rep(2205001, 5)
#'
#'   )
#'
#'   # situation table
#'   situation_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     esituation_11 = rep("G40", 5),
#'     esituation_12 = rep("r56", 5),
#'   )
#'
#'   # medications table
#'   medications_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     emedications_03 = rep(3322, 5)
#'
#'   )
#'
#'   # test the success of the function
#'   result <- seizure_02_population(patient_scene_table = patient_table,
#'                               response_table = response_table,
#'                               situation_table = situation_table,
#'                               medications_table = medications_table,
#'                               erecord_01_col = erecord_01,
#'                               incident_date_col = incident_date,
#'                               patient_DOB_col = patient_dob,
#'                               epatient_15_col = epatient_15,
#'                               epatient_16_col = epatient_16,
#'                               eresponse_05_col = eresponse_05,
#'                               esituation_11_col = esituation_11,
#'                               esituation_12_col = esituation_12,
#'                               emedications_03_col = emedications_03
#'                          )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seizure_02_population <- function(df = NULL,
                       patient_scene_table = NULL,
                       response_table = NULL,
                       situation_table = NULL,
                       medications_table = NULL,
                       erecord_01_col,
                       incident_date_col = NULL,
                       patient_DOB_col = NULL,
                       epatient_15_col,
                       epatient_16_col,
                       eresponse_05_col,
                       esituation_11_col,
                       esituation_12_col,
                       emedications_03_col) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(medications_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn seizure_02_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(medications_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn seizure_02_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
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
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(emedications_03_col)
    )
  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn seizure_02_population}.")
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
    "Running `seizure_02_population()`",
    total = 10,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # get codes as a regex to filter primary/secondary impression fields
  epilepsy_pattern <- "(?:\\bepilep(sy|tic)\\b)(?!.*without)(?:status\\epilepticus)?|(?:neuro|seizure)(?!.*without).*status\\sepilepticus|other\\sseizure|G40(?!\\.[a-z\\d]\\d[249])"

  # medication values for seizure_02

  medication_pattern = "3322|6960|203128|6470|diazepam|midazolam|midazolam hydrochloride|lorazepam"

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
      !is.null(medications_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    is.null(df)
  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(medications_table) || tibble::is_tibble(medications_table),
        is.data.frame(situation_table) || tibble::is_tibble(situation_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn seizure_02_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

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

  # filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    # create the age in years variable

    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365,


    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor1 = {{epatient_15_col}} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor2 = {{epatient_15_col}} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor = system_age_minor1 | system_age_minor2,

    # calculated age checks
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2

    )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

  # filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::mutate(

    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor1 = {{epatient_15_col}} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor2 = {{epatient_15_col}} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor = system_age_minor1 | system_age_minor2

    )

    }

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

    # 911 calls
    call_911_data <- response_table |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = TRUE)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

    # seizure
    seizure_data <- situation_table |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}, {{ esituation_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
                pattern = epilepsy_pattern,
                x = .,
                ignore.case = TRUE,
                perl = TRUE
              ))
      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

    # benzodiazepine check
    benzodiazepine_data <- medications_table |>
      dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = medication_pattern, x = {{emedications_03_col}}, ignore.case = TRUE)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |>
      dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    SEIZURE = {{ erecord_01_col }} %in% seizure_data,
                    BENZO_MED = {{ erecord_01_col }} %in% benzodiazepine_data
                    )

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(


        # seizure patients
        SEIZURE,

        # 911 calls
        CALL_911

      )

  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

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

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Seizure cases",
                 "Benzodiazepine cases",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = TRUE),
        sum(computing_population$SEIZURE, na.rm = TRUE),
        sum(computing_population$BENZO_MED, na.rm = TRUE),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = TRUE)

    # gather data into a list for multi-use output
    seizure.02.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(seizure.02.population)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(medications_table),
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
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

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

  # filter the table to get the initial population regardless of age
  final_data <- df |>
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ esituation_11_col }},
                     {{ esituation_12_col }},
                     {{ emedications_03_col }}

    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    # create the age in years variable

    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365,


    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor1 = {{epatient_15_col}} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor2 = {{epatient_15_col}} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor = system_age_minor1 | system_age_minor2,

    # calculated age checks
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2

    )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

  # filter the table to get the initial population regardless of age
  final_data <- df |>
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ esituation_11_col }},
                     {{ esituation_12_col }},
                     {{ emedications_03_col }}

    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::mutate(

    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor1 = {{epatient_15_col}} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor2 = {{epatient_15_col}} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_minor = system_age_minor1 | system_age_minor2

    )

    }

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

    # START HERE, USE THE CODE BELOW TO FINISH THE DIMENSION
    # TABLE OBJECTS

    # 911 calls
    call_911_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = TRUE)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

    # seizure
    seizure_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}, {{ esituation_12_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
                pattern = epilepsy_pattern,
                x = .,
                ignore.case = TRUE,
                perl = TRUE
              ))
      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

    # benzodiazepine check
    benzodiazepine_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = medication_pattern, x = {{emedications_03_col}}, ignore.case = TRUE)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |>
      dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    SEIZURE = {{ erecord_01_col }} %in% seizure_data,
                    BENZO_MED = {{ erecord_01_col }} %in% benzodiazepine_data
                    )

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(


        # seizure patients
        SEIZURE,

        # 911 calls
        CALL_911

      )

  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

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

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Seizure cases",
                 "Benzodiazepine cases",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = TRUE),
        sum(computing_population$SEIZURE, na.rm = TRUE),
        sum(computing_population$BENZO_MED, na.rm = TRUE),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = TRUE)

    # gather data into a list for multi-use output
    seizure.02.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(seizure.02.population)

  }

}
