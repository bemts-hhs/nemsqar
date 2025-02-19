#' @title Safety-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Safety-01, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to 911 responses where "lights and sirens"
#' were not used in an EMS dataset. This function segments the data by age into
#' adult and pediatric populations.
#'
#' @param df A data frame or tibble containing EMS data.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name containing the unique patient record
#'   identifier.
#' @param incident_date_col Date or POSIXct column indicating the date of the
#'   incident.
#' @param patient_DOB_col Date or POSIXct column for the patient’s date of birth
#' @param epatient_15_col Column containing age.
#' @param epatient_16_col Column for age units.
#' @param eresponse_05_col Column containing response mode codes (e.g., 911
#'   response codes).
#' @param eresponse_24_col Column detailing additional response descriptors as
#'   text.
#'
#' @return #' A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#'
#' @examples
#' # create tables to test correct functioning
#'
#' # patient table
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
#'     eresponse_05 = rep(2205001, 5),
#'     eresponse_24 = rep("No Lights or Sirens", 5)
#'
#'   )
#'
#' # Run the function
#' result <- safety_01_population(patient_scene_table = patient_table,
#'                               response_table = response_table,
#'                               erecord_01_col = erecord_01,
#'                               incident_date_col = incident_date,
#'                               patient_DOB_col = patient_dob,
#'                               epatient_15_col = epatient_15,
#'                               epatient_16_col = epatient_16,
#'                               eresponse_05_col = eresponse_05,
#'                               eresponse_24_col = eresponse_24
#'                         )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_01_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eresponse_24_col) {

  # ensure that not all table arguments AND the df argument are fulfilled
  # user only passes df or all table arguments

    if(

    any(
      !is.null(patient_scene_table),
      !is.null(response_table)
    )

    &&

    !is.null(df)

  ) {

    cli::cli_abort("{.fn safety_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # ensure that df or all table arguments are fulfilled

  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table)
    )

    && is.null(df)

  ) {

    cli::cli_abort("{.fn safety_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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
      missing(eresponse_24_col)
    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn safety_01_population}.")

  }

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # get codes as a regex to find lights and siren responses
  no_lights_and_sirens <- "No Lights or Sirens|2224019"

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

  # initiate the progress bar process
  progress_bar_population <- cli::cli_progress_bar(
    "Running `safety_01_population()`",
    total = 9,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population


  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table)
    ) && is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
      is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
      is.data.frame(response_table) || tibble::is_tibble(response_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn safety_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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

  # filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
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
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

    )

    }

    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

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

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # no lights and sirens check
    no_ls_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_24_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = no_lights_and_sirens, x = {{ eresponse_24_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  NO_LS_CHECK = {{ erecord_01_col }} %in% no_ls_data
                  )

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      CALL_911

    )


  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult
                  )

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor
                  )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult
                    )

    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
    dplyr::filter(system_age_minor
                  )

  }

  # get the summary of results
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "No lights and sirens",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$NO_LS_CHECK, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    safety.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

  cli::cli_progress_done(id = progress_bar_population)

  return(safety.01.population)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table)
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

  # filter the table to get the initial population regardless of age
  final_data <- df |>
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ eresponse_24_col}}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
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
                     {{ eresponse_24_col}}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

    )

    }

    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________

    # 911 calls
    call_911_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # no lights and sirens check
    no_ls_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_24_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = no_lights_and_sirens, x = {{ eresponse_24_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  NO_LS_CHECK = {{ erecord_01_col }} %in% no_ls_data
                  )

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      CALL_911

    )


  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult
                  )

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor
                  )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult
                    )

    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
    dplyr::filter(system_age_minor
                  )

  }

  # get the summary of results
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "No lights and sirens",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$NO_LS_CHECK, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    safety.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

  cli::cli_progress_done(id = progress_bar_population)

  return(safety.01.population)


  }

}
