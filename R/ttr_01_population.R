#' @title TTR-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for TTR-01, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories to records that are 911 requests for patients not
#' transported by EMS during which a basic set of vital signs is documented
#' based on specific criteria and calculates related ECG measures. This function
#' segments the data by age into adult and pediatric populations.
#'
#' @param df A data frame or tibble containing the dataset to analyze. Default
#'   is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param arrest_table A data frame or tibble containing only the earrest fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col A column specifying unique patient records.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col A column indicating the patient’s age in numeric form.
#' @param epatient_16_col A column specifying the unit of patient age (e.g.,
#'   "Years", "Days").
#' @param eresponse_05_col A column specifying the type of response (e.g., 911
#'   codes).
#' @param transport_disposition_col A column specifying transport disposition
#'   for the patient.
#' @param earrest_01_col A column containing cardiac arrest data.
#' @param evitals_06_col A column containing systolic blood pressure (SBP) data
#'   from initial vital signs.
#' @param evitals_07_col A column containing diastolic blood pressure (DBP) data
#'   from initial vital signs.
#' @param evitals_10_col A column containing heart rate data from initial vital
#'   signs.
#' @param evitals_12_col A column containing spO2 data from the initial vital
#'   signs.
#' @param evitals_14_col A column containing respiratory rate data from initial
#'   vital signs.
#' @param evitals_23_col A column containing total Glasgow Coma Scale (GCS)
#'   scores from initial vital signs.
#' @param evitals_26_col A column containing alert, verbal, painful,
#'   unresponsive (AVPU) vital signs.
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
ttr_01_population <- function(df = NULL,
                   patient_scene_table = NULL,
                   response_table = NULL,
                   disposition_table = NULL,
                   vitals_table = NULL,
                   arrest_table = NULL,
                   erecord_01_col,
                   incident_date_col = NULL,
                   patient_dob_col = NULL,
                   epatient_15_col,
                   epatient_16_col,
                   eresponse_05_col,
                   transport_disposition_col,
                   earrest_01_col,
                   evitals_06_col,
                   evitals_07_col,
                   evitals_10_col,
                   evitals_12_col,
                   evitals_14_col,
                   evitals_23_col,
                   evitals_26_col
                   ) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(arrest_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&

    !is.null(df)

  ) {
    cli::cli_abort("{.fn ttr_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(arrest_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&

    is.null(df)

  ) {
    cli::cli_abort("{.fn ttr_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure all *_col arguments are fulfilled

  if (

    any(

      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_dob_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(transport_disposition_col),
      missing(earrest_01_col),
      missing(evitals_06_col),
      missing(evitals_07_col),
      missing(evitals_10_col),
      missing(evitals_12_col),
      missing(evitals_14_col),
      missing(evitals_23_col),
      missing(evitals_26_col)

    )


  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn ttr_01_population}.")
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
    "Running `ttr_01_population()`",
    total = 13,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # Create objects that are filter helpers throughout the function

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # define transports
  no_transport_responses <- "4230009|patient refused transport|no transport|4230013"

  # cardiac arrest response
  cardiac_arrest_response <- "3001003|Yes, Prior to Any EMS Arrival"

  # AVPU responses
  avpu_responses <- "3326001|Alert|3326003|Verbal|3326005|Painful|3326007|Unresponsive"

  # minor values
  minor_values <- "days|hours|minutes|months"

  year_values <- "2516009|years"

  day_values <- "days|2516001"

  hour_values <- "hours|2516003"

  minute_values <- "minutes|2516005"

  month_values <- "months|2516007"

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(arrest_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
        is.data.frame(arrest_table) || tibble::is_tibble(arrest_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table),
        is.data.frame(disposition_table) || tibble::is_tibble(disposition_table)

      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn ttr_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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

    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________

    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    ) {

      final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_dob_col }},
          units = "days"
        )) / 365,

        # system age check
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor = system_age_minor1 | system_age_minor2,

        # calculated age check
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18
        )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_dob_col)
      )) {

      final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(

          # system age check
          system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
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

    # 911 calls
    call_911_data <- response_table |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # no transports
    no_transport_data <- disposition_table |>
      dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(
          {{ transport_disposition_col }},
          ~ grepl(pattern = no_transport_responses, x = ., ignore.case = TRUE)
        )

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # cardiac arrest
    not_cardiac_arrest_data <- arrest_table |>
      dplyr::select({{ erecord_01_col }}, {{ earrest_01_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        !grepl(pattern = cardiac_arrest_response, x = {{ earrest_01_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

    # SBP, DBP, HR, RR
    vitals_data <- vitals_table |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }},
                    {{ evitals_12_col }}, {{ evitals_14_col }}, {{ evitals_23_col }}, {{ evitals_26_col }}
      ) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_all(
          c({{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }}, {{ evitals_12_col }}, {{ evitals_14_col }}), ~ !is.na(.)
        ),

       ( !is.na({{ evitals_23_col }}) | grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T) )

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

    # assign variables to final data
    computing_population <- final_data |>
      dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    NO_TRANSPORT = {{ erecord_01_col }} %in% no_transport_data,
                    NOT_CARDIAC_ARREST = {{ erecord_01_col }} %in% not_cardiac_arrest_data,
                    VITALS = {{ erecord_01_col }} %in% vitals_data
                    )

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        # 911 calls
        CALL_911,

        # not transported
        NO_TRANSPORT

      )

    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

    # Adult and Pediatric Populations

    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    ) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult | calc_age_adult,

                    NOT_CARDIAC_ARREST
      )

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor,

                    NOT_CARDIAC_ARREST
      )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_dob_col)
      )) {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult,

                      NOT_CARDIAC_ARREST
        )

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor,

                      NOT_CARDIAC_ARREST
        )

    }

    # summarize

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Non-transports",
                 "Non-cardiac arrest",
                 "Non-null SBP, DBP, HR, SPO2, RR, and GCS or AVPU",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$NO_TRANSPORT, na.rm = T),
        sum(computing_population$NOT_CARDIAC_ARREST, na.rm = T),
        sum(computing_population$VITALS, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    ttr.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(ttr.01.population)

    } else if (
      any(
        is.null(patient_scene_table),
        is.null(vitals_table),
        is.null(arrest_table),
        is.null(disposition_table),
        is.null(response_table)
      ) &&

      !is.null(df)

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
          !rlang::quo_is_null(rlang::enquo(patient_dob_col))
        )
      ) {
        incident_date <- rlang::enquo(incident_date_col)
        patient_dob <- rlang::enquo(patient_dob_col)

        if (
          (!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))
        ) {
          cli::cli_abort(
            "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
          )
        }
      }

      ###_____________________________________________________________________________
      # fact table
      # the user should ensure that variables beyond those supplied for calculations
      # are distinct (i.e. one value or cell per patient)
      ###_____________________________________________________________________________

      cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_dob_col))
        )
      ) {

        final_data <- df |>
          dplyr::select(-c({{ eresponse_05_col }},
                           {{ transport_disposition_col }},
                           {{ earrest_01_col }},
                           {{ evitals_06_col }},
                           {{ evitals_07_col }},
                           {{ evitals_10_col }},
                           {{ evitals_12_col }},
                           {{ evitals_14_col }},
                           {{ evitals_23_col }},
                           {{ evitals_26_col }}

          )) |>
          dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
          dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
            time1 = {{ incident_date_col }},
            time2 = {{ patient_dob_col }},
            units = "days"
          )) / 365,

          # system age check
          system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor = system_age_minor1 | system_age_minor2,

          # calculated age check
          calc_age_adult = patient_age_in_years_col >= 18,
          calc_age_minor = patient_age_in_years_col < 18
          )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_dob_col)
        )) {

        final_data <- df |>
          dplyr::select(-c({{ eresponse_05_col }},
                           {{ transport_disposition_col }},
                           {{ earrest_01_col }},
                           {{ evitals_06_col }},
                           {{ evitals_07_col }},
                           {{ evitals_10_col }},
                           {{ evitals_12_col }},
                           {{ evitals_14_col }},
                           {{ evitals_23_col }},
                           {{ evitals_26_col }}

          )) |>
          dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
          dplyr::mutate(

            # system age check
            system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
            system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
            system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
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

      # 911 calls
      call_911_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
        dplyr::distinct() |>
        dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

      # no transports
      no_transport_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          dplyr::if_any(
            {{ transport_disposition_col }},
            ~ grepl(pattern = no_transport_responses, x = ., ignore.case = TRUE)
          )

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

      # cardiac arrest
      not_cardiac_arrest_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{ earrest_01_col }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          !grepl(pattern = cardiac_arrest_response, x = {{ earrest_01_col }}, ignore.case = T)

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

      # SBP, DBP, HR, RR
      vitals_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }},
                      {{ evitals_12_col }}, {{ evitals_14_col }}, {{ evitals_23_col }}, {{ evitals_26_col }}
        ) |>
        dplyr::distinct() |>
        dplyr::filter(

          dplyr::if_all(
            c({{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }}, {{ evitals_12_col }}, {{ evitals_14_col }}), ~ !is.na(.)
          ),

          ( !is.na({{ evitals_23_col }}) | grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T) )

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

      # assign variables to final data
      computing_population <- final_data |>
        dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                      NO_TRANSPORT = {{ erecord_01_col }} %in% no_transport_data,
                      NOT_CARDIAC_ARREST = {{ erecord_01_col }} %in% not_cardiac_arrest_data,
                      VITALS = {{ erecord_01_col }} %in% vitals_data
        )

      # get the initial population
      initial_population <- computing_population |>
        dplyr::filter(

          # 911 calls
          CALL_911,

          # not transported
          NO_TRANSPORT

        )

      cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

      # Adult and Pediatric Populations

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_dob_col))
        )
      ) {

        # filter adult
        adult_pop <- initial_population |>
          dplyr::filter(system_age_adult | calc_age_adult,

                        NOT_CARDIAC_ARREST
          )

        cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

        # filter peds
        peds_pop <- initial_population |>
          dplyr::filter(system_age_minor | calc_age_minor,

                        NOT_CARDIAC_ARREST
          )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_dob_col)
        )) {

        # filter adult
        adult_pop <- initial_population |>
          dplyr::filter(system_age_adult,

                        NOT_CARDIAC_ARREST
          )

        cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

        # filter peds
        peds_pop <- initial_population |>
          dplyr::filter(system_age_minor,

                        NOT_CARDIAC_ARREST
          )

      }

      # summarize

      cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

      # summarize counts for populations filtered
      filter_counts <- tibble::tibble(
        filter = c("911 calls",
                   "Non-transports",
                   "Non-cardiac arrest",
                   "Non-null SBP, DBP, HR, SPO2, RR, and GCS or AVPU",
                   "Adults denominator",
                   "Peds denominator",
                   "Initial population",
                   "Total dataset"
        ),
        count = c(
          sum(computing_population$CALL_911, na.rm = T),
          sum(computing_population$NO_TRANSPORT, na.rm = T),
          sum(computing_population$NOT_CARDIAC_ARREST, na.rm = T),
          sum(computing_population$VITALS, na.rm = T),
          nrow(adult_pop),
          nrow(peds_pop),
          nrow(initial_population),
          nrow(computing_population)
        )
      )

      # get the populations of interest

      cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

      # gather data into a list for multi-use output
      ttr.01.population <- list(
        filter_process = filter_counts,
        adults = adult_pop,
        peds = peds_pop,
        initial_population = initial_population
      )

      cli::cli_progress_done(id = progress_bar_population)

      return(ttr.01.population)

    }

  }

