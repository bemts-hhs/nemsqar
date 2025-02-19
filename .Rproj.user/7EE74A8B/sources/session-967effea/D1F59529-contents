#' @title Trauma-01 Population
#'
#' @description
#'
#' Filters data down to the target populations for Trauma-08, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories to records that are 911 requests for patients with
#' injury who were assessed for pain based on specific criteria and calculates
#' related ECG measures. This function segments the data by age into adult and
#' pediatric populations.
#'
#' @param df A data frame or tibble containing EMS records. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data.frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name representing the EMS record ID.
#' @param incident_date_col Column name for the incident date. Default is
#'   `NULL`.
#' @param patient_DOB_col Column name for the patient's date of birth. Default
#'   is `NULL`.
#' @param epatient_15_col Column name for the patient's age in numeric format.
#' @param epatient_16_col Column name for the unit of age (e.g., "Years",
#'   "Months").
#' @param esituation_02_col Column name indicating if the situation involved an
#'   injury.
#' @param eresponse_05_col Column name for the type of EMS response (e.g., 911
#'   call).
#' @param evitals_23_col Column name for the Glasgow Coma Scale (GCS) total
#'   score.
#' @param evitals_26_col Column name for AVPU (Alert, Voice, Pain, Unresponsive)
#'   status.
#' @param evitals_27_col Column name for the pain scale assessment.
#' @param edisposition_28_col Column name for patient care disposition details.
#' @param transport_disposition_col Column name for transport disposition
#'   details.
#'
#' @return #' A list that contains the following:
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
#'     incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
#'     patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
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
#'     esituation_02 = rep("Yes", 5),
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     evitals_23 = rep(15, 5),
#'     evitals_26 = rep("Alert", 5),
#'     evitals_27 = c(0, 2, 4, 6, 8)
#'   )
#'
#'   # disposition table
#'   disposition_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     edisposition_28 = rep(4228001, 5),
#'     edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
#'   )
#'
#'   # test the success of the function
#'   result <- trauma_01_population(patient_scene_table = patient_table,
#'                      response_table = response_table,
#'                      situation_table = situation_table,
#'                      vitals_table = vitals_table,
#'                      disposition_table = disposition_table,
#'                      erecord_01_col = erecord_01,
#'                      epatient_15_col = epatient_15,
#'                      epatient_16_col = epatient_16,
#'                      eresponse_05_col = eresponse_05,
#'                      esituation_02_col = esituation_02,
#'                      evitals_23_col = evitals_23,
#'                      evitals_26_col = evitals_26,
#'                      evitals_27_col = evitals_27,
#'                      edisposition_28_col = edisposition_28,
#'                      transport_disposition_col = edisposition_30
#'                      )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_01_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      disposition_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      evitals_23_col,
                      evitals_26_col,
                      evitals_27_col,
                      edisposition_28_col,
                      transport_disposition_col
                      ) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn trauma_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn trauma_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure all *_col arguments are fulfilled

  if (
    any(
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(esituation_02_col),
      missing(eresponse_05_col),
      missing(evitals_23_col),
      missing(evitals_26_col),
      missing(evitals_27_col),
      missing(edisposition_28_col),
      missing(transport_disposition_col)
    )

  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn trauma_01_population}.")
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
    "Running `trauma_01_population()`",
    total = 14,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # Create objects that are filter helpers throughout the function

  # injury values
  possible_injury <- "Yes|9922005"

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # avpu not values
  avpu_values <- "Alert|3326001"

  # patient care provided
  care_provided <- "4228001|Patient Evaluated and Care Provided"

  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"

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
        is.data.frame(situation_table) || tibble::is_tibble(situation_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table),
        is.data.frame(disposition_table) || tibble::is_tibble(disposition_table)

      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn trauma_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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

  final_data <- patient_scene_table |>
  dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
  dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} < 18 & {{ epatient_15_col }} >= 2) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
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

    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
      dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} < 18 & {{ epatient_15_col }} >= 2) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
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

# GCS

GCS_data <- vitals_table |>
  dplyr::select({{ erecord_01_col }}, {{ evitals_23_col }}) |>
  dplyr::distinct() |>
  dplyr::filter({{ evitals_23_col }} == 15) |>
  dplyr::distinct({{ erecord_01_col }}) |>
  dplyr::pull({{ erecord_01_col }})

cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

# AVPU

AVPU_data <- vitals_table |>
  dplyr::select({{ erecord_01_col }}, {{ evitals_26_col }}) |>
  dplyr::distinct() |>
  dplyr::filter(grepl(pattern = avpu_values,
                      x = {{ evitals_26_col }},
                      ignore.case = T)
                ) |>
  dplyr::distinct({{ erecord_01_col }}) |>
  dplyr::pull({{ erecord_01_col }})

cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

# possible injury

possible_injury_data <- situation_table |>
  dplyr::select({{ erecord_01_col }}, {{ esituation_02_col }}) |>
  dplyr::distinct() |>
  dplyr::filter(grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)) |>
  dplyr::distinct({{ erecord_01_col }}) |>
  dplyr::pull({{ erecord_01_col }})

cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

# patient care provided

patient_care_data <- disposition_table |>
  dplyr::select({{ erecord_01_col }}, {{ edisposition_28_col }}) |>
  dplyr::distinct() |>
  dplyr::filter(grepl(pattern = care_provided, x = {{ edisposition_28_col }}, ignore.case = T)) |>
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

# transports

  transport_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      dplyr::if_any(
        {{ transport_disposition_col }},
        ~ grepl(pattern = transport_responses, x = ., ignore.case = TRUE)
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

# pain scale

  pain_scale_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_27_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      !is.na({{ evitals_27_col }})

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

# assign variables to final data
  computing_population <- final_data |>
  dplyr::mutate(
         GCS = {{ erecord_01_col }} %in% GCS_data,
         AVPU = {{ erecord_01_col }} %in% AVPU_data,
         CALL_911 = {{ erecord_01_col }} %in% call_911_data,
         TRANSPORT = {{ erecord_01_col }} %in% transport_data,
         INJURY = {{ erecord_01_col }} %in% possible_injury_data,
         PATIENT_CARE = {{ erecord_01_col }} %in% patient_care_data,
         PAIN_SCALE = {{ erecord_01_col }} %in% pain_scale_data
         )

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      # injuries
      INJURY,

      # GCS = 15 or AVPU = alert
      (GCS | AVPU),

      # 911 calls
      CALL_911,

      # patient evaluated and care provided
      PATIENT_CARE,

      # transports
      TRANSPORT
    )

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

# Adult and Pediatric Populations


  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

# filter adult
adult_pop <- initial_population |>
  dplyr::filter(system_age_adult | calc_age_adult)

cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

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

    cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }

cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

# summarize counts for populations filtered
filter_counts <- tibble::tibble(
  filter = c("911 calls",
             "GCS is 15",
             "AVPU is alert",
             "Transports",
             "Injury cases",
             "Patient evaluated and care provided",
             "Pain scale administered",
             "Adults denominator",
             "Peds denominator",
             "Initial population",
             "Total dataset"
  ),
  count = c(
    sum(computing_population$CALL_911, na.rm = T),
    sum(computing_population$GCS, na.rm = T),
    sum(computing_population$AVPU, na.rm = T),
    sum(computing_population$TRANSPORT, na.rm = T),
    sum(computing_population$INJURY, na.rm = T),
    sum(computing_population$PATIENT_CARE, na.rm = T),
    sum(computing_population$PAIN_SCALE, na.rm = T),
    nrow(adult_pop),
    nrow(peds_pop),
    nrow(initial_population),
    nrow(computing_population)
  )
)

# get the populations of interest

cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

# gather data into a list for multi-use output
trauma.01.population <- list(
  filter_process = filter_counts,
  adults = adult_pop,
  peds = peds_pop,
  initial_population = initial_population
)

cli::cli_progress_done(id = progress_bar_population)

return(trauma.01.population)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(disposition_table),
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

      final_data <- df |>
        dplyr::select(-c({{ esituation_02_col }},
                         {{ eresponse_05_col }},
                         {{ evitals_23_col }},
                         {{ evitals_26_col }},
                         {{ evitals_27_col }},
                         {{ edisposition_28_col }},
                         {{ transport_disposition_col }}
        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) / 365,

        # system age check
        system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
        system_age_minor1 = ({{ epatient_15_col }} < 18 & {{ epatient_15_col }} >= 2) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
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

      final_data <- df |>
        dplyr::select(-c({{ esituation_02_col }},
                         {{ eresponse_05_col }},
                         {{ evitals_23_col }},
                         {{ evitals_26_col }},
                         {{ evitals_27_col }},
                         {{ edisposition_28_col }},
                         {{ transport_disposition_col }}
        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
        dplyr::mutate(

          # system age check
          system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor1 = ({{ epatient_15_col }} < 18 & {{ epatient_15_col }} >= 2) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
          system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
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

    # GCS

    GCS_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_23_col }}) |>
      dplyr::distinct() |>
      dplyr::filter({{ evitals_23_col }} == 15) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # AVPU

    AVPU_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_26_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = avpu_values,
                          x = {{ evitals_26_col }},
                          ignore.case = T)
      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # possible injury

    possible_injury_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ esituation_02_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

    # patient care provided

    patient_care_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ edisposition_28_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(grepl(pattern = care_provided, x = {{ edisposition_28_col }}, ignore.case = T)) |>
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

    # transports

    transport_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        dplyr::if_any(
          {{ transport_disposition_col }},
          ~ grepl(pattern = transport_responses, x = ., ignore.case = TRUE)
        )

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

    # pain scale

    pain_scale_data <- df |>
      dplyr::select({{ erecord_01_col }}, {{ evitals_27_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        !is.na({{ evitals_27_col }})

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # assign variables to final data
    computing_population <- final_data |>
      dplyr::mutate(
        GCS = {{ erecord_01_col }} %in% GCS_data,
        AVPU = {{ erecord_01_col }} %in% AVPU_data,
        CALL_911 = {{ erecord_01_col }} %in% call_911_data,
        TRANSPORT = {{ erecord_01_col }} %in% transport_data,
        INJURY = {{ erecord_01_col }} %in% possible_injury_data,
        PATIENT_CARE = {{ erecord_01_col }} %in% patient_care_data,
        PAIN_SCALE = {{ erecord_01_col }} %in% pain_scale_data
      )

    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        # injuries
        INJURY,

        # GCS = 15 or AVPU = alert
        (GCS | AVPU),

        # 911 calls
        CALL_911,

        # patient evaluated and care provided
        PATIENT_CARE,

        # transports
        TRANSPORT
      )

    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

    # Adult and Pediatric Populations


    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {

      # filter adult
      adult_pop <- initial_population |>
        dplyr::filter(system_age_adult | calc_age_adult)

      cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

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

      cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

      # filter peds
      peds_pop <- initial_population |>
        dplyr::filter(system_age_minor)

    }

    cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "GCS is 15",
                 "AVPU is alert",
                 "Transports",
                 "Injury cases",
                 "Patient evaluated and care provided",
                 "Pain scale administered",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$GCS, na.rm = T),
        sum(computing_population$AVPU, na.rm = T),
        sum(computing_population$TRANSPORT, na.rm = T),
        sum(computing_population$INJURY, na.rm = T),
        sum(computing_population$PATIENT_CARE, na.rm = T),
        sum(computing_population$PAIN_SCALE, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )

    # get the populations of interest

    cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    trauma.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )

    cli::cli_progress_done(id = progress_bar_population)

    return(trauma.01.population)

  }

}
