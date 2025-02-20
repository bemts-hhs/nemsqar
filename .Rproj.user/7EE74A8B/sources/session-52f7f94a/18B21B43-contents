#' @title Safety-04 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Safety-04, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to a 911 request or interfacility request
#' for patients less than 8 years of age during which patients are transported
#' using a pediatric restraint device. This function segments the data by age
#' into adult and pediatric populations.
#'
#' @param df A data frame or tibble containing EMS data where each row
#'   represents an individual observation.
#' @param patient_scene_table A data frame or tibble containing fields from
#'   epatient and escene needed for this measure's calculations.
#' @param response_table A data frame or tibble containing fields from eresponse
#'   needed for this measure's calculations.
#' @param arrest_table A data frame or tibble containing fields from earrest
#'   needed for this measure's calculations.
#' @param injury_table A data frame or tibble containing fields from einjury
#'   needed for this measure's calculations.
#' @param procedures_table A data frame or tibble containing fields from
#'   eprocedures needed for this measure's calculations.
#' @param disposition_table A data frame or tibble containing fields from
#'   edisposition needed for this measure's calculations.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name indicating the patient age.
#' @param epatient_16_col Column name for the unit of age (e.g., "Years,"
#'   "Months").
#' @param eresponse_05_col Column containing response transport codes.
#' @param earrest_01_col Column with cardiac arrest status information.
#' @param einjury_03_col Column describing traumatic injuries, expected as a
#'   list or text-separated entries.
#' @param eprocedures_03_col Column listing procedures, assumed to contain
#'   multiple procedure codes/texts in each cell.
#' @param edisposition_14_col Column for transport dispositions.
#' @param transport_disposition_col Columns for primary and secondary transport
#'   dispositions.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
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
#'   # disposition table
#'   disposition_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     edisposition_14 = rep(4214001, 5),
#'     edisposition_30 = rep(4230001, 5),
#'   )
#'
#'   # arrest table
#'   arrest_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     earrest_01 = rep("No", 5)
#'   )
#'
#'   # injury table
#'   injury_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     einjury_03 = rep("non-injury", 5)
#'   )
#'
#'   # procedures table
#'   procedures_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     eprocedures_03 = rep("other response", 5)
#'   )
#'
#'   # test the success of the function
#'   result <- safety_04_population(patient_scene_table = patient_table,
#'                         response_table = response_table,
#'                         arrest_table = arrest_table,
#'                         injury_table = injury_table,
#'                         procedures_table = procedures_table,
#'                         disposition_table = disposition_table,
#'                         erecord_01_col = erecord_01,
#'                         incident_date_col = incident_date,
#'                         patient_DOB_col = patient_dob,
#'                         epatient_15_col = epatient_15,
#'                         epatient_16_col = epatient_16,
#'                         eresponse_05_col = eresponse_05,
#'                         earrest_01_col = earrest_01,
#'                         einjury_03_col = einjury_03,
#'                         edisposition_14_col = edisposition_14,
#'                         transport_disposition_col = edisposition_30,
#'                         eprocedures_03_col = eprocedures_03
#'                         )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_04_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      arrest_table = NULL,
                      injury_table = NULL,
                      procedures_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      earrest_01_col,
                      einjury_03_col,
                      eprocedures_03_col,
                      edisposition_14_col,
                      transport_disposition_col
                      ) {

  # ensure that not all table arguments AND the df argument are fulfilled
 # user only passes df or all table arguments

    if(

    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    )

    &&

    !is.null(df)

  ) {

    cli::cli_abort("{.fn safety_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # ensure that df or all table arguments are fulfilled

  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    )

    && is.null(df)

  ) {

    cli::cli_abort("{.fn safety_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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
      missing(earrest_01_col),
      missing(einjury_03_col),
      missing(eprocedures_03_col),
      missing(edisposition_14_col),
      missing(transport_disposition_col)

    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn safety_04_population}.")

  }

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions

  # transport code eresponse.05
  transport_code <- "2205005|Interfacility Transport"

  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"

  # get codes as a regex to find cardiac arrest responses
  cardiac_arrest_responses <- "3001005|3001003|Yes, Prior to Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)|Yes, After Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)"

  # get applicable trauma triage codes for steps 1 and 2
  trauma_triage_crit <- "2903001|Amputation proximal to wrist or ankle|2903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity|2903007|Glasgow Coma Score <=13|2903009|Open or depressed skull fracture|2903011|Paralysis|2903013|Pelvic fractures|2903015|All penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1 year\\) or need for ventilatory support|3903019|Systolic Blood Pressure <90 mmHg|2903021|Two or more long-bone fractures"

  # procedure exclusion related to long board

  long_board <- "450591000124106|Immobilization using long board"

  # additional procedures in the exclusion

  airway_procedures <- "16883004|Endotracheal intubation, emergency procedure|182682004|Emergency laryngeal intubation|232674004|Orotracheal intubation|232678001|Orotracheal fiberoptic intubation|232682004|Nasotracheal fiberoptic intubation|232685002|Insertion of tracheostomy tube|304341005|Awake intubation|418613003|Tracheal intubation through a laryngeal mask airway|424979004|Laryngeal mask airway insertion|427753009|Insertion of esophageal tracheal double lumen supraglottic airway|429161001|Insertion of endotracheal tube using laryngoscope|450611000124|Insertion of Single Lumen Supraglottic Airway Device"

  # car seat code for edisposition.14

  car_seat <- "4214001|Car Seat"

  # minor values
  minor_values <- "days|hours|minutes|2516001|2516003|2516005"

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
    "Running `safety_04_population()`",
    total = 13,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # utilize applicable tables to analyze the data for the measure
  if(

    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    )

    && is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table),
        is.data.frame(disposition_table) || tibble::is_tibble(disposition_table),
        is.data.frame(arrest_table) || tibble::is_tibble(arrest_table),
        is.data.frame(injury_table) || tibble::is_tibble(injury_table),
        is.data.frame(procedures_table) || tibble::is_tibble(procedures_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn safety_04_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,

    # system age check
    system_age_minor1 = {{ epatient_15_col}} <= 8 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col}} < 96 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor3 = {{ epatient_15_col}} <= 120 & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3,

    # calculated age check
    calc_age_minor = patient_age_in_years <= 8
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
    system_age_minor1 = {{ epatient_15_col}} <= 8 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col}} < 96 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor3 = {{ epatient_15_col}} <= 120 & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3

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

  # transports
  transport_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{  transport_disposition_col  }}) |>
    dplyr::filter(

    dplyr::if_any(
      {{ transport_disposition_col }},
      ~ grepl(pattern = transport_responses, x = ., ignore.case = TRUE)
    )

  ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # interfacility
  interfacility_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = transport_code,
        x = {{ eresponse_05_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # cardiac arrest
  cardiac_arrest_data <- arrest_table |>
    dplyr::select({{ erecord_01_col }}, {{  earrest_01_col  }}) |>
    dplyr::filter(

      grepl(pattern = cardiac_arrest_responses, x = {{ earrest_01_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # severe injury
  severe_injury_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{  einjury_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = trauma_triage_crit, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # long board
  long_board_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = long_board, x = {{  eprocedures_03_col  }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # airway procedure
  airway_proc_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = airway_procedures, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # car seat
  car_seat_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{  edisposition_14_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = car_seat, x = {{ edisposition_14_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(TRANSPORT = {{ erecord_01_col }} %in% transport_data,
                  INTERFACILITY = {{ erecord_01_col }} %in% interfacility_data,
                  TRANSPORT_OR_INTERFACILITY = TRANSPORT | INTERFACILITY,
                  CARDIAC_ARREST = {{ erecord_01_col }} %in% cardiac_arrest_data,
                  SEVERE_INJURY = {{ erecord_01_col }} %in% severe_injury_data,
                  LONG_BOARD = {{ erecord_01_col }} %in% long_board_data,
                  AIRWAY_PROCEDURE = {{ erecord_01_col }} %in% airway_proc_data,
                  CAR_SEAT = {{ erecord_01_col }} %in% car_seat_data
    )

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      # filter down to age < 8 years
      system_age_minor | calc_age_minor,

      # NEMSIS 3.5 transports / interfacility only
      TRANSPORT_OR_INTERFACILITY

    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )) {

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        # filter down to age < 8 years
        system_age_minor,

        # NEMSIS 3.5 transports / interfacility only
        TRANSPORT_OR_INTERFACILITY

      )

  }

  # Only calculate for pediatric patients < 8 yrs of age

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # filter peds for the exclusion criteria
  peds_pop <- initial_population |>
    dplyr::filter(!CARDIAC_ARREST &
                    !SEVERE_INJURY &
                    !LONG_BOARD &
                    !AIRWAY_PROCEDURE
    )

  # get the summary of results

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Transport runs",
               "Interfacility runs",
               "Cardiac arrest calls",
               "Severe injury calls",
               "Calls involving long board",
               "Calls involving an airway procedure",
               "Car seat used",
               "Peds denominator",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$TRANSPORT, na.rm = T),
      sum(computing_population$INTERFACILITY, na.rm = T),
      sum(computing_population$CARDIAC_ARREST, na.rm = T),
      sum(computing_population$SEVERE_INJURY, na.rm = T),
      sum(computing_population$LONG_BOARD, na.rm = T),
      sum(computing_population$AIRWAY_PROCEDURE, na.rm = T),
      sum(computing_population$CAR_SEAT, na.rm = T),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # gather data into a list for multi-use output
  safety.04.population <- list(
    filter_process = filter_counts,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(safety.04.population)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    )

    && !is.null(df)

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

  final_data <- df |>
        dplyr::select(-c({{ eresponse_05_col }},
                     {{ earrest_01_col }},
                     {{ einjury_03_col }},
                     {{ eprocedures_03_col }},
                     {{ edisposition_14_col }},
                     {{ transport_disposition_col }}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,

    # system age check
    system_age_minor1 = {{ epatient_15_col}} <= 8 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col}} < 96 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor3 = {{ epatient_15_col}} <= 120 & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3,

    # calculated age check
    calc_age_minor = patient_age_in_years <= 8
    )

    } else if(

      all(
        is.null(incident_date_col),
        is.null(patient_DOB_col)
      )) {

    final_data <- df |>
              dplyr::select(-c({{ eresponse_05_col }},
                     {{ earrest_01_col }},
                     {{ einjury_03_col }},
                     {{ eprocedures_03_col }},
                     {{ edisposition_14_col }},
                     {{ transport_disposition_col }}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(

    # system age check
    system_age_minor1 = {{ epatient_15_col}} <= 8 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col}} < 96 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor3 = {{ epatient_15_col}} <= 120 & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3

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

  # transports
  transport_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  transport_disposition_col  }}) |>
    dplyr::filter(

      dplyr::if_any(
        {{ transport_disposition_col }},
        ~ grepl(pattern = transport_responses, x = ., ignore.case = TRUE)
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # interfacility
  interfacility_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = transport_code,
        x = {{ eresponse_05_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # cardiac arrest
  cardiac_arrest_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  earrest_01_col  }}) |>
    dplyr::filter(

      grepl(pattern = cardiac_arrest_responses, x = {{ earrest_01_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # severe injury
  severe_injury_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  einjury_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = trauma_triage_crit, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # long board
  long_board_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = long_board, x = {{  eprocedures_03_col  }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # airway procedure
  airway_proc_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = airway_procedures, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # car seat
  car_seat_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  edisposition_14_col  }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::filter(

      grepl(pattern = car_seat, x = {{ edisposition_14_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(TRANSPORT = {{ erecord_01_col }} %in% transport_data,
                  INTERFACILITY = {{ erecord_01_col }} %in% interfacility_data,
                  TRANSPORT_OR_INTERFACILITY = TRANSPORT | INTERFACILITY,
                  CARDIAC_ARREST = {{ erecord_01_col }} %in% cardiac_arrest_data,
                  SEVERE_INJURY = {{ erecord_01_col }} %in% severe_injury_data,
                  LONG_BOARD = {{ erecord_01_col }} %in% long_board_data,
                  AIRWAY_PROCEDURE = {{ erecord_01_col }} %in% airway_proc_data,
                  CAR_SEAT = {{ erecord_01_col }} %in% car_seat_data
    )

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      # filter down to age < 8 years
      system_age_minor | calc_age_minor,

      # NEMSIS 3.5 transports / interfacility only
      TRANSPORT_OR_INTERFACILITY

    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )) {

    # get the initial population
    initial_population <- computing_population |>
      dplyr::filter(

        # filter down to age < 8 years
        system_age_minor,

        # NEMSIS 3.5 transports / interfacility only
        TRANSPORT_OR_INTERFACILITY

      )

  }

  # Only calculate for pediatric patients < 8 yrs of age

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # filter peds for the exclusion criteria
  peds_pop <- initial_population |>
    dplyr::filter(!CARDIAC_ARREST &
                    !SEVERE_INJURY &
                    !LONG_BOARD &
                    !AIRWAY_PROCEDURE
    )

  # get the summary of results

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Transport runs",
               "Interfacility runs",
               "Cardiac arrest calls",
               "Severe injury calls",
               "Calls involving long board",
               "Calls involving an airway procedure",
               "Car seat used",
               "Peds denominator",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$TRANSPORT, na.rm = T),
      sum(computing_population$INTERFACILITY, na.rm = T),
      sum(computing_population$CARDIAC_ARREST, na.rm = T),
      sum(computing_population$SEVERE_INJURY, na.rm = T),
      sum(computing_population$LONG_BOARD, na.rm = T),
      sum(computing_population$AIRWAY_PROCEDURE, na.rm = T),
      sum(computing_population$CAR_SEAT, na.rm = T),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # gather data into a list for multi-use output
  safety.04.population <- list(
    filter_process = filter_counts,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(safety.04.population)

  }

}
