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
#' @inheritParams airway_01_population
#' @inheritParams safety_02_population
#' @param injury_table A data frame or tibble containing fields from eInjury
#'   needed for this measure's calculations.
#' @param einjury_03_col Column describing Trauma triage criteria for the red
#' boxes (Injury Patterns and Mental Status and Vital Signs) in the 2021 ACS
#' National Guideline for the Field Triage of Injured Patients.
#' @param edisposition_14_col Column giving the position of the patient during
#' transport from the scene.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
#' * a tibble with a summary of missingness for each column in each table
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
safety_04_population <- function(
  df = NULL,
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
  # ensure that not all table arguments AND the df argument are fulfilled ----
  # user only passes df or all table arguments

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    ) &&

      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn safety_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn safety_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # ensure all *_col arguments are fulfilled ----
  if (
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
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn safety_04_population}."
    )
  }

  # options for the progress bar ----
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")

  options(
    cli.progress_bar_style = list(
      complete = cli::col_green("\u25CF"), # Black Circle
      incomplete = cli::col_br_white("\u2500") # Light Horizontal Line
    )
  )

  # initiate the progress bar process ----
  progress_bar_population <- cli::cli_progress_bar(
    "Running `safety_04_population()`",
    total = 13,
    type = "tasks",
    clear = FALSE,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING ----
  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    ) &&
      !is.null(df)
  ) {
    # Ensure df is a data frame or tibble
    validate_data_structure(
      input = df,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      logic = "or",
      type = "error"
    )

    # make tables from df ----
    # patient
    patient_scene_table <- df |>
      dplyr::select(
        -{{ eresponse_05_col }},
        -{{ earrest_01_col }},
        -{{ einjury_03_col }},
        -{{ eprocedures_03_col }},
        -{{ edisposition_14_col }},
        -c({{ transport_disposition_col }})
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eresponse_05_col }}
      ) |>
      dplyr::distinct()

    # arrest ----
    arrest_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ earrest_01_col }}
      ) |>
      dplyr::distinct()

    # injury ----
    injury_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ einjury_03_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
      ) |>
      dplyr::distinct()

    # disposition ----
    disposition_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ edisposition_14_col }},
        {{ transport_disposition_col }}
      ) |>
      dplyr::distinct()
  } else if (
    # utilize applicable tables to analyze the data for the measure ----
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    # Ensure all tables are of class `data.frame` or `tibble` ----
    validate_data_structure(
      input = patient_scene_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = response_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = arrest_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = injury_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = procedures_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = disposition_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    # get distinct tables when passed to table arguments ----
    # patient
    patient_scene_table <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- response_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eresponse_05_col }}
      ) |>
      dplyr::distinct()

    # arrest ----
    arrest_table <- arrest_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ earrest_01_col }}
      ) |>
      dplyr::distinct()

    # injury ----
    injury_table <- injury_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ einjury_03_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- procedures_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
      ) |>
      dplyr::distinct()

    # disposition ----
    disposition_table <- disposition_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ edisposition_14_col }},
        {{ transport_disposition_col }}
      ) |>
      dplyr::distinct()
  }

  # Validate date columns if provided ----
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    # Use quasiquotation on the date variables to check format ----
    incident_date <- rlang::enquo(incident_date_col)
    patient_dob <- rlang::enquo(patient_DOB_col)

    # Convert quosures to names and check the column classes ----
    incident_date_name <- rlang::as_name(incident_date)
    patient_dob_name <- rlang::as_name(patient_dob)

    validate_class(
      input = patient_scene_table[[incident_date_name]],
      class_type = c("date", "date-time"),
      logic = "or",
      type = "error",
      var_name = "incident_date_col"
    )

    validate_class(
      input = patient_scene_table[[patient_dob_name]],
      class_type = c("date", "date-time"),
      logic = "or",
      type = "error",
      var_name = "patient_DOB_col"
    )
  }

  ###___________________________________________________________________________
  # Estimate missingness in each table for included columns in the measure ----
  ###___________________________________________________________________________

  # utilize the internal `nemsqa_missing_summary` to estimate missingness
  missings <- nemsqa_missing_summary(
    patient_scene_table,
    response_table,
    arrest_table,
    injury_table,
    procedures_table,
    disposition_table
  )

  cli::cli_progress_update(
    set = 1,
    id = progress_bar_population,
    force = TRUE
  )

  ###_____________________________________________________________________________
  # fact table ----
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
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
      dplyr::mutate(
        patient_age_in_years = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) /
          365,

        # system age check ----
        system_age_minor1 = {{ epatient_15_col }} <= 8 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor2 = {{ epatient_15_col }} < 96 &
          grepl(
            pattern = month_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor3 = {{ epatient_15_col }} <= 120 &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor = system_age_minor1 |
          system_age_minor2 |
          system_age_minor3,

        # calculated age check ----
        calc_age_minor = patient_age_in_years <= 8
      )
  } else if (
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
      dplyr::mutate(
        # system age check ----
        system_age_minor1 = {{ epatient_15_col }} <= 8 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor2 = {{ epatient_15_col }} < 96 &
          grepl(
            pattern = month_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor3 = {{ epatient_15_col }} <= 120 &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor = system_age_minor1 |
          system_age_minor2 |
          system_age_minor3
      )
  }

  ###_____________________________________________________________________________
  ### dimension tables ----
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(
    set = 2,
    id = progress_bar_population,
    force = TRUE
  )

  # transports ----
  transport_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
    dplyr::filter(
      dplyr::if_any(
        {{ transport_disposition_col }},
        ~ grepl(pattern = transport_responses, x = ., ignore.case = TRUE)
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 3,
    id = progress_bar_population,
    force = TRUE
  )

  # interfacility ----
  interfacility_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::filter(
      grepl(
        pattern = interfacility_transport_code,
        x = {{ eresponse_05_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 4,
    id = progress_bar_population,
    force = TRUE
  )

  # cardiac arrest ----
  cardiac_arrest_data <- arrest_table |>
    dplyr::select({{ erecord_01_col }}, {{ earrest_01_col }}) |>
    dplyr::filter(
      grepl(
        pattern = cardiac_arrest_responses,
        x = {{ earrest_01_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 5,
    id = progress_bar_population,
    force = TRUE
  )

  # severe injury ----
  severe_injury_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::filter(
      grepl(
        pattern = trauma_triage_crit_safety_04,
        x = {{ einjury_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 6,
    id = progress_bar_population,
    force = TRUE
  )

  # long board ----
  long_board_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::filter(
      grepl(
        pattern = long_board,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 7,
    id = progress_bar_population,
    force = TRUE
  )

  # airway procedure ----
  airway_proc_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::filter(
      grepl(
        pattern = airway_procedures_safety_04,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 8,
    id = progress_bar_population,
    force = TRUE
  )

  # car seat ----
  car_seat_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ edisposition_14_col }}) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
    dplyr::filter(
      grepl(
        pattern = car_seat,
        x = {{ edisposition_14_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 9,
    id = progress_bar_population,
    force = TRUE
  )

  # assign variables to final data ----
  computing_population <- final_data |>
    dplyr::mutate(
      TRANSPORT = {{ erecord_01_col }} %in% transport_data,
      INTERFACILITY = {{ erecord_01_col }} %in% interfacility_data,
      TRANSPORT_OR_INTERFACILITY = TRANSPORT | INTERFACILITY,
      CARDIAC_ARREST = {{ erecord_01_col }} %in% cardiac_arrest_data,
      SEVERE_INJURY = {{ erecord_01_col }} %in% severe_injury_data,
      LONG_BOARD = {{ erecord_01_col }} %in% long_board_data,
      AIRWAY_PROCEDURE = {{ erecord_01_col }} %in% airway_proc_data,
      CAR_SEAT = {{ erecord_01_col }} %in% car_seat_data
    )

  cli::cli_progress_update(
    set = 10,
    id = progress_bar_population,
    force = TRUE
  )

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    # get the initial population ----
    initial_population <- computing_population |>
      dplyr::filter(
        # filter down to age < 8 years ----
        system_age_minor | calc_age_minor,

        # NEMSIS 3.5 transports / interfacility only ----
        TRANSPORT_OR_INTERFACILITY
      )
  } else if (
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    # get the initial population ----
    initial_population <- computing_population |>
      dplyr::filter(
        # filter down to age < 8 years ----
        system_age_minor,

        # NEMSIS 3.5 transports / interfacility only ----
        TRANSPORT_OR_INTERFACILITY
      )
  }

  # Only calculate for pediatric patients < 8 yrs of age ----

  cli::cli_progress_update(
    set = 11,
    id = progress_bar_population,
    force = TRUE
  )

  # filter peds for the exclusion criteria ----
  peds_pop <- initial_population |>
    dplyr::filter(
      !CARDIAC_ARREST &
        !SEVERE_INJURY &
        !LONG_BOARD &
        !AIRWAY_PROCEDURE
    )

  # get the summary of results ----

  cli::cli_progress_update(
    set = 12,
    id = progress_bar_population,
    force = TRUE
  )

  # summarize counts for populations filtered ----
  filter_counts <- tibble::tibble(
    filter = c(
      "Transport runs",
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
      sum(computing_population$TRANSPORT, na.rm = TRUE),
      sum(computing_population$INTERFACILITY, na.rm = TRUE),
      sum(computing_population$CARDIAC_ARREST, na.rm = TRUE),
      sum(computing_population$SEVERE_INJURY, na.rm = TRUE),
      sum(computing_population$LONG_BOARD, na.rm = TRUE),
      sum(computing_population$AIRWAY_PROCEDURE, na.rm = TRUE),
      sum(computing_population$CAR_SEAT, na.rm = TRUE),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest ----

  cli::cli_progress_update(
    set = 13,
    id = progress_bar_population,
    force = TRUE
  )

  # gather data into a list for multi-use output ----
  safety.04.population <- list(
    filter_process = filter_counts,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population,
    missingness = missings
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(safety.04.population)
}
