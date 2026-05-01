#' @title Hypoglycemia-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Hypoglycemia-01, and
#' categorizes records to identify needed information for the calculations.
#'
#' Identifies key categories related to diabetes/hypoglycemia incidents in an
#' EMS dataset, specifically focusing on cases where 911 was called for
#' diabetes/hypoglycemia distress, certain medications were administered, and a
#' weight is taken. This function segments the data into pediatric populations,
#' computing the proportion of cases that have a documented weight.
#'
#' @inheritParams airway_01_population
#' @inheritParams asthma_01_population
#' @param evitals_18_col Column for blood glucose levels.
#' @param evitals_23_col Column for Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col Column for AVPU alertness levels.
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
#' # patient table
#' patient_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'   "2025-01-01", "2025-06-01")),
#'   patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'   "2023-01-01", "1970-06-01")),
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
#'   esituation_11 = c(rep("E13.64", 3), rep("E16.2", 2)),
#'   esituation_12 = c(rep("E13.64", 2), rep("E16.2", 3))
#' )
#'
#' # medications table
#' medications_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   emedications_03 = c(372326, 376937, 377980, 4850, 4832),
#'
#' )
#'
#' # vitals table
#' vitals_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   evitals_18 = c(60, 59, 58, 57, 56),
#'   evitals_23 = c(16, 15, 14, 13, 12),
#'   evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert")
#'
#' )
#'
#' # procedures table
#' procedures_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eprocedures_03 = rep("710925007", 5)
#'
#' )
#'
#' # test the success of the function
#' result <- hypoglycemia_01_population(patient_scene_table = patient_table,
#'                             response_table = response_table,
#'                             situation_table = situation_table,
#'                             medications_table = medications_table,
#'                             vitals_table = vitals_table,
#'                             procedures_table = procedures_table,
#'                             erecord_01_col = erecord_01,
#'                             incident_date_col = incident_date,
#'                             patient_DOB_col = patient_dob,
#'                             epatient_15_col = epatient_15,
#'                             epatient_16_col = epatient_16,
#'                             eresponse_05_col = eresponse_05,
#'                             esituation_11_col = esituation_11,
#'                             esituation_12_col = esituation_12,
#'                             emedications_03_col = emedications_03,
#'                             evitals_18_col = evitals_18,
#'                             evitals_23_col = evitals_23,
#'                             evitals_26_col = evitals_26,
#'                             eprocedures_03_col = eprocedures_03
#'                             )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
hypoglycemia_01_population <- function(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  medications_table = NULL,
  procedures_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_18_col,
  evitals_23_col,
  evitals_26_col,
  emedications_03_col,
  eprocedures_03_col
) {
  # ensure that not all table arguments AND the df argument are fulfilled ----
  # user only passes df or all table arguments
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(medications_table),
      !is.null(procedures_table)
    ) &&

      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # ensure that df or all table arguments are fulfilled ----
  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(medications_table),
      is.null(procedures_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
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
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(evitals_18_col),
      missing(evitals_23_col),
      missing(evitals_26_col),
      missing(emedications_03_col),
      missing(eprocedures_03_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn hypoglycemia_01_population}."
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
    "Running `hypoglycemia_01_population()`",
    total = 17,
    type = "tasks",
    clear = FALSE,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING ----
  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(medications_table),
      is.null(procedures_table)
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
        -{{ esituation_11_col }},
        -{{ esituation_12_col }},
        -{{ emedications_03_col }},
        -{{ eresponse_05_col }},
        -{{ eprocedures_03_col }},
        -{{ evitals_18_col }},
        -{{ evitals_23_col }},
        -{{ evitals_26_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- df |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct()

    # situation ----
    situation_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ esituation_11_col }},
        {{ esituation_12_col }}
      ) |>
      dplyr::distinct()

    # medications ----
    medications_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ emedications_03_col }}
      ) |>
      dplyr::distinct()

    # vitals ----
    vitals_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_18_col }},
        {{ evitals_23_col }},
        {{ evitals_26_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
      ) |>
      dplyr::distinct()
  } else if (
    # utilize applicable tables to analyze the data for the measure ----
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(medications_table),
      !is.null(procedures_table)
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
      input = situation_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = medications_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = vitals_table,
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

    # get distinct tables when passed to table arguments ----
    # patient
    patient_scene_table <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- response_table |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct()

    # situation ----
    situation_table <- situation_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ esituation_11_col }},
        {{ esituation_12_col }}
      ) |>
      dplyr::distinct()

    # medications ----
    medications_table <- medications_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ emedications_03_col }}
      ) |>
      dplyr::distinct()

    # vitals ----
    vitals_table <- vitals_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_18_col }},
        {{ evitals_23_col }},
        {{ evitals_26_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- procedures_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
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
  # Estimate missingness in each table for included columns in the measure
  ###___________________________________________________________________________

  # utilize the internal `nemsqa_missing_summary` to estimate missingness
  missings <- nemsqa_missing_summary(
    patient_scene_table,
    response_table,
    situation_table,
    procedures_table,
    vitals_table,
    medications_table
  )

  progress_bar_population

  # progress update, these will be repeated throughout the script ----
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
    # filter the table to get the initial population ----
    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
      dplyr::mutate(
        patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) /
          365,
        patient_age_in_days_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )),

        # system age check ----
        system_age_adult = {{ epatient_15_col }} >= 18 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor1 = {{ epatient_15_col }} < 18 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor2 = !is.na({{ epatient_15_col }}) &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor3 = !({{ epatient_15_col }} < 1 &
          grepl(
            pattern = day_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          )) &
          !({{ epatient_15_col }} < 24 &
            grepl(
              pattern = hour_values,
              x = {{ epatient_16_col }},
              ignore.case = TRUE
            )) &
          !({{ epatient_15_col }} < 120 &
            grepl(
              pattern = minute_values,
              x = {{ epatient_16_col }},
              ignore.case = TRUE
            )),
        system_age_minor = (system_age_minor1 | system_age_minor2) &
          system_age_minor3,

        # calculated age check ----
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18 &
          patient_age_in_days_col >= 1
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
        system_age_adult = {{ epatient_15_col }} >= 18 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor1 = {{ epatient_15_col }} < 18 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor2 = !is.na({{ epatient_15_col }}) &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_minor3 = !({{ epatient_15_col }} < 1 &
          grepl(
            pattern = day_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          )) &
          !({{ epatient_15_col }} < 24 &
            grepl(
              pattern = hour_values,
              x = {{ epatient_16_col }},
              ignore.case = TRUE
            )) &
          !({{ epatient_15_col }} < 120 &
            grepl(
              pattern = minute_values,
              x = {{ epatient_16_col }},
              ignore.case = TRUE
            )),
        system_age_minor = (system_age_minor1 | system_age_minor2) &
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

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 2,
    id = progress_bar_population,
    force = TRUE
  )

  # altered mental status 1 ----
  altered_data1 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = altered_mental_status,
      x = {{ esituation_11_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 3,
    id = progress_bar_population,
    force = TRUE
  )

  # altered mental status 2 ----
  altered_data2 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = altered_mental_status,
      x = {{ esituation_12_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 4,
    id = progress_bar_population,
    force = TRUE
  )

  # AVPU ----
  AVPU_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_26_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = avpu_responses,
      x = {{ evitals_26_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 5,
    id = progress_bar_population,
    force = TRUE
  )

  # GCS ----
  GCS_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter({{ evitals_23_col }} < 15) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 6,
    id = progress_bar_population,
    force = TRUE
  )

  # diabetes data 1 ----
  diabetes_data1 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = diabetes_codes,
      x = {{ esituation_11_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 7,
    id = progress_bar_population,
    force = TRUE
  )

  # diabetes data 2 ----
  diabetes_data2 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = diabetes_codes,
      x = {{ esituation_12_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 8,
    id = progress_bar_population,
    force = TRUE
  )

  # blood glucose ----
  blood_glucose_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_18_col }}) |>
    dplyr::distinct() |>
    dplyr::filter({{ evitals_18_col }} < 60) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 9,
    id = progress_bar_population,
    force = TRUE
  )

  # 911 calls ----
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = codes_911,
      x = {{ eresponse_05_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 10,
    id = progress_bar_population,
    force = TRUE
  )

  # correct treatment 1 ----
  correct_treatment_data1 <- medications_table |>
    dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ emedications_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 11,
    id = progress_bar_population,
    force = TRUE
  )

  # correct treatment 2 ----
  correct_treatment_data2 <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 12,
    id = progress_bar_population,
    force = TRUE
  )

  # assign variables to the final data ----
  computing_population <- final_data |>
    dplyr::mutate(
      GCS = {{ erecord_01_col }} %in% GCS_data,
      AVPU = {{ erecord_01_col }} %in% AVPU_data,
      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
      ALTERED1 = {{ erecord_01_col }} %in% altered_data1,
      ALTERED2 = {{ erecord_01_col }} %in% altered_data2,
      ALTERED = ALTERED1 | ALTERED2,
      DIABETES1 = {{ erecord_01_col }} %in% diabetes_data1,
      DIABETES2 = {{ erecord_01_col }} %in% diabetes_data2,
      DIABETES = DIABETES1 | DIABETES2,
      BLOOD_GLUCOSE = {{ erecord_01_col }} %in% blood_glucose_data,
      TREATMENT1 = {{ erecord_01_col }} %in% correct_treatment_data1,
      TREATMENT2 = {{ erecord_01_col }} %in% correct_treatment_data2,
      TREATMENT = TREATMENT1 | TREATMENT2
    )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 13,
    id = progress_bar_population,
    force = TRUE
  )

  # get the initial population ----

  initial_population <- computing_population |>
    dplyr::filter(
      (DIABETES & (GCS | AVPU)) |

        (ALTERED & BLOOD_GLUCOSE) &

          CALL_911,

      system_age_minor3
    )

  # Adult and Pediatric Populations ----

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 14,
    id = progress_bar_population,
    force = TRUE
  )

  if (
    # use the system generated and calculated ages ----

    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    # filter adult ----
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult | calc_age_adult)

    # progress update, these will be repeated throughout the script ----
    cli::cli_progress_update(
      set = 15,
      id = progress_bar_population,
      force = TRUE
    )

    # filter peds ----
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor)
  } else if (
    # only use the system generated values ----

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    # filter adult ----
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)

    # progress update, these will be repeated throughout the script ----
    cli::cli_progress_update(
      set = 15,
      id = progress_bar_population,
      force = TRUE
    )

    # filter peds ----
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)
  }

  # summarize ----

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(
    set = 16,
    id = progress_bar_population,
    force = TRUE
  )

  # summarize counts for populations filtered ----
  filter_counts <- tibble::tibble(
    filter = c(
      "Diabetes/Hypoglycemia and Verbal, Painful, Unresponsive or GCS < 15",
      "Altered mental status and low blood glucose",
      "911 calls",
      "Adults denominator",
      "Peds denominator",
      "Initial population",
      "Total dataset"
    ),
    count = c(
      sum(
        computing_population$DIABETES &
          (computing_population$AVPU | computing_population$GCS),
        na.rm = TRUE
      ),
      sum(
        computing_population$ALTERED & computing_population$BLOOD_GLUCOSE,
        na.rm = TRUE
      ),
      sum(computing_population$CALL_911, na.rm = TRUE),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  cli::cli_progress_update(
    set = 17,
    id = progress_bar_population,
    force = TRUE
  )

  # get the population of interest ----
  hypoglycemia.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population,
    missingness = missings
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(hypoglycemia.01.population)
}
