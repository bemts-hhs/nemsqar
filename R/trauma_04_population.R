#' @title Trauma-04 Populations
#'
#' @description
#'
#' This function processes EMS data to generate the population needed to
#' calculated the Trauma-04 NEMSQA measure.
#'
#' @inheritParams airway_01_population
#' @inheritParams asthma_01_population
#' @inheritParams safety_02_population
#' @inheritParams safety_04_population
#' @inheritParams trauma_01_population
#' @inheritParams pediatrics_03b_population
#' @param eresponse_10_col Column name containing informatin about scene delays,
#' if any, of the EMS unit associated with the EMS event.
#' @param edisposition_02_col Column name containing the code of the destination
#' the patient was delivered or transferred to.
#' @param trauma_center_facility_IDs A character vector of trauma center
#' facility IDs that will allow destination facilities documented in
#' `edisposition_02_col` to be classified correctly as trauma centers when
#' applicable.
#' @param edisposition_23_col `r lifecycle::badge("deprecated")` Use
#' `edisposition_02_col` instead. You must also pass a character vector of
#' trauma center facility IDs to `trauma_center_facility_IDs` to ensure that
#' destination facility IDs passed via `edisposition_02_col` are correctly
#' identified as trauma centers as applicable.
#' @param evitals_10_col Column name containing the patient's heart rate
#' expressed as a number per minute.
#' @param evitals_14_col Column name containing the patient's respiratory rate
#' expressed as a number per minute.
#' @param evitals_15_col Column name containing the patient's respiratory
#' effort.
#' @param evitals_21_col Column name containing the patient's Glasgow Coma Score
#' Motor response.
#' @param eexam_16_col Column name containing the assessment findings associated
#' with the patient's extremities.
#' @param eexam_20_col Column name containing the assessment findings of the
#' patient's neurological examination.
#' @param eexam_23_col Column name containing the assessment findings associated
#' with the patient's lungs.
#' @param eexam_25_col Column name containing the assessment findings associated
#' with the patient's chest.
#' @param einjury_01_col Column name containing the category of the
#' reported/suspected external cause of the injury.
#' @param einjury_04_col Column name containing Trauma triage criteria for the
#' yellow boxes (Mechanism of Injury and EMS Judgment) in the current ACS
#' National Guideline for the Field Triage of Injured Patients.
#' @param einjury_09_col Column name containing the distance in feet the patient
#' fell, measured from the lowest point of the patient to the ground.
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
#'     eresponse_05 = rep(2205001, 5),
#'     eresponse_10 = rep(2210011, 5)
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
#'     evitals_06 = c(100, 90, 80, 70, 85),
#'     evitals_10 = c(110, 89, 88, 71, 85),
#'     evitals_12 = c(50, 60, 70, 80, 75),
#'     evitals_14 = c(30, 9, 8, 7, 31),
#'     evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
#'     evitals_21 = c(5, 4, 3, 2, 1)
#'   )
#'
#'   # disposition table
#'   disposition_table <- tibble::tibble(
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   edisposition_02 = as.character(c(
#'     9908029,
#'     9908027,
#'     9908025,
#'     9908023,
#'     9876543
#'   )),
#'   edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
#' )
#'
#'   # injury table
#'   injury_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
#'     einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
#'     einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
#'     einjury_09 = c(11, 12, 13, 14, 15)
#'   )
#'
#'   # exam table
#'   exam_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
#'     eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
#'     eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
#'     eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023)
#'   )
#'
#'   # procedures table
#'   procedures_table <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003)
#'   )
#'
#' # test the success of the function
#' result <- trauma_04_population(
#'   patient_scene_table = patient_table,
#'   response_table = response_table,
#'   situation_table = situation_table,
#'   vitals_table = vitals_table,
#'   disposition_table = disposition_table,
#'   exam_table = exam_table,
#'   injury_table = injury_table,
#'   procedures_table = procedures_table,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   eresponse_05_col = eresponse_05,
#'   eresponse_10_col = eresponse_10,
#'   esituation_02_col = esituation_02,
#'   evitals_06_col = evitals_06,
#'   evitals_10_col = evitals_10,
#'   evitals_12_col = evitals_12,
#'   evitals_14_col = evitals_14,
#'   evitals_15_col = evitals_15,
#'   evitals_21_col = evitals_21,
#'   eexam_16_col = eexam_16,
#'   eexam_20_col = eexam_20,
#'   eexam_23_col = eexam_23,
#'   eexam_25_col = eexam_25,
#'   edisposition_02_col = edisposition_02,
#'   trauma_center_facility_IDs = as.character(c(
#'     9908029,
#'     9908027,
#'     9908025,
#'     9908023,
#'     9908021
#'   )),
#'   transport_disposition_col = edisposition_30,
#'   eprocedures_03_col = eprocedures_03,
#'   einjury_01_col = einjury_01,
#'   einjury_03_col = einjury_03,
#'   einjury_04_col = einjury_04,
#'   einjury_09_col = einjury_09
#' )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_04_population <- function(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  exam_table = NULL,
  procedures_table = NULL,
  injury_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  esituation_02_col,
  eresponse_05_col,
  eresponse_10_col,
  transport_disposition_col,
  edisposition_23_col = lifecycle::deprecated(),
  edisposition_02_col,
  trauma_center_facility_IDs,
  evitals_06_col,
  evitals_10_col,
  evitals_12_col,
  evitals_14_col,
  evitals_15_col,
  evitals_21_col,
  eexam_16_col,
  eexam_20_col,
  eexam_23_col,
  eexam_25_col,
  eprocedures_03_col,
  einjury_01_col,
  einjury_03_col,
  einjury_04_col,
  einjury_09_col
) {
  # deprecate the argument `edisposition_23_col`
  if (!missing(edisposition_23_col)) {
    lifecycle::deprecate_stop(
      when = "1.2.0",
      what = "trauma_04_population(edisposition_23_col)",
      with = "trauma_04_population(edisposition_02_col)",
      details = "Along with `trauma_04_population(edisposition_02_col)`, users must pass a character vector of trauma center facility IDs to `trauma_center_facility_IDs` that will allow facility IDs passed via `edisposition_02_col` to be correctly identifed as trauma centers."
    )
  }

  # ensure that not all table arguments AND the df argument are fulfilled ----
  # user only passes df or all table arguments
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn trauma_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # ensure that df or all table arguments are fulfilled ----
  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn trauma_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
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
      missing(esituation_02_col),
      missing(eresponse_05_col),
      missing(eresponse_10_col),
      missing(transport_disposition_col),
      missing(edisposition_02_col),
      missing(evitals_06_col),
      missing(evitals_10_col),
      missing(evitals_12_col),
      missing(evitals_14_col),
      missing(evitals_15_col),
      missing(evitals_21_col),
      missing(eexam_16_col),
      missing(eexam_20_col),
      missing(eexam_23_col),
      missing(eexam_25_col),
      missing(eprocedures_03_col),
      missing(einjury_01_col),
      missing(einjury_03_col),
      missing(einjury_04_col),
      missing(einjury_09_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing. Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn trauma_04_population}."
    )
  }

  if (missing(trauma_center_facility_IDs)) {
    cli::cli_abort(
      "{.var trauma_center_facility_IDs} is missing. Please make sure you pass a {.cls character} vector to {.var trauma_center_facility_IDs} to run {.fn trauma_04_population}."
    )
  }

  # validate the trauma center IDs as character or factor
  validate_character_factor(input = trauma_center_facility_IDs, type = "error")

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
    "Running `trauma_04_population()`",
    total = 31,
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
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) &&
      !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics ----
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
        -{{ esituation_02_col }},
        -{{ edisposition_02_col }},
        -c({{ transport_disposition_col }}),
        -{{ eresponse_05_col }},
        -{{ eresponse_10_col }},
        -{{ evitals_06_col }},
        -{{ evitals_10_col }},
        -{{ evitals_12_col }},
        -{{ evitals_14_col }},
        -{{ evitals_15_col }},
        -{{ evitals_21_col }},
        -{{ eexam_16_col }},
        -{{ eexam_20_col }},
        -{{ eexam_23_col }},
        -{{ eexam_25_col }},
        -{{ eprocedures_03_col }},
        -{{ einjury_01_col }},
        -{{ einjury_03_col }},
        -{{ einjury_04_col }},
        -{{ einjury_09_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eresponse_05_col }},
        {{ eresponse_10_col }}
      ) |>
      dplyr::distinct()

    # situation ----
    situation_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ esituation_02_col }}
      ) |>
      dplyr::distinct()

    # vitals ----
    vitals_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_06_col }},
        {{ evitals_10_col }},
        {{ evitals_12_col }},
        {{ evitals_14_col }},
        {{ evitals_15_col }},
        {{ evitals_21_col }}
      ) |>
      dplyr::distinct()

    # disposition ----
    disposition_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ edisposition_02_col }},
        c({{ transport_disposition_col }})
      ) |>
      dplyr::distinct()

    # exam ----
    exam_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eexam_16_col }},
        {{ eexam_20_col }},
        {{ eexam_23_col }},
        {{ eexam_25_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
      ) |>
      dplyr::distinct()

    # injury ----
    injury_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ einjury_01_col }},
        {{ einjury_03_col }},
        {{ einjury_04_col }},
        {{ einjury_09_col }}
      ) |>
      dplyr::distinct()
  } else if (
    # utilize applicable tables to analyze the data for the measure ----
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    # Ensure df is a data frame or tibble ----
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
      input = vitals_table,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    validate_data_structure(
      input = exam_table,
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
      input = injury_table,
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
    # patient ----
    patient_scene_table <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # response ----
    response_table <- response_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eresponse_05_col }},
        {{ eresponse_10_col }}
      ) |>
      dplyr::distinct()

    # situation ----
    situation_table <- situation_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ esituation_02_col }}
      ) |>
      dplyr::distinct()

    # vitals ----
    vitals_table <- vitals_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_06_col }},
        {{ evitals_10_col }},
        {{ evitals_12_col }},
        {{ evitals_14_col }},
        {{ evitals_15_col }},
        {{ evitals_21_col }}
      ) |>
      dplyr::distinct()

    # disposition ----
    disposition_table <- disposition_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ edisposition_02_col }},
        c({{ transport_disposition_col }})
      ) |>
      dplyr::distinct()

    # exam ----
    exam_table <- exam_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eexam_16_col }},
        {{ eexam_20_col }},
        {{ eexam_23_col }},
        {{ eexam_25_col }}
      ) |>
      dplyr::distinct()

    # procedures ----
    procedures_table <- procedures_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ eprocedures_03_col }}
      ) |>
      dplyr::distinct()

    # injury ----
    injury_table <- injury_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ einjury_01_col }},
        {{ einjury_03_col }},
        {{ einjury_04_col }},
        {{ einjury_09_col }}
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

  progress_bar_population

  ###_____________________________________________________________________________
  # fact table ----
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  ###_____________________________________________________________________________

  cli::cli_progress_update(
    set = 1,
    id = progress_bar_population,
    force = TRUE
  )

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
      dplyr::mutate(
        patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) /
          365,

        # system age check ----
        system_age_65 = {{ epatient_15_col }} >= 65 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_64 = ({{ epatient_15_col }} < 65 &
          {{ epatient_15_col }} >= 10) &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_1 = {{ epatient_15_col }} < 10 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_2 = {{ epatient_15_col }} < 120 &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10 = system_age_10_1 | system_age_10_2,

        # calculated age check ----
        calc_age_65 = patient_age_in_years_col >= 65,
        calc_age_10_64 = patient_age_in_years_col < 65 &
          patient_age_in_years_col >= 10,
        calc_age_10 = patient_age_in_years_col < 10
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
        system_age_65 = {{ epatient_15_col }} >= 65 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_64 = ({{ epatient_15_col }} < 65 &
          {{ epatient_15_col }} >= 10) &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_1 = {{ epatient_15_col }} < 10 &
          grepl(
            pattern = year_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10_2 = {{ epatient_15_col }} < 120 &
          grepl(
            pattern = minor_values,
            x = {{ epatient_16_col }},
            ignore.case = TRUE
          ),
        system_age_10 = system_age_10_1 | system_age_10_2
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

  # possible injury ----
  possible_injury_data <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_02_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = possible_injury,
        x = {{ esituation_02_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 3,
    id = progress_bar_population,
    force = TRUE
  )

  # transports ----
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

  cli::cli_progress_update(
    set = 4,
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

  # GCS ----
  GCS_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_21_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern = GCS_motor_values,
      x = {{ evitals_21_col }},
      ignore.case = TRUE
    )) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 5,
    id = progress_bar_population,
    force = TRUE
  )

  # lung assessment ----
  lung_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = lung_assessment_values,
        x = {{ eexam_23_col }},
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

  # chest assessment ----
  chest_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_25_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(pattern = chest_assessment_values, x = {{ eexam_25_col }})
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 7,
    id = progress_bar_population,
    force = TRUE
  )

  # respiratory effort ----
  respiratory_effort_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_15_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = respiratory_effort_values,
        x = {{ evitals_15_col }},
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

  # airway management ----
  airway_management_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = airway_management_values,
        x = {{ eprocedures_03_col }},
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

  # pulse oximetry ----
  pulse_oximetry_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ evitals_12_col }} < 90
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 10,
    id = progress_bar_population,
    force = TRUE
  )

  # SBP ----
  SBP_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ evitals_06_col }} < 110
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 11,
    id = progress_bar_population,
    force = TRUE
  )

  # heart rate and SBP ----
  HR_SBP_data_10_65_plus <- vitals_table |>
    dplyr::select(
      {{ erecord_01_col }},
      {{ evitals_10_col }},
      {{ evitals_06_col }}
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ evitals_10_col }} > {{ evitals_06_col }}
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 12,
    id = progress_bar_population,
    force = TRUE
  )

  # trauma triage criteria steps 1 and 2 age 65+ ----
  trauma_triage_1_2_data_65 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = trauma_triage_1_2_values_65,
        x = {{ einjury_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 13,
    id = progress_bar_population,
    force = TRUE
  )

  # trauma triage criteria steps 1 and 2 age 10 - 65 ----
  trauma_triage_1_2_data_10_64 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = trauma_triage_1_2_values_10_64,
        x = {{ einjury_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 14,
    id = progress_bar_population,
    force = TRUE
  )

  # trauma triage criteria steps 1 and 2 age < 10 ----
  trauma_triage_1_2_data_10 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = trauma_triage_1_2_values_10,
        x = {{ einjury_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 15,
    id = progress_bar_population,
    force = TRUE
  )

  # extremities assessment ----
  extremities_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = extremities_assessment_values,
        x = {{ eexam_16_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 16,
    id = progress_bar_population,
    force = TRUE
  )

  # neurological assessment ----
  neurological_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_20_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = neurological_assessment_values,
        x = {{ eexam_20_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 17,
    id = progress_bar_population,
    force = TRUE
  )

  # tourniquet ----
  tourniquet_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = tourniquet_values,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 18,
    id = progress_bar_population,
    force = TRUE
  )

  # trauma triage criteria steps 3 and 4 ----
  trauma_triage_3_4_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_04_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = trauma_triage_3_4_values,
        x = {{ einjury_04_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 19,
    id = progress_bar_population,
    force = TRUE
  )

  # fall height ----
  fall_height_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_09_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ einjury_09_col }} > 10
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 20,
    id = progress_bar_population,
    force = TRUE
  )

  # scene delay ----
  scene_delay_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_10_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = scene_delay_values,
        x = {{ eresponse_10_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 21,
    id = progress_bar_population,
    force = TRUE
  )

  # cause of injury ----
  cause_of_injury_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_01_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      grepl(
        pattern = cause_of_injury_values,
        x = {{ einjury_01_col }},
        ignore.case = TRUE
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 22,
    id = progress_bar_population,
    force = TRUE
  )

  # respiratory rate for < 10 yrs population ----
  respiratory_rate_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_14_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ evitals_14_col }} < 10 | {{ evitals_14_col }} > 29
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 23,
    id = progress_bar_population,
    force = TRUE
  )

  # SBP check variable for ages < 10 years ----
  # if using calculated and system ages
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    SBP_age_10_data <- final_data |>
      dplyr::select(
        {{ erecord_01_col }},
        system_age_10,
        calc_age_10,
        {{ epatient_15_col }}
      ) |>
      dplyr::distinct() |>
      dplyr::filter(system_age_10 | calc_age_10) |>
      dplyr::left_join(
        vitals_table |>
          dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |>
          dplyr::distinct(),

        by = rlang::as_name(rlang::enquo(erecord_01_col))
      ) |>
      dplyr::mutate(
        SBP_10 = ({{ evitals_06_col }} + ({{ epatient_15_col }} * 2)) >= 70
      ) |>
      dplyr::filter(SBP_10) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    # SBP check variable for ages < 10 years ----
    # if using system ages only
  } else if (
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    SBP_age_10_data <- final_data |>
      dplyr::select(
        {{ erecord_01_col }},
        system_age_10,
        {{ epatient_15_col }}
      ) |>
      dplyr::distinct() |>
      dplyr::filter(system_age_10) |>
      dplyr::left_join(
        vitals_table |>
          dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |>
          dplyr::distinct(),

        by = rlang::as_name(rlang::enquo(erecord_01_col))
      ) |>
      dplyr::mutate(
        SBP_10 = ({{ evitals_06_col }} + ({{ epatient_15_col }} * 2)) >= 70
      ) |>
      dplyr::filter(SBP_10) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})
  }

  cli::cli_progress_update(
    set = 24,
    id = progress_bar_population,
    force = TRUE
  )

  # hospital capability ----
  trauma_center_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ edisposition_02_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      {{ edisposition_02_col }} %in% trauma_center_facility_IDs
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 25,
    id = progress_bar_population,
    force = TRUE
  )

  # assign variables to final data ----
  computing_population <- final_data |>
    dplyr::mutate(
      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
      TRANSPORTS = {{ erecord_01_col }} %in% transport_data,
      POSSIBLE_INJURY = {{ erecord_01_col }} %in% possible_injury_data,
      GCS = {{ erecord_01_col }} %in% GCS_data,
      LUNG = {{ erecord_01_col }} %in% lung_assessment_data,
      CHEST = {{ erecord_01_col }} %in% chest_data,
      RESPIRATORY_EFFORT = {{ erecord_01_col }} %in% respiratory_effort_data,
      AIRWAY_MANAGEMENT = {{ erecord_01_col }} %in% airway_management_data,
      EXTREMITIES = {{ erecord_01_col }} %in% extremities_assessment_data,
      NEURO = {{ erecord_01_col }} %in% neurological_assessment_data,
      TOURNIQUET = {{ erecord_01_col }} %in% tourniquet_data,
      TRAUMA_TRIAGE_3_4 = {{ erecord_01_col }} %in% trauma_triage_3_4_data,
      FALL_HEIGHT = {{ erecord_01_col }} %in% fall_height_data,
      SCENE_DELAY = {{ erecord_01_col }} %in% scene_delay_data,
      INJURY_CAUSE = {{ erecord_01_col }} %in% cause_of_injury_data,
      PULSE_OXIMETRY = {{ erecord_01_col }} %in% pulse_oximetry_data,
      SBP = {{ erecord_01_col }} %in% SBP_data,
      SBP_10 = {{ erecord_01_col }} %in% SBP_age_10_data,
      HR_SBP_10_65_PLUS = {{ erecord_01_col }} %in% HR_SBP_data_10_65_plus,
      TRAUMA_TRIAGE_1_2_65 = {{ erecord_01_col }} %in%
        trauma_triage_1_2_data_65,
      TRAUMA_TRIAGE_1_2_10_64 = {{ erecord_01_col }} %in%
        trauma_triage_1_2_data_10_64,
      TRAUMA_TRIAGE_1_2_10 = {{ erecord_01_col }} %in%
        trauma_triage_1_2_data_10,
      RESPIRATORY_RATE_10 = {{ erecord_01_col }} %in% respiratory_rate_data,
      TRAUMA_CENTER = {{ erecord_01_col }} %in% trauma_center_data
    )

  cli::cli_progress_update(
    set = 26,
    id = progress_bar_population,
    force = TRUE
  )

  # get the initial population ----
  initial_population <- computing_population |>
    dplyr::filter(
      POSSIBLE_INJURY,
      CALL_911,
      TRANSPORTS
    )

  # Adult and Pediatric Populations ----

  cli::cli_progress_update(
    set = 27,
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
    # filter older adult ----
    pop_65 <- initial_population |>
      dplyr::filter(system_age_65 | calc_age_65) |>
      dplyr::filter(
        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_65 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )

    cli::cli_progress_update(
      set = 28,
      id = progress_bar_population,
      force = TRUE
    )

    # filter ages 10 to 65 ----
    pop_10_64 <- initial_population |>
      dplyr::filter(system_age_10_64 | calc_age_10_64) |>
      dplyr::filter(
        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )

    cli::cli_progress_update(
      set = 29,
      id = progress_bar_population,
      force = TRUE
    )

    # filter ages < 10 ----
    pop_10 <- initial_population |>
      dplyr::filter(system_age_10 | calc_age_10) |>
      dplyr::filter(
        GCS |
          RESPIRATORY_RATE_10 |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          SBP_10 |
          TRAUMA_TRIAGE_1_2_10 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )
  } else if (
    # only use the system generated values ----

    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    # filter older adult ----
    pop_65 <- initial_population |>
      dplyr::filter(system_age_65) |>
      dplyr::filter(
        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )

    cli::cli_progress_update(
      set = 28,
      id = progress_bar_population,
      force = TRUE
    )

    # filter ages 10 to 65 ----
    pop_10_64 <- initial_population |>
      dplyr::filter(system_age_10_64) |>
      dplyr::filter(
        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )

    cli::cli_progress_update(
      set = 29,
      id = progress_bar_population,
      force = TRUE
    )

    # filter ages < 10 ----
    pop_10 <- initial_population |>
      dplyr::filter(system_age_10) |>
      dplyr::filter(
        GCS |
          RESPIRATORY_RATE_10 |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          SBP_10 |
          TRAUMA_TRIAGE_1_2_10 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE
      )
  }

  # summarize ----
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(
    set = 30,
    id = progress_bar_population,
    force = TRUE
  )

  # summarize counts for populations filtered ----
  filter_counts <- tibble::tibble(
    filter = c(
      "Situation possible injury",
      "911 calls",
      "Transports",
      "GCS Motor 1-5",
      "Breath sounds absent, decreased, increased respiratory effort",
      "Flail segment, retraction, accessory muscles used in breathing",
      "Respiratory effort apneic, labored, mech. assist, rapid, shallow, weak/agonal",
      "Airway management procedures",
      "Pulse oximetry < 90",
      "SBP < 110",
      "Respiratory rate < 10 or > 29 ages < 10 yrs",
      "(SBP + (Pt. Age * 2)) >= 70 ages < 10 yrs",
      "Heart rate greater than SBP ages >= 10 yrs",
      "Met trauma triage criteria 1-2 ages 65+ yrs",
      "Met trauma triage criteria 1-2 ages 10-64 yrs",
      "Met trauma triage criteria 1-2 ages < 10 yrs",
      "Motor function abnormal/weakness or sensation absent",
      "Hemiplegia left/right, weakness left/right",
      "Tournique procedure",
      "Trauma triage criteria 3-4",
      "Cause of Injury matches V20-V39, V80, V86",
      "Scene delay = extrication",
      "Fall > 10 ft.",
      "Transported to a Trauma Center",
      "Patients 65+ yrs denominator",
      "Patients 10-64 yrs denominator",
      "Patients < 10 yrs denominator",
      "Initial population",
      "Total dataset"
    ),
    count = c(
      sum(computing_population$POSSIBLE_INJURY, na.rm = TRUE),
      sum(computing_population$CALL_911, na.rm = TRUE),
      sum(computing_population$TRANSPORTS, na.rm = TRUE),
      sum(computing_population$GCS, na.rm = TRUE),
      sum(computing_population$LUNG, na.rm = TRUE),
      sum(computing_population$CHEST, na.rm = TRUE),
      sum(computing_population$RESPIRATORY_EFFORT, na.rm = TRUE),
      sum(computing_population$AIRWAY_MANAGEMENT, na.rm = TRUE),
      sum(computing_population$PULSE_OXIMETRY, na.rm = TRUE),
      sum(computing_population$SBP, na.rm = TRUE),
      sum(computing_population$RESPIRATORY_RATE_10, na.rm = TRUE),
      sum(computing_population$SBP_10, na.rm = TRUE),
      sum(computing_population$HR_SBP_10_65_PLUS, na.rm = TRUE),
      sum(computing_population$TRAUMA_TRIAGE_1_2_65, na.rm = TRUE),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10_64, na.rm = TRUE),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10, na.rm = TRUE),
      sum(computing_population$EXTREMITIES, na.rm = TRUE),
      sum(computing_population$NEURO, na.rm = TRUE),
      sum(computing_population$TOURNIQUET, na.rm = TRUE),
      sum(computing_population$TRAUMA_TRIAGE_3_4, na.rm = TRUE),
      sum(computing_population$INJURY_CAUSE, na.rm = TRUE),
      sum(computing_population$SCENE_DELAY, na.rm = TRUE),
      sum(computing_population$FALL_HEIGHT, na.rm = TRUE),
      sum(computing_population$TRAUMA_CENTER, na.rm = TRUE),
      nrow(pop_65),
      nrow(pop_10_64),
      nrow(pop_10),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  cli::cli_progress_update(
    set = 31,
    id = progress_bar_population,
    force = TRUE
  )

  # get the population of interest ----
  trauma.04.population <- list(
    filter_process = filter_counts,
    population_65 = pop_65,
    population_10_64 = pop_10_64,
    population_10 = pop_10,
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(trauma.04.population)
}
