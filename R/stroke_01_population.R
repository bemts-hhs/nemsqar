#' @title Stroke-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Stroke-01, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to stroke-related incidents in an EMS
#' dataset, specifically focusing on cases where 911 was called for stroke, and
#' a stroke scale was administered. .
#'
#' @inheritParams airway_01_population
#' @inheritParams asthma_01_population
#' @inheritParams hypoglycemia_01_population
#' @param evitals_29_col The column containing the stroke scale score achieved
#'   during assessment.
#' @param evitals_30_col The column containing stroke scale type descriptors
#'   (e.g., FAST, NIH, etc.).
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
#' * a tibble with a summary of missingness for each column in each table
#' 
#' @details
#'
#' Adult and pediatric populations are determined using NEMSIS patient age
#' fields. When both `incident_date_col` and `patient_DOB_col` are supplied,
#' calculated age is used in conjunction with reported age variables to improve
#' population classification.
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
#'     esituation_11 = c(rep("I60", 3), rep("I61", 2)),
#'     esituation_12 = c(rep("I63", 2), rep("I64", 3)),
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     evitals_23 = c(16, 15, 14, 13, 12),
#'     evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
#'     evitals_29 = rep("positive", 5),
#'     evitals_30 = rep("a pain scale", 5)
#'   )
#'
#'   # test the success of the function
#'   result <- stroke_01_population(patient_scene_table = patient_table,
#'                               response_table = response_table,
#'                               situation_table = situation_table,
#'                               vitals_table = vitals_table,
#'                               erecord_01_col = erecord_01,
#'                               incident_date_col = incident_date,
#'                               patient_DOB_col = patient_dob,
#'                               eresponse_05_col = eresponse_05,
#'                               esituation_11_col = esituation_11,
#'                               esituation_12_col = esituation_12,
#'                               evitals_29_col = evitals_29,
#'                               evitals_23_col = evitals_23,
#'                               evitals_26_col = evitals_26,
#'                               evitals_30_col = evitals_30
#'                               )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
stroke_01_population <- function(
  df = NULL,
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
  evitals_23_col,
  evitals_26_col,
  evitals_29_col,
  evitals_30_col
) {
  # Ensure that not all table arguments AND the df argument are fulfilled ----
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
    cli::cli_abort(
      "{.fn stroke_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn stroke_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
    )
  }

  # Ensure all *_col arguments are fulfilled ----

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
      missing(evitals_23_col),
      missing(evitals_26_col),
      missing(evitals_29_col),
      missing(evitals_30_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn stroke_01_population}."
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
    "Running `stroke_01_population()`",
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
      is.null(situation_table),
      is.null(vitals_table)
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
    # patient ----
    patient_scene_table <- df |>
      dplyr::select(
        -{{ esituation_11_col }},
        -{{ esituation_12_col }},
        -{{ eresponse_05_col }},
        -{{ evitals_23_col }},
        -{{ evitals_26_col }},
        -{{ evitals_29_col }},
        -{{ evitals_30_col }}
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

    # vitals ----
    vitals_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_23_col }},
        {{ evitals_26_col }},
        {{ evitals_29_col }},
        {{ evitals_30_col }}
      ) |>
      dplyr::distinct()
  } else if (
    # utilize applicable tables to analyze the data for the measure ----

    all(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
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
      input = vitals_table,
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

    # vitals ----
    vitals_table <- vitals_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ evitals_23_col }},
        {{ evitals_26_col }},
        {{ evitals_29_col }},
        {{ evitals_30_col }}
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
    situation_table,
    vitals_table
  )

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
    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
      dplyr::mutate(
        patient_age_in_years_col = as.numeric(difftime(
          time1 = {{ incident_date_col }},
          time2 = {{ patient_DOB_col }},
          units = "days"
        )) /
          365,

        # system age checks ----
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
        system_age_minor = system_age_minor1 | system_age_minor2,

        # calculated age checks ----
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18
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
        # system age checks ----
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
        system_age_minor = system_age_minor1 | system_age_minor2
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

  # stroke 1 ----
  stroke_data1 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_11_col }}) |>
    dplyr::filter(
      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_11_col }},
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

  # stroke 2 ----
  stroke_data2 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_12_col }}) |>
    dplyr::filter(
      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_12_col }},
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

  # 911 calls ----
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::filter(
      grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
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

  # GCS ----
  GCS_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_23_col }}) |>
    dplyr::filter(
      {{ evitals_23_col }} <= 9
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(
    set = 6,
    id = progress_bar_population,
    force = TRUE
  )

  # AVPU ----
  AVPU_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_26_col }}) |>
    dplyr::filter(
      grepl(
        pattern = avpu_unresponsive,
        x = {{ evitals_26_col }},
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

  # stroke scale 1 ----
  stroke_scale_data1 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_29_col }}) |>
    dplyr::filter(
      !is.na({{ evitals_29_col }}) &
        grepl(
          pattern = stroke_values,
          x = {{ evitals_29_col }},
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

  # stroke scale 2 ----
  stroke_scale_data2 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_30_col }}) |>
    dplyr::filter(
      !is.na({{ evitals_30_col }}) &
        grepl(
          pattern = stroke_scale_values,
          x = {{ evitals_30_col }},
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
      STROKE1 = {{ erecord_01_col }} %in% stroke_data1,
      STROKE2 = {{ erecord_01_col }} %in% stroke_data2,
      STROKE = STROKE1 | STROKE2,
      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
      GCS = {{ erecord_01_col }} %in% GCS_data,
      AVPU = {{ erecord_01_col }} %in% AVPU_data,
      NOT_GCS_AVPU = !(GCS | AVPU),
      STROKE_SCALE1 = {{ erecord_01_col }} %in% stroke_scale_data1,
      STROKE_SCALE2 = {{ erecord_01_col }} %in% stroke_scale_data2,
      STROKE_SCALE = STROKE_SCALE1 | STROKE_SCALE2
    )

  initial_population <- computing_population |>
    dplyr::filter(
      # Identify Records that have seizure documentation defined above ----
      STROKE,

      # filter down to 911 calls ----
      CALL_911,

      # no GCS < 9 or AVPU not equal to Unresponsive ----
      NOT_GCS_AVPU
    )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = TRUE)

  # Adult and Pediatric Populations ----

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

    cli::cli_progress_update(
      set = 11,
      id = progress_bar_population,
      force = TRUE
    )

    # filter peds ----
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor)
  } else if (
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
  ) {
    # filter adult ----
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)

    cli::cli_progress_update(
      set = 11,
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
    set = 12,
    id = progress_bar_population,
    force = TRUE
  )

  # get the summary of results ----
  filter_counts <- tibble::tibble(
    filter = c(
      "911 calls",
      "Stroke cases",
      "GCS <= 9",
      "AVPU = Unresponsive",
      "Non-Null Stroke Scale Score or Type",
      "Adults denominator",
      "Peds denominator",
      "Initial population",
      "Total dataset"
    ),
    count = c(
      sum(computing_population$CALL_911, na.rm = TRUE),
      sum(computing_population$STROKE, na.rm = TRUE),
      sum(computing_population$GCS, na.rm = TRUE),
      sum(computing_population$AVPU, na.rm = TRUE),
      sum(computing_population$STROKE_SCALE, na.rm = TRUE),
      nrow(adult_pop),
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
  stroke.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population,
    missingness = missings
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(stroke.01.population)
}
