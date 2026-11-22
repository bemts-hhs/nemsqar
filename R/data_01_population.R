#' @title Data-01 Population
#'
#' @description
#'
#' Creates the analysis populations for the Data-01 validity score measure.
#' The function prepares a computing population, overall denominator
#' population, adult population, and pediatric population for subsequent
#' validity score analyses.
#'
#' Data-01 evaluates EMS registry data validity using pre-calculated record
#' validity scores supplied by the EMS software vendor or registry. Validity
#' scores must be numeric values bounded to the interval `[0, 1]`, where
#' larger values indicate greater conformance with registry-defined data quality
#' expectations, such as completeness, valid values, formatting requirements,
#' and other validation rules. Users with validity scores reported as
#' percentages (e.g., `[0, 100]`) should convert scores to proportions prior to
#' using this function.
#'
#' The function supports two data input workflows:
#'
#' - A single analytic dataset supplied through `df`.
#' - Separate patient, audit, and disposition tables supplied through
#'   `patient_scene_table`, `audit_table`, and `disposition_table`.
#'
#' Records are restricted to patients with documented patient evaluation,
#' refusal, death, or treatment dispositions indicating patient contact.
#'
#' When incident and date-of-birth variables are supplied, both reported and
#' calculated ages are utilized to derive adult and pediatric populations.
#'
#' @inheritParams airway_01_population
#' @inheritParams safety_02_population
#' @param audit_table A data.frame or tibble containing audit-related fields,
#'   such as record creation timestamps and record-level validity scores.
#'   The contents of this table are typically registry- or vendor-specific and
#'   may not correspond to elements defined within the National EMS Information
#'   System (NEMSIS) data standard.
#' @param patient_care_col Disposition variable used to identify patient
#'   contact encounters.
#' @param validity_score_col Numeric validity score variable bounded to
#'   `[0, 1]`.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
#' * a tibble with a summary of missingness for each column in each table
#'
#' Additional variables are created in the returned populations including:
#'
#' - `PATIENT_CARE`
#' - `VALIDITY_SCORE_FORMAT`
#' - `VALIDITY_SCORE_RANGE`
#' - `VALIDITY_SCORE_RANGE_FORMAT`
#'
#' @details
#'
#' Adult and pediatric populations are determined using NEMSIS patient age
#' fields. When both `incident_date_col` and `patient_DOB_col` are supplied,
#' calculated age is used in conjunction with reported age variables to improve
#' population classification.
#'
#' Validity score ranges are categorized into predefined intervals used
#' throughout the Data-01 validity score reporting framework.
#'
#' @examples
#'
#' # Synthetic Data-01 example
#' test_data <- tibble::tibble(
#'   erecord_01 = paste0("R", 1:10),
#'   incident_date = as.Date("2024-01-01"),
#'   patient_dob = as.Date(c(
#'     "1980-01-01", "1975-01-01", "1990-01-01", "1985-01-01",
#'     "1970-01-01", "2015-01-01", "2012-01-01", "2010-01-01",
#'     "2018-01-01", "2016-01-01"
#'   )),
#'   epatient_15 = c(44, 49, 34, 39, 54, 9, 12, 14, 6, 8),
#'   epatient_16 = rep("Years", 10),
#'   patient_disposition = rep(
#'     "Patient Treated, Transported by this EMS Unit (4212033)",
#'     10
#'   ),
#'   incident_validity_score = c(
#'     1.00, 0.98, 0.96, 0.92, 0.89,
#'     0.84, 0.77, 0.65, 0.54, 0.41
#'   )
#' )
#'
#' # Create the Data-01 populations
#' data_01_population(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   patient_care_col = patient_disposition,
#'   validity_score_col = incident_validity_score
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
# define the function ----
data_01_population <- function(
  df = NULL,
  patient_scene_table = NULL,
  audit_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  patient_care_col,
  validity_score_col
) {
  # Check for tables or DF ----

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
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
      missing(patient_care_col),
      missing(validity_score_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn data_01_population}."
    )
  }

  # Preserve the user's existing cli progress bar style ----
  # Many *_population() functions temporarily modify the global
  # `cli.progress_bar_style` option to display custom progress bars.
  # Store the current value so it can be restored when the function exits,
  # regardless of whether execution completes successfully or errors.
  old_pb_style <- getOption("cli.progress_bar_style")

  # Restore the user's original progress bar style on function exit ----
  # This prevents package functions from permanently modifying global
  # session options and avoids unintended side effects elsewhere in R.
  on.exit(
    options(cli.progress_bar_style = old_pb_style),
    add = TRUE
  )

  # options for the progress bar ----
  # a green dot for progress
  # a white line for tasks not yet complete
  options(cli.progress_bar_style = "dot")

  options(
    cli.progress_bar_style = list(
      complete = cli::col_green("\u25CF"), # Black Circle
      incomplete = cli::col_br_white("\u2500") # Light Horizontal Line
    )
  )

  # initiate the progress bar process ----
  progress_bar_population <- cli::cli_progress_bar(
    "Running `data_01_population()`",
    total = 12,
    type = "tasks",
    clear = FALSE,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

  ####### CREATE SEPARATE TABLES FROM DF IF TABLES ARE MISSING ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table)
    ) &&
      !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics ----
  ) {
    # Ensure df is a data frame or tibble ----
    validate_data_structure(
      input = df,
      structure_type = c("data.frame", "tbl", "tbl_df"),
      type = "error",
      logic = "or"
    )

    # make tables from df ----
    # patient ----
    patient_scene_table <- df |>
      dplyr::select(
        -{{ patient_care_col }},
        -{{ validity_score_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # audit ----
    audit_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ validity_score_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # disposition ----
    disposition_table <- df |>
      dplyr::select({{ erecord_01_col }}, {{ patient_care_col }}) |>
      dplyr::distinct()
  } else if (
    # else continue with the tables passed to the applicable arguments ----

    all(
      !is.null(patient_scene_table),
      !is.null(audit_table),
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
      input = audit_table,
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

    # audit ----
    audit_table <- audit_table |>
      dplyr::select({{ erecord_01_col }}, {{ validity_score_col }}) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # disposition ----
    disposition_table <- disposition_table |>
      dplyr::select({{ erecord_01_col }}, {{ patient_care_col }}) |>
      dplyr::distinct()
  }

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

  # Data validation ----
  ## validate that validity score is numeric on a scale between [0,1]
  ### pull and check data
  validity_data <- validate_data_pull(
    input = audit_table,
    type = "error",
    col = {{ validity_score_col }},
    var_name = "validity_score_col",
    calls = 2
  )

  ### validate validity data scale
  validate_numeric(
    input = validity_data,
    min = 0,
    max = 1,
    na_ok = TRUE,
    null_ok = TRUE,
    finite = FALSE,
    type = "error",
    var_name = "validity_score_col",
    calls = 2
  )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

  # Only check the date columns if they are in fact passed ----
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

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

  ###___________________________________________________________________________
  # Estimate missingness in each table for included columns in the measure ----
  ###___________________________________________________________________________

  # utilize the internal `nemsqa_missing_summary` to estimate missingness
  missings <- nemsqa_missing_summary(
    patient_scene_table,
    audit_table,
    disposition_table
  )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

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

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # dimension tables ----
  # each dimension table is turned into a vector of unique IDs
  # that are then utilized on the fact table to create distinct variables
  # that tell if the patient had the characteristic or not for final
  # calculations of the numerator and filtering
  ###_____________________________________________________________________________

  # patient evaluated and care provided ----
  patient_eval_care_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ patient_care_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(
      dplyr::if_any(
        {{ patient_care_col }},
        ~ grepl(
          pattern = patient_treated,
          x = .,
          ignore.case = TRUE
        )
      )
    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

  # assign variables to final data ----
  computing_population <- final_data |>
    dplyr::left_join(audit_table, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::mutate(
      PATIENT_CARE = {{ erecord_01_col }} %in% patient_eval_care_data,
      VALIDITY_SCORE_FORMAT = paste0({{ validity_score_col }} * 100, "%"),
      VALIDITY_SCORE_RANGE = base::cut(
        {{ validity_score_col }},
        breaks = validity_band_breaks,
        labels = validity_labels_raw,
        right = FALSE,
        include.lowest = TRUE
      ),
      VALIDITY_SCORE_RANGE_FORMAT = base::cut(
        {{ validity_score_col }},
        breaks = validity_band_breaks,
        labels = validity_labels_pct,
        right = FALSE,
        include.lowest = TRUE
      )
    )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

  # get the initial population ----
  initial_population <- computing_population |>
    dplyr::filter(
      PATIENT_CARE
    )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

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
      set = 10,
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
      set = 10,
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
    set = 11,
    id = progress_bar_population,
    force = TRUE
  )

  # summarize counts for populations filtered ----
  filter_counts <- tibble::tibble(
    filter = c(
      "Patients evaluated and care provided",
      "Adults denominator",
      "Peds denominator",
      "Initial population",
      "Total dataset"
    ),
    count = c(
      sum(computing_population$PATIENT_CARE, na.rm = TRUE),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest ----

  cli::cli_progress_update(
    set = 12,
    id = progress_bar_population,
    force = TRUE
  )

  # gather data into a list for multi-use output ----
  data.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population,
    missingness = missings
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(data.01.population)
}
