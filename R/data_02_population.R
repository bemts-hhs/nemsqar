#' @title Data-02 Population
#'
#' @description
#'
#' Creates the analysis populations for the Data-02 documentation timeliness
#' measure. The function prepares a computing population, overall denominator
#' population, adult population, and pediatric population for subsequent
#' documentation timeliness analyses.
#'
#' Data-02 evaluates the timeliness of EMS patient care documentation using
#' user-defined date-time variables that represent when documentation could
#' reasonably begin and when documentation occurred. Elapsed documentation
#' time is calculated in hours and days and categorized into predefined
#' reporting intervals.
#'
#' The function supports two data input workflows:
#'
#' - A single analytic dataset supplied through `df`.
#' - Separate patient, audit, disposition, and times tables supplied through
#'   `patient_scene_table`, `audit_table`, `disposition_table`, and
#'   `times_table`.
#'
#' Records are restricted to patients with documented patient evaluation,
#' refusal, death, or treatment dispositions indicating patient contact.
#'
#' When incident and date-of-birth variables are supplied, both reported and
#' calculated ages are utilized to derive adult and pediatric populations.
#'
#' @inheritParams airway_01_population
#' @inheritParams safety_02_population
#' @inheritParams data_01_population
#' @param times_table A data.frame or tibble containing event date-time
#'   variables used to evaluate documentation timeliness. These variables may
#'   include NEMSIS time elements (e.g., unit back in service date/time) or
#'   registry-, vendor-, or agency-specific timestamps related to record
#'   creation, modification, submission, approval, or other documentation
#'   workflow events. The contents of this table may not correspond entirely
#'   to elements defined within the National EMS Information System (NEMSIS)
#'   data standard.
#' @param time_available_to_document_col Date-time variable representing
#'   the point at which patient care documentation could reasonably begin.
#'   Examples may include unit back in service time, back at home location time,
#'   ems call completed time, and others.
#' @param time_documentation_occurred_col Date-time variable representing
#'   when patient care documentation was completed or first entered into the
#'   EMS software, registry, or other documentation system. This variable is
#'   typically vendor- or registry-specific and may not correspond to a NEMSIS
#'   data element. Examples include incident created time, incident completed
#'   time, and others.
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
#' - `RECORD_COMPLETION_HOURS`
#' - `RECORD_COMPLETION_DAYS`
#' - `RECORD_COMPLETION_DAYS_RANGE`
#' - `RECORD_COMPLETION_DAYS_LABEL`
#' - `DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE`
#'
#' @details
#'
#' Adult and pediatric populations are determined using NEMSIS patient age
#' fields. When both `incident_date_col` and `patient_DOB_col` are supplied,
#' calculated age is used in conjunction with reported age variables to improve
#' population classification.
#'
#' Documentation timeliness is calculated as the elapsed time between
#' `time_available_to_document_col` and
#' `time_documentation_occurred_col`. Elapsed time is reported in both hours
#' and days and categorized into predefined documentation timeliness bands.
#'
#' A chronology flag (`DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE`) is created
#' when documentation occurs before the documentation-available timestamp.
#' Such records are retained because they may reflect registry workflows,
#' CAD-generated records, automated record creation processes, or agency-
#' specific documentation practices.
#'
#' @examples
#'
#' # Synthetic Data-02 example
#' test_data <- tibble::tibble(
#'   erecord_01 = paste0("R", 1:10),
#'
#'   # Incident and demographic variables
#'   incident_date = as.Date("2024-01-01"),
#'   patient_dob = as.Date(c(
#'     "1980-01-01", "1975-01-01", "1990-01-01", "1985-01-01",
#'     "1970-01-01", "2015-01-01", "2012-01-01", "2010-01-01",
#'     "2018-01-01", "2016-01-01"
#'   )),
#'   epatient_15 = c(44, 49, 34, 39, 54, 9, 12, 14, 6, 8),
#'   epatient_16 = rep("Years", 10),
#'
#'   # Patient contact disposition
#'   patient_disposition = rep(
#'     "Patient Treated, Transported by this EMS Unit (4212033)",
#'     10
#'   ),
#'
#'   # Time documentation became available
#'   unit_back_in_service = as.POSIXct(
#'     "2024-01-01 08:00:00",
#'     tz = "UTC"
#'   ) + c(
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0,
#'     0
#'   ) * 86400,
#'
#'   # Time documentation occurred
#'   incident_created = as.POSIXct(
#'     c(
#'       "2024-01-01 11:00:00",  # 3 hours
#'       "2024-01-01 18:00:00",  # 10 hours
#'       "2024-01-02 04:00:00",  # 20 hours
#'       "2024-01-02 12:00:00",  # 28 hours
#'       "2024-01-03 12:00:00",  # 52 hours
#'       "2024-01-05 12:00:00",  # 4 days
#'       "2024-01-10 12:00:00",  # 9 days
#'       "2024-01-20 12:00:00",  # 19 days
#'       "2024-02-15 12:00:00",  # 45 days
#'       "2024-04-15 12:00:00"   # >60 days
#'     ),
#'     tz = "UTC"
#'   )
#' )
#'
#' # Create the Data-02 populations
#' data_02_population(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   patient_care_col = patient_disposition,
#'   time_available_to_document_col = unit_back_in_service,
#'   time_documentation_occurred_col = incident_created
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
data_02_population <- function(
  df = NULL,
  patient_scene_table = NULL,
  audit_table = NULL,
  disposition_table = NULL,
  times_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  patient_care_col,
  time_available_to_document_col,
  time_documentation_occurred_col
) {
  # Check for tables or DF ----

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table),
      !is.null(times_table)
    ) &&
      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_02_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # Ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(audit_table),
      is.null(disposition_table),
      is.null(times_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn data_02_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach."
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
      missing(time_available_to_document_col),
      missing(time_documentation_occurred_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn data_02_population}."
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
    "Running `data_02_population()`",
    total = 13,
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
      is.null(disposition_table),
      is.null(times_table)
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
        -{{ time_available_to_document_col }},
        -{{ time_documentation_occurred_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # audit ----
    audit_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ time_documentation_occurred_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # disposition ----
    disposition_table <- df |>
      dplyr::select({{ erecord_01_col }}, {{ patient_care_col }}) |>
      dplyr::distinct()

    # times ----
    times_table <- df |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ time_available_to_document_col }}
      ) |>
      dplyr::distinct()
  } else if (
    # else continue with the tables passed to the applicable arguments ----

    all(
      !is.null(patient_scene_table),
      !is.null(audit_table),
      !is.null(disposition_table),
      !is.null(times_table)
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

    validate_data_structure(
      input = times_table,
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
      dplyr::select(
        {{ erecord_01_col }},
        {{ time_documentation_occurred_col }}
      ) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE)

    # disposition ----
    disposition_table <- disposition_table |>
      dplyr::select({{ erecord_01_col }}, {{ patient_care_col }}) |>
      dplyr::distinct()

    # times ----
    times_table <- times_table |>
      dplyr::select(
        {{ erecord_01_col }},
        {{ time_available_to_document_col }}
      ) |>
      dplyr::distinct()
  }

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

  # Data validation ----
  ## validate that incident created time is a date-time
  ### pull and check data
  time_documentation_occurred_data <- validate_data_pull(
    input = audit_table,
    type = "error",
    col = {{ time_documentation_occurred_col }},
    var_name = "time_documentation_occurred_col",
    calls = 2
  )

  ### validate incident created
  validate_class(
    input = time_documentation_occurred_data,
    class_type = "date-time",
    na_ok = TRUE,
    null_ok = TRUE,
    finite = FALSE,
    type = "error",
    var_name = "time_documentation_occurred_col",
    calls = 2
  )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

  ## validate that time back in service is a date-time
  ### pull and check data
  time_available_to_document_data <- validate_data_pull(
    input = times_table,
    type = "error",
    col = {{ time_available_to_document_col }},
    var_name = "time_available_to_document_col",
    calls = 2
  )

  ### validate time back in service
  validate_class(
    input = time_available_to_document_data,
    class_type = "date-time",
    na_ok = TRUE,
    null_ok = TRUE,
    finite = FALSE,
    type = "error",
    var_name = "time_available_to_document_col",
    calls = 2
  )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

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
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

  ###___________________________________________________________________________
  # Estimate missingness in each table for included columns in the measure ----
  ###___________________________________________________________________________

  # utilize the internal `nemsqa_missing_summary` to estimate missingness
  missings <- nemsqa_missing_summary(
    patient_scene_table,
    audit_table,
    disposition_table,
    times_table
  )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

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
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

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
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

  # assign variables to final data ----
  computing_population <- final_data |>
    dplyr::left_join(audit_table, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::left_join(times_table, by = dplyr::join_by({{ erecord_01_col }})) |>
    dplyr::mutate(
      PATIENT_CARE = {{ erecord_01_col }} %in% patient_eval_care_data,

      # record completion time ----
      RECORD_COMPLETION_HOURS = as.numeric(
        difftime(
          time1 = {{ time_documentation_occurred_col }},
          time2 = {{ time_available_to_document_col }},
          units = "hours"
        )
      ),

      RECORD_COMPLETION_DAYS = as.numeric(
        difftime(
          time1 = {{ time_documentation_occurred_col }},
          time2 = {{ time_available_to_document_col }},
          units = "days"
        )
      ),

      # time bands ----
      RECORD_COMPLETION_DAYS_RANGE = base::cut(
        RECORD_COMPLETION_DAYS,
        breaks = time_band_breaks,
        labels = time_band_labels_raw,
        right = FALSE,
        include.lowest = TRUE
      ),

      RECORD_COMPLETION_DAYS_LABEL = base::cut(
        RECORD_COMPLETION_DAYS,
        breaks = time_band_breaks,
        labels = time_band_labels,
        right = FALSE,
        include.lowest = TRUE
      ),

      # DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE ----
      # Indicates that the documentation timestamp occurred before the
      # documentation-available timestamp. This condition may reflect
      # automated record creation workflows, computer-aided dispatch (CAD)
      # generated records, agency-specific documentation practices, or
      # potential data quality issues.

      DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE = {{
        time_documentation_occurred_col
      }} <
        {{ time_available_to_document_col }}
    )

  # progress update, these will be repeated throughout the script ----
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

  # get the initial population ----
  initial_population <- computing_population |>
    dplyr::filter(
      PATIENT_CARE
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

  # summarize counts for populations filtered ----
  filter_counts <- tibble::tibble(
    filter = c(
      "Patients evaluated and care provided",
      "Documentation occurred before available to document",
      "Adults denominator",
      "Peds denominator",
      "Initial population",
      "Total dataset"
    ),
    count = c(
      sum(computing_population$PATIENT_CARE, na.rm = TRUE),
      sum(
        computing_population$DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE,
        na.rm = TRUE
      ),
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
  data.02.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population,
    computing_population = computing_population,
    missingness = missings
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(data.02.population)
}
