#' @title Trauma-04 Calculations
#'
#' @description
#'
#' This function processes EMS data to generate a set of binary variables
#' indicating whether specific trauma triage criteria are met. The output is a
#' data frame enriched with these indicators for further analysis.  The final
#' outcome is whether or not the EMS record documents the use of a verified
#' trauma center levels 1-5 in the hospital capability documentation.
#'
#' @inheritParams airway_01_population
#' @inheritParams asthma_01_population
#' @inheritParams safety_02_population
#' @inheritParams safety_04_population
#' @inheritParams trauma_01_population
#' @inheritParams pediatrics_03b_population
#' @inheritParams airway_01
#'
#' @return A data.frame summarizing results for two population groups (All,
#'   Adults and Peds) with the following columns:
#' - `pop`: Population type (All, Adults, and Peds).
#' - `numerator`: Count of incidents meeting the measure.
#' - `denominator`: Total count of included incidents.
#' - `prop`: Proportion of incidents meeting the measure.
#' - `prop_label`: Proportion formatted as a percentage with a specified number
#'    of decimal places.
#' - `lower_ci`: Lower bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#' - `upper_ci`: Upper bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#'
#' @examples
#' # Synthetic test data
#'   test_data <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     eresponse_10 = rep(2210011, 5),
#'     esituation_02 = rep("Yes", 5),
#'     evitals_06 = c(100, 90, 80, 70, 85),
#'     evitals_10 = c(110, 89, 88, 71, 85),
#'     evitals_12 = c(50, 60, 70, 80, 75),
#'     evitals_14 = c(30, 9, 8, 7, 31),
#'     evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
#'     evitals_21 = c(5, 4, 3, 2, 1),
#'     eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
#'     eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
#'     eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
#'     eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023),
#'     edisposition_23 = c(9908029, 9908027, 9908025, 9908023, 9908021),
#'     edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007),
#'     eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003),
#'     einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
#'     einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
#'     einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
#'     einjury_09 = c(11, 12, 13, 14, 15)
#'   )
#'
#'   # Run function with the first and last pain score columns
#'   # Return 95% confidence intervals using the Wilson method
#'   trauma_04(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     incident_date_col = NULL,
#'     patient_DOB_col = NULL,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     eresponse_10_col = eresponse_10,
#'     esituation_02_col = esituation_02,
#'     evitals_06_col = evitals_06,
#'     evitals_10_col = evitals_10,
#'     evitals_12_col = evitals_12,
#'     evitals_14_col = evitals_14,
#'     evitals_15_col = evitals_15,
#'     evitals_21_col = evitals_21,
#'     eexam_16_col = eexam_16,
#'     eexam_20_col = eexam_20,
#'     eexam_23_col = eexam_23,
#'     eexam_25_col = eexam_25,
#'     edisposition_23_col = edisposition_23,
#'     transport_disposition_col = edisposition_30,
#'     eprocedures_03_col = eprocedures_03,
#'     einjury_01_col = einjury_01,
#'     einjury_03_col = einjury_03,
#'     einjury_04_col = einjury_04,
#'     einjury_09_col = einjury_09,
#'     confidence_interval = TRUE
#'   )
#'
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_04 <- function(
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
  edisposition_23_col,
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
  einjury_09_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
) {
  # Set default method and adjustment method ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # Ensure that not all table arguments AND the df argument are fulfilled ----
  # User must pass either `df` or all table arguments, but not both
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
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Trauma-04")

    # header ----
    cli::cli_h2("Gathering Records for Trauma-04")

    trauma_04_populations <- trauma_04_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      vitals_table = vitals_table,
      exam_table = exam_table,
      procedures_table = procedures_table,
      injury_table = injury_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_23_col = {{ edisposition_23_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Trauma-04")

    population_65 <- trauma_04_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = ">= 65 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # 10 to 64 population ----
    population_10_64 <- trauma_04_populations$population_10_64 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = "10-64 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # patients < 10 yrs ----
    population_10 <- trauma_04_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = "< 10 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # summary ----
    trauma.04 <- dplyr::bind_rows(
      population_65,
      population_10_64,
      population_10
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2) # Convert to minutes and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 'm'))}."
      )
    } else {
      run_time <- round(run_time_secs, 2) # Keep in seconds and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 's'))}."
      )
    }

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(trauma.04$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(trauma.04)
  } else if (
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
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Trauma-04")

    # header ----
    cli::cli_h2("Gathering Records for Trauma-04")

    trauma_04_populations <- trauma_04_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_23_col = {{ edisposition_23_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Trauma-04")

    population_65 <- trauma_04_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = ">= 65 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # 10 to 64 population ----
    population_10_64 <- trauma_04_populations$population_10_64 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = "10-64 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # patients < 10 yrs ----
    population_10 <- trauma_04_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-04",
        population_name = "< 10 yrs",
        numerator_col = HOSPITAL_CAPABILITY,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        ...
      )

    # summary ----
    trauma.04 <- dplyr::bind_rows(
      population_65,
      population_10_64,
      population_10
    )

    # create a separator ----
    cli::cli_text("\n")

    # Calculate and display the runtime ----
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2) # Convert to minutes and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 'm'))}."
      )
    } else {
      run_time <- round(run_time_secs, 2) # Keep in seconds and round
      cli::cli_alert_success(
        "Function completed in {cli::col_green(paste0(run_time, 's'))}."
      )
    }

    # create a separator ----
    cli::cli_text("\n")

    # when confidence interval is "wilson", check for n < 10 ----
    # to warn about incorrect Chi-squared approximation
    if (
      any(trauma.04$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(trauma.04)
  }
}
