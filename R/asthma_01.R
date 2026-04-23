#' @title Asthma-01 Calculation
#'
#' @description
#'
#' Calculates the NEMSQA Asthma-01 measure.
#'
#' Calculates key statistics related to asthma-related incidents in an EMS
#' dataset, specifically focusing on cases where 911 was called for respiratory
#' distress, and certain medications were administered. This function segments
#' the data by age into adult and pediatric populations, computing the
#' proportion of cases that received beta-agonist treatment.
#'
#' @inheritParams airway_01_population
#' @inheritParams asthma_01_population
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
#'
#' # Synthetic test data
#' test_data <- tibble::tibble(
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'   eresponse_05 = rep(2205001, 5),
#'   esituation_11 = c("Respiratory Distress", "Respiratory Distress",
#'   "Chest Pain", "Respiratory Distress", "Respiratory Distress"),
#'   esituation_12 = c("Asthma", "Asthma", "Other condition", "Asthma", "Asthma"),
#'   emedications_03 = c("Albuterol", "Albuterol", "Epinephrine", "None",
#'   "Albuterol")
#' )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' asthma_01(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   eresponse_05_col = eresponse_05,
#'   esituation_11_col = esituation_11,
#'   esituation_12_col = esituation_12,
#'   emedications_03_col = emedications_03,
#'   confidence_interval = TRUE
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
asthma_01 <- function(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  medications_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  emedications_03_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
) {
  # Set default method and adjustment method ----
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # ensure that not all table arguments AND the df argument are fulfilled ----
  # user only passes df or all table arguments

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(medications_table)
    ) &&

      !is.null(df)
  ) {
    cli::cli_abort(
      "{.fn asthma_01} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
    )
  }

  # ensure that df or all table arguments are fulfilled ----

  if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(medications_table)
    ) &&
      is.null(df)
  ) {
    cli::cli_abort(
      "{.fn asthma_01} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments."
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
      missing(emedications_03_col)
    )
  ) {
    cli::cli_abort(
      "One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn asthma_01}."
    )
  }

  # utilize applicable tables to analyze the data for the measure ----
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(medications_table)
    ) &&
      is.null(df)
  ) {
    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Asthma-01")

    # header ----
    cli::cli_h2("Gathering Records for Asthma-01")

    # gather the population of interest ----
    asthma_01_populations <- asthma_01_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      medications_table = medications_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      esituation_11_col = {{ esituation_11_col }},
      esituation_12_col = {{ esituation_12_col }},
      emedications_03_col = {{ emedications_03_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Asthma-01")

    # summary ----
    asthma.01 <- results_summarize(
      total_population = asthma_01_populations$initial_population,
      adult_population = asthma_01_populations$adults,
      peds_population = asthma_01_populations$peds,
      measure_name = "Asthma-01",
      numerator_col = beta_agonist_check,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
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
      any(asthma.01$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(asthma.01)
  } else if (
    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(medications_table)
    ) &&
      !is.null(df)
  ) {
    # utilize a dataframe to analyze the data for the measure analytics ----

    # Start timing the function execution ----
    start_time <- Sys.time()

    # header ----
    cli::cli_h1("Asthma-01")

    # header ----
    cli::cli_h2("Gathering Records for Asthma-01")

    # gather the population of interest ----
    asthma_01_populations <- asthma_01_population(
      df = df,
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      medications_table = medications_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      esituation_11_col = {{ esituation_11_col }},
      esituation_12_col = {{ esituation_12_col }},
      emedications_03_col = {{ emedications_03_col }}
    )

    # create a separator ----
    cli::cli_text("\n")

    # header for calculations ----
    cli::cli_h2("Calculating Asthma-01")

    # summary ----
    asthma.01 <- results_summarize(
      total_population = asthma_01_populations$initial_population,
      adult_population = asthma_01_populations$adults,
      peds_population = asthma_01_populations$peds,
      measure_name = "Asthma-01",
      numerator_col = beta_agonist_check,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
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
      any(asthma.01$denominator < 10) &&
        method == "wilson" &&
        confidence_interval
    ) {
      cli::cli_warn(
        "In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10."
      )
    }

    return(asthma.01)
  }
}
