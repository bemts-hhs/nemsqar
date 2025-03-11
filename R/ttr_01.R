#' @title TTR-01 Calculation
#'
#' @description
#'
#' This function calculates the TTR-01 measure, which evaluates the completeness
#' of vitals documentation for patients not experiencing cardiac arrest who were
#' also not transported during a 911 response. It determines the total
#' population, adult population, and pediatric population meeting the criteria
#' for the TTR_01 measure.
#'
#' @param df A data frame or tibble containing the dataset to analyze. Default
#'   is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param arrest_table A data frame or tibble containing only the earrest fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col A column specifying unique patient records.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col A column indicating the patient’s age in numeric form.
#' @param epatient_16_col A column specifying the unit of patient age (e.g.,
#'   "Years", "Days").
#' @param eresponse_05_col A column specifying the type of response (e.g., 911
#'   codes).
#' @param transport_disposition_col A column specifying transport disposition
#'   for the patient.
#' @param earrest_01_col A column containing cardiac arrest data.
#' @param evitals_06_col A column containing systolic blood pressure (SBP) data
#'   from initial vital signs.
#' @param evitals_07_col A column containing diastolic blood pressure (DBP) data
#'   from initial vital signs.
#' @param evitals_10_col A column containing heart rate data from initial vital
#'   signs.
#' @param evitals_12_col A column containing spO2 data from the initial vital
#'   signs.
#' @param evitals_14_col A column containing respiratory rate data from initial
#'   vital signs.
#' @param evitals_23_col A column containing total Glasgow Coma Scale (GCS)
#'   scores from initial vital signs.
#' @param evitals_26_col A column containing alert, verbal, painful,
#'   unresponsive (AVPU) vital signs.
#' @param confidence_interval Logical. If `TRUE`, the function calculates a
#'   confidence interval for the proportion estimate.
#' @param method Character. Specifies the method used to calculate confidence
#'   intervals. Options are `"wilson"` (Wilson score interval) and
#'   `"clopper-pearson"` (exact binomial interval). Partial matching is
#'   supported, so `"w"` and `"c"` can be used as shorthand.
#' @param conf.level Numeric. The confidence level for the interval, expressed
#'   as a proportion (e.g., 0.95 for a 95% confidence interval). Defaults to
#'   0.95.
#' @param correct Logical. If `TRUE`, applies a continuity correction to the
#'   Wilson score interval when `method = "wilson"`. Defaults to `TRUE`.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
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
#'   test_data <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'     "2025-01-01", "2025-06-01")),
#'     patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'     "2023-01-01", "1970-06-01")),
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     earrest_01 = rep("No", 5),
#'     evitals_06 = c(100, 90, 80, 70, 85),
#'     evitals_07 = c(80, 90, 50, 60, 87),
#'     evitals_10 = c(110, 89, 88, 71, 85),
#'     evitals_12 = c(50, 60, 70, 80, 75),
#'     evitals_14 = c(30, 9, 8, 7, 31),
#'     evitals_23 = c(6, 7, 8, 9, 10),
#'     evitals_26 = c(3326007, 3326005, 3326003, 3326001, 3326007),
#'     edisposition_30 = c(4230013, 4230009, 4230013, 4230009, 4230013)
#'   )
#'
#'   # Run function with the first and last pain score columns
#'   # Return 95% confidence intervals using the Wilson method
#'   ttr_01(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     incident_date_col = incident_date,
#'     patient_DOB_col = patient_dob,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     earrest_01_col = earrest_01,
#'     evitals_06_col = evitals_06,
#'     evitals_07_col = evitals_07,
#'     evitals_10_col = evitals_10,
#'     evitals_12_col = evitals_12,
#'     evitals_14_col = evitals_14,
#'     evitals_23_col = evitals_23,
#'     evitals_26_col = evitals_26,
#'     transport_disposition_col = edisposition_30
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
ttr_01 <- function(df = NULL,
                   patient_scene_table = NULL,
                   response_table = NULL,
                   disposition_table = NULL,
                   vitals_table = NULL,
                   arrest_table = NULL,
                   erecord_01_col,
                   incident_date_col = NULL,
                   patient_DOB_col = NULL,
                   epatient_15_col,
                   epatient_16_col,
                   eresponse_05_col,
                   transport_disposition_col,
                   earrest_01_col,
                   evitals_06_col,
                   evitals_07_col,
                   evitals_10_col,
                   evitals_12_col,
                   evitals_14_col,
                   evitals_23_col,
                   evitals_26_col,
                   confidence_interval = FALSE,
                   method = c("wilson", "clopper-pearson"),
                   conf.level = 0.95,
                   correct = TRUE,
                   ...
                   ) {

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(arrest_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("TTR-01")

    # Header
    cli::cli_h2("Gathering Records for TTR-01")

    # Gather the population of interest
    ttr_01_populations <- ttr_01_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      disposition_table = disposition_table,
      vitals_table = vitals_table,
      arrest_table = arrest_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      earrest_01_col = {{ earrest_01_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_07_col = {{ evitals_07_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating TTR-01")

    # summarize
    ttr.01 <- results_summarize(
      total_population = NULL,
      adult_population = ttr_01_populations$adults,
      peds_population = ttr_01_populations$peds,
      population_names = c("adults", "peds"),
      measure_name = "TTR-01",
      numerator_col = VITALS,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {
      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(ttr.01)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(arrest_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&

    !is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("TTR-01")

    # Header
    cli::cli_h2("Gathering Records for TTR-01")

    # Gather the population of interest
    ttr_01_populations <- ttr_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      earrest_01_col = {{ earrest_01_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_07_col = {{ evitals_07_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating TTR-01")

    # summarize
    ttr.01 <- results_summarize(
      total_population = NULL,
      adult_population = ttr_01_populations$adults,
      peds_population = ttr_01_populations$peds,
      population_names = c("adults", "peds"),
      measure_name = "TTR-01",
      numerator_col = VITALS,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
      ...
    )

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {
      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {
      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(ttr.01)

  }

}

