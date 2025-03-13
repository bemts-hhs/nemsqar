#' @title Syncope-01 Calculation
#'
#' @description
#'
#' The `syncope_01` function processes EMS dataset to identify potential syncope
#' (fainting) cases based on specific criteria and calculates related ECG
#' measures. This function dplyr::filters data for 911 response calls, assesses
#' primary and associated symptoms for syncope, determines age-based populations
#' (adult and pediatric), and aggregates results by unique patient encounters.
#'
#' @param df Main data frame containing EMS records.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient age (numeric).
#' @param epatient_16_col Column for the patient age units (e.g., "Years",
#'   "Months").
#' @param eresponse_05_col Column containing response type codes, specifically
#'   911 codes.
#' @param esituation_09_col Column with primary symptoms associated with the
#'   patient encounter.
#' @param esituation_10_col Column with other associated symptoms.
#' @param esituation_11_col Column for primary impression code.
#' @param esituation_12_col Column for secondary impression codes.
#' @param evitals_04_col Column with ECG information if available.
#' @param confidence_interval `r lifecycle::badge("experimental")` Logical. If
#'   `TRUE`, the function calculates a confidence interval for the proportion
#'   estimate.
#' @param method `r lifecycle::badge("experimental")`Character. Specifies the
#'   method used to calculate confidence intervals. Options are `"wilson"`
#'   (Wilson score interval) and `"clopper-pearson"` (exact binomial interval).
#'   Partial matching is supported, so `"w"` and `"c"` can be used as shorthand.
#' @param conf.level `r lifecycle::badge("experimental")`Numeric. The confidence
#'   level for the interval, expressed as a proportion (e.g., 0.95 for a 95%
#'   confidence interval). Defaults to 0.95.
#' @param correct `r lifecycle::badge("experimental")`Logical. If `TRUE`,
#'   applies a continuity correction to the Wilson score interval when `method =
#'   "wilson"`. Defaults to `TRUE`.
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
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     esituation_09 = c(rep("R55", 3), rep("R40.4", 2)),
#'     esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
#'     esituation_11 = c(rep("R55", 3), rep("R40.4", 2)),
#'     esituation_12 = c(rep("R40.4", 2), rep("R55", 3)),
#'     evitals_04 = rep("15 Lead", 5)
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#'   syncope_01(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     esituation_09_col = esituation_09,
#'     esituation_10_col = esituation_10,
#'     esituation_11_col = esituation_11,
#'     esituation_12_col = esituation_12,
#'     evitals_04_col = evitals_04,
#'     confidence_interval = TRUE
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
syncope_01 <- function(df = NULL,
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
                       esituation_09_col,
                       esituation_10_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_04_col,
                       confidence_interval = FALSE,
                       method = c("wilson", "clopper-pearson"),
                       conf.level = 0.95,
                       correct = TRUE,
                       ...) {

  # Set default method and adjustment method
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Syncope-01")

    # header
    cli::cli_h2("Gathering Records for Syncope-01")

    # gather the population of interest
    syncope_01_populations <- syncope_01_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    vitals_table = vitals_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_09_col = {{ esituation_09_col }},
                                                    esituation_10_col = {{ esituation_10_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_04_col = {{ evitals_04_col }}
                                                    )

  # create a separator
  cli::cli_text("\n")

  # header for calculations
  cli::cli_h2("Calculating Syncope-01")

  # summarize
  syncope.01 <- results_summarize(
    total_population = NULL,
    adult_population = syncope_01_populations$adults,
    peds_population = syncope_01_populations$peds,
    measure_name = "Syncope-01",
    population_names = c("adults", "peds"),
    numerator_col = ECG_PERFORMED,
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

  # when confidence interval is "wilson", check for n < 10
  # to warn about incorrect Chi-squared approximation
  if (any(syncope.01$denominator < 10) && method == "wilson" && confidence_interval) {

    cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

  }

  return(syncope.01)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&

    !is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Syncope-01")

    # header
    cli::cli_h2("Gathering Records for Syncope-01")

    # gather the population of interest
    syncope_01_populations <- syncope_01_population(df = df,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_09_col = {{ esituation_09_col }},
                                                    esituation_10_col = {{ esituation_10_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_04_col = {{ evitals_04_col }}
                                                    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Syncope-01")

    # summarize
    syncope.01 <- results_summarize(
      total_population = NULL,
      adult_population = syncope_01_populations$adults,
      peds_population = syncope_01_populations$peds,
      measure_name = "Syncope-01",
      population_names = c("adults", "peds"),
      numerator_col = ECG_PERFORMED,
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

    # when confidence interval is "wilson", check for n < 10
    # to warn about incorrect Chi-squared approximation
    if (any(syncope.01$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(syncope.01)

  }

}
