#' @title Respiratory-01 Calculation
#'
#' @description
#'
#' The `respiratory_01` function filters and analyzes data related to emergency
#' 911 respiratory distress incidents, providing summary statistics for adult
#' and pediatric populations. This function uses specific data columns for 911
#' response codes, primary and secondary impressions, and vital signs to
#' calculate the proportion of cases with complete vital signs recorded,
#' stratified by age.
#'
#' @param df A data frame containing incident data with each row representing an
#'   observation.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing at least the
#'   esituation fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals
#'   fields needed for this measure's calculations.
#' @param erecord_01_col Unique Patient ID
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column name for 911 response codes (e.g., 2205001,
#'   2205003, 2205009).
#' @param esituation_11_col Column name for primary impression codes related to
#'   respiratory distress.
#' @param esituation_12_col Column name for secondary impression codes related
#'   to respiratory distress.
#' @param evitals_12_col Column name for the first vital sign measurement.
#' @param evitals_14_col Column name for the second vital sign measurement.
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
#' # Synthetic test data
#' test_data <- tibble::tibble(
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'   eresponse_05 = rep(2205001, 5),
#'   esituation_11 = c(rep("J80", 3), rep("I50.9", 2)),
#'   esituation_12 = c(rep("J80", 2), rep("I50.9", 3)),
#'   evitals_12 = c(60, 59, 58, 57, 56),
#'   evitals_14 = c(16, 15, 14, 13, 12)
#' )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' respiratory_01(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   eresponse_05_col = eresponse_05,
#'   esituation_11_col = esituation_11,
#'   esituation_12_col = esituation_12,
#'   evitals_12_col = evitals_12,
#'   evitals_14_col = evitals_14,
#'   confidence_interval = TRUE
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
respiratory_01 <- function(df = NULL,
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
                           evitals_12_col,
                           evitals_14_col,
                           confidence_interval = FALSE,
                           method = c("wilson", "clopper-pearson"),
                           conf.level = 0.95,
                           correct = TRUE,
                           ...) {

  # Set default method and adjustment method
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table)
    )

    && is.null(df)

  ) {

  # Start timing the function execution
  start_time <- Sys.time()

  # header
  cli::cli_h1("Respiratory-01")

  # header
  cli::cli_h2("Gathering Records for Respiratory-01")

  # gather the population of interest
  respiratory_01_populations <- respiratory_01_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           situation_table = situation_table,
                           vitals_table = vitals_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           esituation_11_col = {{ esituation_11_col }},
                           esituation_12_col = {{ esituation_12_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           evitals_14_col = {{ evitals_14_col }}
                           )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Respiratory-01")

    # summary
    respiratory.01 <- results_summarize(total_population = respiratory_01_populations$initial_population,
                                   adult_population = respiratory_01_populations$adults,
                                   peds_population = respiratory_01_populations$peds,
                                   population_names = c("all", "adults", "peds"),
                                   measure_name = "Respiratory-01",
                                   numerator_col = VITALS_CHECK,
                                   confidence_interval = confidence_interval,
                                   method = method,
                                   conf.level = conf.level,
                                   correct = correct,
                                   ...)

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
    if (any(respiratory.01$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(respiratory.01)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table)
    )

    && !is.null(df)

  )

  # utilize a dataframe to analyze the data for the measure analytics

  {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Respiratory-01")

    # header
    cli::cli_h2("Gathering Records for Respiratory-01")

    # gather the population of interest
    respiratory_01_populations <- respiratory_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col}},
      epatient_15_col = {{ epatient_15_col}},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      esituation_11_col = {{ esituation_11_col }},
      esituation_12_col = {{ esituation_12_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }}
    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Respiratory-01")

    # summary
    respiratory.01 <- results_summarize(total_population = respiratory_01_populations$initial_population,
                                        adult_population = respiratory_01_populations$adults,
                                        peds_population = respiratory_01_populations$peds,
                                        population_names = c("all", "adults", "peds"),
                                        measure_name = "Respiratory-01",
                                        numerator_col = VITALS_CHECK,
                                        confidence_interval = confidence_interval,
                                        method = method,
                                        conf.level = conf.level,
                                        correct = correct,
                                        ...)

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
    if (any(respiratory.01$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(respiratory.01)

  }

}
