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
#' @param df A data.frame or tibble containing EMS data. Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   ePatient and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the
#'   eResponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing at least the
#'   eSituation fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param medications_table A data.frame or tibble containing at least the
#'   eMedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier. Default is `NULL`.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient's numeric age agnostic
#'   of unit.
#' @param epatient_16_col Column representing the patient's age unit ("Years",
#'   "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col Column that contains eResponse.05.
#' @param esituation_11_col Column that contains eSituation.11.
#' @param esituation_12_col Column that contains all eSituation.12 values as a
#'   single comma-separated list.
#' @param emedications_03_col Column that contains all eMedications.03 values as
#'   a single comma-separated list.
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
asthma_01 <- function(df = NULL,
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
                      ...) {

  # Set default method and adjustment method
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table),
        !is.null(response_table),
        !is.null(situation_table),
        !is.null(medications_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Asthma-01")

    # header
    cli::cli_h2("Gathering Records for Asthma-01")

    # gather the population of interest
    asthma_01_populations <- asthma_01_population(patient_scene_table = patient_scene_table,
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

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Asthma-01")

    # summary
    asthma.01 <- results_summarize(total_population = asthma_01_populations$initial_population,
                                   adult_population = asthma_01_populations$adults,
                                   peds_population = asthma_01_populations$peds,
                                   measure_name = "Asthma-01",
                                   numerator_col = beta_agonist_check,
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
    if (any(asthma.01$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }


    return(asthma.01)

  } else if(all(is.null(patient_scene_table), is.null(response_table), is.null(situation_table), is.null(medications_table)) && !is.null(df))

    # utilize a dataframe to analyze the data for the measure analytics

  {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Asthma-01")

    # header
    cli::cli_h2("Gathering Records for Asthma-01")

    # gather the population of interest
    asthma_01_populations <- asthma_01_population(df = df,
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

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Asthma-01")

    # summary
    asthma.01 <- results_summarize(total_population = asthma_01_populations$initial_population,
                                   adult_population = asthma_01_populations$adults,
                                   peds_population = asthma_01_populations$peds,
                                   measure_name = "Asthma-01",
                                   numerator_col = beta_agonist_check,
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
    if (any(asthma.01$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(asthma.01)

  }

}
