#' @title Trauma-03 Calculation
#'
#' @description
#'
#' This function calculates the "Trauma-03" measure, which evaluates pain scale
#' reassessment for trauma patients, using a comprehensive data frame with EMS
#' records. The function processes input data to create both fact and dimension
#' tables, identifies eligible patients, and summarizes results for adult and
#' pediatric populations.
#'
#' @param df A data frame or tibble containing EMS data with all relevant
#'   columns. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col The column for patient age numeric value.
#' @param epatient_16_col The column for patient age unit (e.g., "Years",
#'   "Months").
#' @param esituation_02_col The column containing information on the presence of
#'   injury.
#' @param eresponse_05_col The column representing the 911 response type.
#' @param edisposition_28_col The column for patient care disposition details.
#' @param transport_disposition_col The column for patient transport
#'   disposition.
#' @param evitals_01_col The column for the time of pain scale measurement.
#' @param evitals_27_col The column for the full set of pain scale scores.
#' @param evitals_27_initial_col The column for the initial pain scale score.
#' @param evitals_27_last_col The column for the last pain scale score.
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
#' # for testing a single pain scale column
#'   test_data2 <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     esituation_02 = rep("Yes", 5),
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05
#'     18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01
#'     14:00:00")),
#'     edisposition_28 = rep(4228001, 5),
#'     edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
#'   )
#'
#'   # Expand data so each erecord_01 has 2 rows (one for each pain score)
#'   test_data_expanded2 <- test_data2 |>
#'     tidyr::uncount(weights = 2) |>  # Duplicate each row twice
#'     # Assign pain scores
#'     dplyr::mutate(evitals_27 = c(0, 0, 2, 1, 4, 3, 6, 5, 8, 7)) |>
#'     dplyr::group_by(erecord_01) |>
#'     dplyr::mutate(
#'     # Lower score = later time
#'       time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),
#'       evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
#'     ) |>
#'     dplyr::ungroup() |>
#'     dplyr::select(-time_offset)  # Remove temporary column
#'
#' # Run function with the single pain score column
#' # Return 95% confidence intervals using the Wilson method
#'   trauma_03(
#'     df = test_data_expanded2,
#'     erecord_01_col = erecord_01,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     esituation_02_col = esituation_02,
#'     evitals_01_col = evitals_01,
#'     evitals_27_initial_col = NULL,
#'     evitals_27_last_col = NULL,
#'     evitals_27_col = evitals_27,
#'     edisposition_28_col = edisposition_28,
#'     transport_disposition_col = edisposition_30,
#'     confidence_interval = TRUE
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_03 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      disposition_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      edisposition_28_col,
                      transport_disposition_col,
                      evitals_01_col,
                      evitals_27_col = NULL,
                      evitals_27_initial_col = NULL,
                      evitals_27_last_col = NULL,
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
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&

    is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("Trauma-03")

    # Header
    cli::cli_h2("Gathering Records for Trauma-03")

    # Gather the population of interest
    trauma_03_populations <- trauma_03_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      vitals_table = vitals_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_27_col = {{ evitals_27_col }},
      evitals_27_initial_col = {{ evitals_27_initial_col }},
      evitals_27_last_col = {{ evitals_27_last_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating Trauma-03")

  # summarize
  trauma.03 <- results_summarize(total_population = trauma_03_populations$initial_population,
                                 adult_population = trauma_03_populations$adults,
                                 peds_population = trauma_03_populations$peds,
                                 population_names = c("all", "adults", "peds"),
                                 measure_name = "Trauma-03",
                                 numerator_col = PAIN_SCALE,
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
    if (any(trauma.03$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(trauma.03)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&
    !is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # Header
    cli::cli_h1("Trauma-03")

    # Header
    cli::cli_h2("Gathering Records for Trauma-03")

    # Gather the population of interest
    trauma_03_populations <- trauma_03_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      evitals_01_col = {{ evitals_01_col }},
      evitals_27_col = {{ evitals_27_col }},
      evitals_27_initial_col = {{ evitals_27_initial_col }},
      evitals_27_last_col = {{ evitals_27_last_col }},
      edisposition_28_col = {{ edisposition_28_col }},
      transport_disposition_col = {{ transport_disposition_col }}
    )

    # Create a separator
    cli::cli_text("\n")

    # Header for calculations
    cli::cli_h2("Calculating Trauma-03")

    # summarize
    trauma.03 <- results_summarize(total_population = trauma_03_populations$initial_population,
                                 adult_population = trauma_03_populations$adults,
                                 peds_population = trauma_03_populations$peds,
                                 population_names = c("all", "adults", "peds"),
                                 measure_name = "Trauma-03",
                                 numerator_col = PAIN_SCALE,
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
    if (any(trauma.03$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(trauma.03)

  }

}


