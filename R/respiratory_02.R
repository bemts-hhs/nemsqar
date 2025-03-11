#' @title Respiratory-02 Calculation
#'
#' @description
#'
#' The `respiratory_02` function calculates metrics for pediatric and adult
#' respiratory populations based on pre-defined criteria, such as low oxygen
#' saturation and specific medication or procedure codes. It returns a summary
#' table of the overall, pediatric, and adult populations, showing counts and
#' proportions.
#'
#' @param df A data frame containing incident data with each row representing an
#'   observation.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals
#'   fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the
#'   emedications fields needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing only the
#'   eprocedures fields needed for this measure's calculations.
#' @param erecord_01_col Column name for eRecord.01, used to form a unique
#'   patient ID.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col integer Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column name for response codes (e.g., incident type).
#' @param evitals_12_col Column name for oxygen saturation (SpO2) values.
#' @param emedications_03_col Column name for medication codes.
#' @param eprocedures_03_col Column name for procedure codes.
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
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     emedications_03 = c("Oxygen", "Oxygen", "Oxygen", "Oxygen", "Oxygen"),
#'     evitals_12 = c(60, 59, 58, 57, 56),
#'     eprocedures_03 = rep("applicable thing", 5)
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#'   respiratory_02(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     emedications_03_col = emedications_03,
#'     evitals_12_col = evitals_12,
#'     eprocedures_03_col = eprocedures_03,
#'     confidence_interval = TRUE
#'   )
#'
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
respiratory_02 <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           vitals_table = NULL,
                           medications_table = NULL,
                           procedures_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col,
                           confidence_interval = FALSE,
                           method = c("wilson", "clopper-pearson"),
                           conf.level = 0.95,
                           correct = TRUE,
                           ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(vitals_table),
      !is.null(medications_table),
      !is.null(procedures_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Respiratory-02")

    # header
    cli::cli_h2("Gathering Records for Respiratory-02")

  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           vitals_table = vitals_table,
                           procedures_table = procedures_table,
                           medications_table = medications_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )

  # create a separator
  cli::cli_text("\n")

  # header for calculations
  cli::cli_h2("Calculating Respiratory-02")

  # summary
  respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                      adult_population = respiratory_02_populations$adults,
                                      peds_population = respiratory_02_populations$peds,
                                      population_names = c("all", "adults", "peds"),
                                      measure_name = "Respiratory-02",
                                      numerator_col = OXYGEN,
                                      confidence_interval,
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

  return(respiratory.02)


    } else if(

        all(
          is.null(patient_scene_table),
          is.null(response_table),
          is.null(vitals_table),
          is.null(medications_table),
          is.null(procedures_table)
        ) && !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics

    ) {

  # Start timing the function execution
  start_time <- Sys.time()

  # header
  cli::cli_h1("Respiratory-02")

  # header
  cli::cli_h2("Gathering Records for Respiratory-02")

  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           df = df,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Respiratory-02")

    # summary
    respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                        adult_population = respiratory_02_populations$adults,
                                        peds_population = respiratory_02_populations$peds,
                                        population_names = c("all", "adults", "peds"),
                                        measure_name = "Respiratory-02",
                                        numerator_col = OXYGEN,
                                        confidence_interval,
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

    return(respiratory.02)

    }

}
