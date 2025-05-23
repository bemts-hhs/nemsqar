#' @title Pediatrics-03B Calculation
#'
#' @description
#'
#' The function calculates a pediatric metric focused on EMS responses,
#' specifically targeting responses that involve patients under 18 years of age,
#' where certain weight-based medications were administered. This function
#' filters EMS data to identify relevant 911 responses and further narrows down
#' the dataset to cases involving children, calculating the proportion of cases
#' with documented weight among those where weight-based medications were
#' administered.
#'
#' @param df A data frame or tibble containing emergency response records.
#'   Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing only ePatient
#'   and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing only the eResponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param exam_table A data.frame or tibble containing only the eExam fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the
#'   eMedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col Column for unique EMS record identifiers.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column containing the EMS response codes.
#' @param eexam_01_col Column containing documented weight information.
#' @param eexam_02_col Another column for weight documentation, if applicable.
#' @param emedications_03_col Column indicating medication administration.
#' @param emedications_04_col Column listing medications administered.
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
#' @return A data.frame summarizing results for two population groups (Peds)
#'   with the following columns:
#' - `pop`: Population type (Peds).
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
#'   incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'   "2025-06-01", "2025-12-15")),
#'   patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01",
#'   "2023-06-01", "2019-12-15")),
#'   epatient_15 = c(4, 5, 3, 2, 6),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
#'   eresponse_05 = rep(2205001, 5),
#'   emedications_03 = rep("stuff", 5),
#'   emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
#'   eexam_01 = c(60, 59, 58, 57, 56),
#'   eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
#' )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' pediatrics_03b(
#'   df = test_data,
#'   erecord_01_col = erecord_01,
#'   incident_date_col = incident_date,
#'   patient_DOB_col = patient_dob,
#'   epatient_15_col = epatient_15,
#'   epatient_16_col = epatient_16,
#'   eresponse_05_col = eresponse_05,
#'   emedications_03_col = emedications_03,
#'   emedications_04_col = emedications_04,
#'   eexam_01_col = eexam_01,
#'   eexam_02_col = eexam_02,
#'   confidence_interval = TRUE
#' )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
pediatrics_03b <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           exam_table = NULL,
                           medications_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
                           confidence_interval = FALSE,
                           method = c("wilson", "clopper-pearson"),
                           conf.level = 0.95,
                           correct = TRUE,
                           ...) {

  # Set default method and adjustment method
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

  if(all(
    !is.null(patient_scene_table),
    !is.null(response_table),
    !is.null(exam_table),
    !is.null(medications_table)
  )

  && is.null(df)

  ) {

  # Start timing the function execution
  start_time <- Sys.time()

  # header
  cli::cli_h1("Pediatrics-03b")

  # header
  cli::cli_h2("Gathering Records for Pediatrics-03b")

  # gather the population of interest
  pediatrics03b_populations <- pediatrics_03b_population(patient_scene_table = patient_scene_table,
                                                         response_table = response_table,
                                                         exam_table = exam_table,
                                                         medications_table = medications_table,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")

  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")

  # summary
  pediatrics.03b <- results_summarize(total_population = NULL,
                                      adult_population = NULL,
                                      peds_population = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_names = "peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
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
  if (any(pediatrics.03b$denominator < 10) && method == "wilson" && confidence_interval) {

    cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

  }

  return(pediatrics.03b)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(exam_table),
      is.null(medications_table)
    )

    && !is.null(df)

  )

  # utilize a dataframe to analyze the data for the measure analytics

  {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Pediatrics-03b")

    # header
    cli::cli_h2("Gathering Records for Pediatrics-03b")

  pediatrics03b_populations <- pediatrics_03b_population(df = df,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")

  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")

  # summary
  pediatrics.03b <- results_summarize(total_population = NULL,
                                      adult_population = NULL,
                                      peds_population = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_names = "peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
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
  if (any(pediatrics.03b$denominator < 10) && method == "wilson" && confidence_interval) {

    cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

  }

  return(pediatrics.03b)

  }

}
