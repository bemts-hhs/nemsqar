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
#' @param ... Additional parameters for the `dplyr::summarize` output.
#'
#' @return A tibble summarizing results for three population groups (All,
#'   Adults, and Peds) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (All, Adults, Peds).
#'   `numerator`: Count of incidents
#'   where patient weight was documented.
#'   `denominator`: Total count of
#'   incidents.
#'   `prop`: Proportion of incidents where patient weight was
#'   documented.
#'   `prop_label`: Proportion formatted as a percentage with a
#'   specified number of decimal places.
#'
#' @examples
#'
#' # Synthetic test data
#' #' test_data <- tibble::tibble(
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-06-01", "2025-12-15")),
#'   patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01", "2023-06-01", "2019-12-15")),
#'   epatient_15 = c(4, 5, 3, 2, 6),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
#'   eresponse_05 = rep(2205001, 5),
#'   emedications_03 = rep("stuff", 5),
#'   emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
#'   eexam_01 = c(60, 59, 58, 57, 56),
#'   eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
#' )
#'
#' # Run function
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
#'   eexam_02_col = eexam_02
#' )
#'
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
                           ...) {

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
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_name = "Peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
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
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_name = "Peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
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

  return(pediatrics.03b)

  }

}
