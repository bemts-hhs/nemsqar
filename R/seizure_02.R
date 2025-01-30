#' @title Seizure-02 Calculation
#'
#' @description
#'
#' Calculates the NEMSQA Seizure-02 Measure.
#'
#' Calculates age-based seizure metrics for a dataset. This function filters
#' data for patients based on incident information, diagnoses, and administered
#' medications to assess adherence to Seizure-02 metrics.
#'
#' @param df A data frame where each row is an observation, containing all
#'   necessary columns for analysis.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the
#'   emedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name for patient age in numeric form.
#' @param epatient_16_col Column name for age unit (e.g., `"Years"` or
#'   `"Months"`).
#' @param eresponse_05_col Column name for response codes; "911" call codes are
#'   filtered.
#' @param esituation_11_col Column name for primary impressions.
#' @param esituation_12_col Column name for secondary impressions.
#' @param emedications_03_col Column name for medications administered; ideally
#'   a list column or string with comma-separated values.
#' @param ... Additional arguments passed to `dplyr::summarize`.
#'
#' @return A tibble summarizing results for three population groups (All,
#'   Adults, and Peds) with the following columns: `pop`: Population type (All,
#'   Adults, or Peds). `numerator`: Count of incidents where beta-agonist
#'   medications were administered. `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents involving beta-agonist medications.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
seizure_02 <- function(df = NULL,
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
                       ...) {

  # utilize applicable tables to analyze the data for the measure
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(medications_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Seizure-02")

    # header
    cli::cli_h2("Gathering Records for Seizure-02")

    # gather the population of interest
    seizure_02_populations <- seizure_02_population(patient_scene_table = patient_scene_table,
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
  cli::cli_h2("Calculating Seizure-02")

  # summarize
  seizure.02 <- results_summarize(total_population = seizure_02_populations$initial_population,
                                  adult_population = seizure_02_populations$adults,
                                  peds_population = seizure_02_populations$peds,
                                  measure_name = "Seizure-02",
                                  numerator_col = BENZO_MED,
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
    cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

  } else {

    run_time <- round(run_time_secs, 2)  # Keep in seconds and round
    cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

  }

  # create a separator
  cli::cli_text("\n")

  return(seizure.02)

  } else if (
    any(
      is.null(patient_scene_table),
      is.null(medications_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    !is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Seizure-02")

    # header
    cli::cli_h2("Gathering Records for Seizure-02")

    # gather the population of interest
    seizure_02_populations <- seizure_02_population(df = df,
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
  cli::cli_h2("Calculating Seizure-02")

  # summarize
  seizure.02 <- results_summarize(total_population = seizure_02_populations$initial_population,
                                  adult_population = seizure_02_populations$adults,
                                  peds_population = seizure_02_populations$peds,
                                  measure_name = "Seizure-02",
                                  numerator_col = BENZO_MED,
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
    cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

  } else {

    run_time <- round(run_time_secs, 2)  # Keep in seconds and round
    cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

  }

  # create a separator
  cli::cli_text("\n")

  return(seizure.02)


  }

}
