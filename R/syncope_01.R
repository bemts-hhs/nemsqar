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
#' @param patient_dob_col Column that contains the patient's date of birth. This
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
#' @param ... Additional arguments passed to `dplyr::summarize` for grouped
#'   summaries.
#'
#' @return A tibble summarizing results for three population groups (Adults, and
#'   Peds) with the following columns:
#'
#'   `pop`: Population type (Adults, Peds). `numerator`: Count of incidents
#'   where beta-agonist medications were administered. `denominator`: Total
#'   count of incidents. `prop`: Proportion of incidents involving beta-agonist
#'   medications. `prop_label`: Proportion formatted as a percentage with a
#'   specified number of decimal places.
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
                       patient_dob_col = NULL,
                       epatient_15_col,
                       epatient_16_col,
                       eresponse_05_col,
                       esituation_09_col,
                       esituation_10_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_04_col,
                       ...) {

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
                                                    patient_dob_col = {{ patient_dob_col }},
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

  # adults
  adult_population <- syncope_01_populations$adults |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Adult",
                      ECG_PERFORMED,
                      ...)

  # peds
  peds_population <- syncope_01_populations$peds |>
    summarize_measure(measure_name = "Syncope-01",
                      population_name = "Peds",
                      ECG_PERFORMED,
                      ...)

  # summary
  syncope.01 <- dplyr::bind_rows(adult_population, peds_population)

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
                                                    patient_dob_col = {{ patient_dob_col }},
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

    # adults
    adult_population <- syncope_01_populations$adults |>
      summarize_measure(measure_name = "Syncope-01",
                        population_name = "Adult",
                        ECG_PERFORMED,
                        ...)

    # peds
    peds_population <- syncope_01_populations$peds |>
      summarize_measure(measure_name = "Syncope-01",
                        population_name = "Peds",
                        ECG_PERFORMED,
                        ...)

    # summary
    syncope.01 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(syncope.01)

  }

}
