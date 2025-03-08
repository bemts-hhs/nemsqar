#' @title TBI-01 Calculation
#'
#' @description
#'
#' This function screens for potential traumatic brain injury (TBI) cases based
#' on specific criteria in a patient dataset. It produces a subset of the data
#' with calculated variables for TBI identification.
#'
#' @param df A data frame or tibble containing the patient data.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data.frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name in df with the patient’s unique record ID.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name in df with the patient’s age value.
#' @param epatient_16_col Column name in df with the patient’s age unit (e.g.,
#'   years, months).
#' @param eresponse_05_col Column name in df with response codes for the type of
#'   EMS call.
#' @param esituation_11_col Column name in df with the primary provider
#'   impression.
#' @param esituation_12_col Column name in df with the secondary provider
#'   impression.
#' @param transport_disposition_col Column name in df with the transport
#'   disposition.
#' @param evitals_06_col Column name in df with systolic blood pressure (SBP).
#' @param evitals_12_col Column name in df with pulse oximetry values.
#' @param evitals_16_col Column name in df with ETCO2 values.
#'   values.
#' @param evitals_23_col Column name in df with Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col Column name in df with AVPU (alert, verbal, painful,
#'   unresponsive) values.
#' @param ... Additional parameters passed to `dplyr::summarize` or other dplyr
#'   functions.
#'
#' @return A tibble summarizing results for three population groups (Adults, and
#'   Peds) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (Adults, Peds).
#'   `numerator`: Count of incidents where SP02, ETCO2, and SBP were all
#'   measured.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents where SP02, ETCO2, and SBP were all
#'   measured.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
#'
#' @examples
#'
#' # Synthetic test data
#'   test_data <- tibble::tibble(
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     epatient_15 = c(34, 5, 45, 2, 60),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
#'     eresponse_05 = rep(2205001, 5),
#'     esituation_11 = c(rep("S02", 3), rep("S06", 2)),
#'     esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
#'     evitals_06 = c(85, 80, 100, 90, 82),
#'     evitals_12 = c(95, 96, 97, 98, 99),
#'     evitals_16 = c(35, 36, 37, 38, 39),
#'     evitals_23 = rep(8, 5),
#'     evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful"),
#'     edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
#'   )
#'
#'   # Run function
#'   tbi_01(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     epatient_15_col = epatient_15,
#'     epatient_16_col = epatient_16,
#'     eresponse_05_col = eresponse_05,
#'     esituation_11_col = esituation_11,
#'     esituation_12_col = esituation_12,
#'     evitals_06_col = evitals_06,
#'     evitals_12_col = evitals_12,
#'     evitals_16_col = evitals_16,
#'     evitals_23_col = evitals_23,
#'     evitals_26_col = evitals_26,
#'     transport_disposition_col = edisposition_30
#'   )
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
tbi_01 <- function(df = NULL,
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
                   eresponse_05_col,
                   esituation_11_col,
                   esituation_12_col,
                   transport_disposition_col,
                   evitals_06_col,
                   evitals_12_col,
                   evitals_16_col,
                   evitals_23_col,
                   evitals_26_col,
                   ...
                   ) {

  if (
    all(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(disposition_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("TBI-01")

    # header
    cli::cli_h2("Gathering Records for TBI-01")

    # gather the population of interest
    tbi_01_populations <- tbi_01_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    disposition_table = disposition_table,
                                                    vitals_table = vitals_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    incident_date_col = {{ incident_date_col }},
                                                    patient_DOB_col = {{ patient_DOB_col }},
                                                    epatient_15_col = {{ epatient_15_col }},
                                                    epatient_16_col = {{ epatient_16_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    transport_disposition_col = {{ transport_disposition_col }},
                                                    evitals_06_col = {{ evitals_06_col }},
                                                    evitals_12_col = {{ evitals_12_col }},
                                                    evitals_16_col = {{ evitals_16_col }},
                                                    evitals_23_col = {{ evitals_23_col }},
                                                    evitals_26_col = {{ evitals_26_col }}
                                            )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating TBI-01")

    # summarize

    # adults
    adult_population <- tbi_01_populations$adults |>
      summarize_measure(measure_name = "TBI-01",
                        population_name = "Adults",
                        VITALS_CHECK,
                        ...)

    # peds
    peds_population <- tbi_01_populations$peds |>
      summarize_measure(measure_name = "TBI-01",
                        population_name = "Peds",
                        VITALS_CHECK,
                        ...)

    # summary
    tbi.01 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(tbi.01)

  } else if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(disposition_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    !is.null(df)
  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("TBI-01")

    # header
    cli::cli_h2("Gathering Records for TBI-01")

    # gather the population of interest
    tbi_01_populations <- tbi_01_population(df = df,
                                            erecord_01_col = {{ erecord_01_col }},
                                            incident_date_col = {{ incident_date_col }},
                                            patient_DOB_col = {{ patient_DOB_col }},
                                            epatient_15_col = {{ epatient_15_col }},
                                            epatient_16_col = {{ epatient_16_col }},
                                            eresponse_05_col = {{ eresponse_05_col }},
                                            esituation_11_col = {{ esituation_11_col }},
                                            esituation_12_col = {{ esituation_12_col }},
                                            transport_disposition_col = {{ transport_disposition_col }},
                                            evitals_06_col = {{ evitals_06_col }},
                                            evitals_12_col = {{ evitals_12_col }},
                                            evitals_16_col = {{ evitals_16_col }},
                                            evitals_23_col = {{ evitals_23_col }},
                                            evitals_26_col = {{ evitals_26_col }}
                                            )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating TBI-01")

    # summarize

    # adults
    adult_population <- tbi_01_populations$adults |>
      summarize_measure(measure_name = "TBI-01",
                        population_name = "Adults",
                        VITALS_CHECK,
                        ...)

    # peds
    peds_population <- tbi_01_populations$peds |>
      summarize_measure(measure_name = "TBI-01",
                        population_name = "Peds",
                        VITALS_CHECK,
                        ...)

    # summary
    tbi.01 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(tbi.01)

  }

}
