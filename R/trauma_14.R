#' @title Trauma-14 Calculation
#'
#' @description
#'
#' This function processes EMS data to generate a set of binary variables
#' indicating whether specific trauma triage criteria are met. The output #' is
#' a data frame enriched with these indicators for further analysis.  The final
#' outcome is whether or not the EMS record documents the use of #' a
#' pre-hospital trauma activation.
#'
#' @param df A data frame or tibble containing EMS data with all relevant
#'   columns.
#' @param patient_scene_table A data frame or tibble containing fields from
#'   epatient and escene needed for this measure's calculations.
#' @param situation_table A data frame or tibble containing fields from
#'   esituation needed for this measure's calculations.
#' @param response_table A data frame or tibble containing fields from eresponse
#'   needed for this measure's calculations.
#' @param disposition_table A data frame or tibble containing fields from
#'   edisposition needed for this measure's calculations.
#' @param vitals_table A data frame or tibble containing fields from evitals
#'   needed for this measure's calculations.
#' @param exam_table A data frame or tibble containing fields from eexam needed
#'   for this measure's calculations.
#' @param procedures_table A data frame or tibble containing fields from
#'   eprocedures needed for this measure's calculations.
#' @param injury_table A data frame or tibble containing fields from einjury
#'   needed for this measure's calculations.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col The column for patient age numeric value.
#' @param epatient_16_col The column for patient age unit (e.g., "Years",
#'   "Months").
#' @param esituation_02_col The column containing information on the presence of
#'   injury.
#' @param eresponse_05_col The column representing the 911 response type.
#' @param eresponse_10_col Column name containing scene delay information.
#' @param transport_disposition_col The column for patient transport
#'   disposition.
#' @param edisposition_24_col Column name containing pre-hospital trauma alert
#'   information.
#' @param evitals_06_col Column name containing systolic blood pressure (SBP)
#'   values.
#' @param evitals_10_col Column name containing heart rate values.
#' @param evitals_12_col Column name containing pulse oximetry values.
#' @param evitals_14_col Column name containing capillary refill information.
#' @param evitals_15_col Column name containing respiratory effort values.
#' @param evitals_21_col Column name containing Glasgow Coma Scale (GCS) Motor
#'   values.
#' @param eexam_16_col Column name containing extremities assessment details.
#' @param eexam_20_col Column name containing neurological assessment details.
#' @param eexam_23_col Column name containing lung assessment details.
#' @param eexam_25_col Column name containing chest assessment details.
#' @param eprocedures_03_col Column name containing airway management or
#'   tourniquet usage details.
#' @param einjury_01_col Column name containing injury cause details.
#' @param einjury_03_col Column name containing trauma triage steps 1 and 2
#'   information.
#' @param einjury_04_col Column name containing trauma triage steps 3 and 4
#'   information.
#' @param einjury_09_col Column name containing fall height information.
#' @param ... Additional arguments passed to helper functions for further
#'   customization.
#'
#' @return A tibble summarizing results for three age groups (< 10 yrs, 10–65
#'   yrs, and >= 65 yrs) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (< 10 yrs, 10–65 yrs, >= 65 yrs).
#'   `numerator`: Count of incidents where a pre-hospital trauma alert was
#'   called.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents where a pre-hospital trauma alert was
#'   called.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_14 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      vitals_table = NULL,
                      exam_table = NULL,
                      procedures_table = NULL,
                      injury_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      eresponse_10_col,
                      transport_disposition_col,
                      edisposition_24_col,
                      evitals_06_col,
                      evitals_10_col,
                      evitals_12_col,
                      evitals_14_col,
                      evitals_15_col,
                      evitals_21_col,
                      eexam_16_col,
                      eexam_20_col,
                      eexam_23_col,
                      eexam_25_col,
                      eprocedures_03_col,
                      einjury_01_col,
                      einjury_03_col,
                      einjury_04_col,
                      einjury_09_col,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Trauma-14")

    # header
    cli::cli_h2("Gathering Records for Trauma-14")

    trauma_14_populations <- trauma_14_population(

      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      vitals_table = vitals_table,
      exam_table = exam_table,
      procedures_table = procedures_table,
      injury_table = injury_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_dob_col = {{ patient_dob_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_24_col = {{ edisposition_24_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}

    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Trauma-14")

    population_65 <- trauma_14_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = ">= 65 yrs",
        numerator_col = TRAUMA_ALERT_65,
        ...
      )

    # 10 to 64 population
    population_10_64 <- trauma_14_populations$population_10_64 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "10-64 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )

    # patients < 10 yrs
    population_10 <- trauma_14_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "< 10 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )

    # summary
    trauma.04 <- dplyr::bind_rows(population_65, population_10_64, population_10)

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {

      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, m))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, s))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(trauma.04)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) && !is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Trauma-14")

    # header
    cli::cli_h2("Gathering Records for Trauma-14")

    trauma_14_populations <- trauma_14_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_dob_col = {{ patient_dob_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_24_col = {{ edisposition_24_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}

    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Trauma-14")

    population_65 <- trauma_14_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = ">= 65 yrs",
        numerator_col = TRAUMA_ALERT_65,
        ...
      )

    # 10 to 64 population
    population_10_64 <- trauma_14_populations$population_10_64 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "10-64 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )

    # patients < 10 yrs
    population_10 <- trauma_14_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "< 10 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )

    # summary
    trauma.04 <- dplyr::bind_rows(population_65, population_10_64, population_10)

    # create a separator
    cli::cli_text("\n")

    # Calculate and display the runtime
    end_time <- Sys.time()
    run_time_secs <- difftime(end_time, start_time, units = "secs")
    run_time_secs <- as.numeric(run_time_secs)

    if (run_time_secs >= 60) {

      run_time <- round(run_time_secs / 60, 2)  # Convert to minutes and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, m))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, s))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(trauma.04)

  }

}
