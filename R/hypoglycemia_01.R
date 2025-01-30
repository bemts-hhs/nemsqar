#' @title Hypoglycemia-01
#'
#' @description
#'
#' The `hypoglycemia_01` function calculates the NEMSQA measure evaluating how
#' often hypoglycemic patients with altered mental status receive hypoglycemia
#' treatment.
#'
#' @param df A data frame or tibble containing emergency response records.
#'   Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   ePatient and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the
#'   eResponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing at least the
#'   eSituation fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data.frame or tibble containing at least the eVitals
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing at least the
#'   eMedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param procedures_table A data.frame or tibble containing at least the
#'   eProcedures fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col Column representing the unique record identifier.
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
#' @param eresponse_05_col Column containing response type codes.
#' @param esituation_11_col Column for primary impression fields, containing
#'   ICD-10 codes.
#' @param esituation_12_col Column for secondary impression fields, containing
#'   ICD-10 codes.
#' @param evitals_18_col Column for blood glucose levels.
#' @param evitals_23_cl Column for Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col Column for AVPU alertness levels.
#' @param emedications_03_col Column for administered medications.
#' @param eprocedures_03_col Column for procedures performed.
#' @param ... Additional arguments for summarization, passed to the summarize
#'   function.
#'
#' @return A tibble summarizing results for three population groups (All,
#'   Adults, and Peds) with the following columns:
#'
#'   `pop`: Population type (All, Adults, Peds). `numerator`: Count of incidents
#'   where specific hypoglycemia best practices were administered.
#'   `denominator`: Total count of incidents. `prop`: Proportion of incidents
#'   where specific hypoglycemia best practices were administered. `prop_label`:
#'   Proportion formatted as a percentage with a specified number of decimal
#'   places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
hypoglycemia_01 <- function(df = NULL,
                            patient_scene_table = NULL,
                            response_table = NULL,
                            situation_table = NULL,
                            vitals_table = NULL,
                            medications_table = NULL,
                            procedures_table = NULL,
                            erecord_01_col,
                            incident_date_col = NULL,
                            patient_DOB_col = NULL,
                            epatient_15_col,
                            epatient_16_col,
                            eresponse_05_col,
                            esituation_11_col,
                            esituation_12_col,
                            evitals_18_col,
                            evitals_23_cl,
                            evitals_26_col,
                            emedications_03_col,
                            eprocedures_03_col,
                            ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(medications_table),
      !is.null(procedures_table)
    )

    && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Hypoglycemia-01")

    # header
    cli::cli_h2("Gathering Records for Hypoglycemia-01")

    # gather the population of interest
    hypoglycemia_01_populations <- hypoglycemia_01_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           situation_table = situation_table,
                           vitals_table = vitals_table,
                           medications_table = medications_table,
                           procedures_table = procedures_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           esituation_11_col = {{ esituation_11_col }},
                           esituation_12_col = {{ esituation_12_col }},
                           evitals_18_col = {{ evitals_18_col }},
                           evitals_23_cl = {{ evitals_23_cl }},
                           evitals_26_col = {{ evitals_26_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Hypoglycemia-01")

    # summary
    hypoglycemia.01 <- results_summarize(total_population = hypoglycemia_01_populations$initial_population,
                                         adult_population = hypoglycemia_01_populations$adults,
                                         peds_population = hypoglycemia_01_populations$peds,
                                         measure_name = "Hypoglycemia-01",
                                         numerator_col = TREATMENT,
                                         ...)

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

    return(hypoglycemia.01)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(medications_table),
      is.null(procedures_table)
    )

    && !is.null(df)
  )

  # Start timing the function execution
  start_time <- Sys.time()

  # header
  cli::cli_h1("Hypoglycemia-01")

  # header
  cli::cli_h2("Gathering Records for Hypoglycemia-01")

    # gather the population of interest
    hypoglycemia_01_populations <- hypoglycemia_01_population(
                                                  df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col}},
                                                  epatient_15_col = {{ epatient_15_col}},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  evitals_18_col = {{ evitals_18_col }},
                                                  evitals_23_cl = {{ evitals_23_cl }},
                                                  evitals_26_col = {{ evitals_26_col }},
                                                  emedications_03_col = {{ emedications_03_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Hypoglycemia-01")

    # summary
    hypoglycemia.01 <- results_summarize(total_population = hypoglycemia_01_populations$initial_population,
                                         adult_population = hypoglycemia_01_populations$adults,
                                         peds_population = hypoglycemia_01_populations$peds,
                                         measure_name = "Hypoglycemia-01",
                                         numerator_col = TREATMENT,
                                         ...)

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

    return(hypoglycemia.01)
}
