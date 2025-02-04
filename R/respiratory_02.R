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
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col integer Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column name for response codes (e.g., incident type).
#' @param evitals_12_col Column name for oxygen saturation (SpO2) values.
#' @param emedications_03_col Column name for medication codes.
#' @param eprocedures_03_col Column name for procedure codes.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return A tibble summarizing results for the Adults, Peds, and all records
#'   with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (Adults, Peds, All).
#'   `numerator`: Count of EMS responses originating from a 911 request for
#'   patients with hypoxia during which oxygen is administered.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of EMS responses originating from a 911 request for
#'   patients with hypoxia during which oxygen is administered.
#'   `prop_label`: Proportion formatted as a percentage with a
#'   specified number of decimal places.
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
                           patient_dob_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col,
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
                           patient_dob_col = {{ patient_dob_col}},
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
                                      measure_name = "Respiratory-02",
                                      numerator_col = OXYGEN,
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
                           patient_dob_col = {{ patient_dob_col}},
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
                                   measure_name = "Respiratory-02",
                                   numerator_col = OXYGEN,
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

    return(respiratory.02)

    }

}
