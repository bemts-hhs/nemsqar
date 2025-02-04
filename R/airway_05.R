#' @title Airway-05 Calculation
#'
#' @description
#'
#' Calculates the NEMSQA Airway-05 measure.
#'
#' Calculates the proportion of endotracheal intubation attempts with adequate
#' oxygenation.
#'
#' @param df A dataframe or tibble contianing EMS data where each row represents
#'   an observation and columns represent features. Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient, escene, and earrest.01 fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param arrest_table A data.frame or tibble containing at least the earrest
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A dataframe or tibble containing at least the
#'   eProcedures fields needed. Default is `NULL`.
#' @param vitals_table A dataframe or tibble containing at least the eVitals
#'   fields needed. Default is `NULL`.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. Default is
#'   `NULL`.
#' @param patient_dob_col Column that contains the patient's date of birth.
#'   Default is `NULL`.
#' @param epatient_15_col Column representing the patient's numeric age agnostic
#'   of unit.
#' @param epatient_16_col Column representing the patient's age unit ("Years",
#'   "Months", "Days", "Hours", or "Minutes").
#' @param eresponse_05_col Column that contains eResponse.05.
#' @param earrest_01_col Column representing whether or not the patient is in
#'   arrest.
#' @param evitals_01_col Date-time or POSIXct column containing vital signs
#'   date/time
#' @param evitals_12_col Numeric column containing pulse oximetry values.
#' @param eprocedures_01_col Date-time or POSIXct column for procedures
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col Column containing procedure codes with or without
#'   procedure names.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#'
#' `measure`: The name of the measure being calculated.
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were
#' administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @author Samuel Kordik, Nicolas Foss Ed.D., MS
#'
#' @export
#'
airway_05 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      arrest_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      earrest_01_col,
                      eresponse_05_col,
                      evitals_01_col,
                      evitals_12_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table),
        !is.null(response_table),
        !is.null(procedures_table),
        !is.null(vitals_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Airway-05")

    # header
    cli::cli_h2("Gathering Records for Airway-05")

  #############################################################################
  #                                                                           #
  #     Get Population Level Information from tables and columns              #
  #                                                                           #
  #############################################################################

  airway_05_population <- airway_05_population(patient_scene_table = patient_scene_table,
                                               response_table = response_table,
                                               arrest_table = arrest_table,
                                               procedures_table = procedures_table,
                                               vitals_table = vitals_table,
                                               erecord_01_col = {{ erecord_01_col }},
                                               incident_date_col = {{ incident_date_col }},
                                               patient_dob_col = {{ patient_dob_col }},
                                               epatient_15_col = {{ epatient_15_col }},
                                               epatient_16_col = {{ epatient_16_col }},
                                               earrest_01_col = {{ earrest_01_col }},
                                               eresponse_05_col = {{ eresponse_05_col }},
                                               evitals_01_col = {{ evitals_01_col }},
                                               evitals_12_col = {{ evitals_12_col }},
                                               eprocedures_01_col = {{ eprocedures_01_col }},
                                               eprocedures_02_col = {{ eprocedures_02_col }},
                                               eprocedures_03_col = {{ eprocedures_03_col }}
                                               )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-05")

    # summary
    # adults
    adult_population <- airway_05_population$adults |>
      summarize_measure(measure_name = "Airway-05",
                        population_name = "Adult",
                        numerator,
                        ...)

    # peds
    peds_population <- airway_05_population$peds |>
      summarize_measure(measure_name = "Airway-05",
                        population_name = "Peds",
                        numerator,
                        ...)

    # union
    airway.05 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(airway.05)

    } else if(
    all(is.null(patient_scene_table),
        is.null(response_table),
        is.null(procedures_table),
        is.null(vitals_table),
        is.null(arrest_table)

    ) && !is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Airway-05")

    # header
    cli::cli_h2("Gathering Records for Airway-05")

  #############################################################################
  #                                                                           #
  #     Get Population Level Information from tables and columns              #
  #                                                                           #
  #############################################################################

  airway_05_population <- airway_05_population(df = df,
                                               erecord_01_col = {{ erecord_01_col }},
                                               incident_date_col = {{ incident_date_col }},
                                               patient_dob_col = {{ patient_dob_col }},
                                               epatient_15_col = {{ epatient_15_col }},
                                               epatient_16_col = {{ epatient_16_col }},
                                               earrest_01_col = {{ earrest_01_col }},
                                               eresponse_05_col = {{ eresponse_05_col }},
                                               evitals_01_col = {{ evitals_01_col }},
                                               evitals_12_col = {{ evitals_12_col }},
                                               eprocedures_01_col = {{ eprocedures_01_col }},
                                               eprocedures_02_col = {{ eprocedures_02_col }},
                                               eprocedures_03_col = {{ eprocedures_03_col }}
                                               )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-05")

    # summary
    # adults
    adult_population <- airway_05_population$adults |>
      summarize_measure(measure_name = "Airway-05",
                        population_name = "Adult",
                        numerator,
                        ...)

    # peds
    peds_population <- airway_05_population$peds |>
      summarize_measure(measure_name = "Airway-05",
                        population_name = "Peds",
                        numerator,
                        ...)

    # union
    airway.05 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(airway.05)

    }
}
