#' @title Airway-01 Calculation
#'
#' @description Calculates the NEMSQA Airway-01 measure.
#'
#' Calculates the proportion of times when the first endotracheal intubation
#' attempt is successful with no peri-intubation hypoxia or hypotension.
#'
#' @param df A dataframe or tibble contianing EMS data where each row represents
#'   an observation and columns represent features.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient, escene, and earrest.01 fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations.
#' @param arrest_table A data.frame or tibble containing at least the earrest
#'   fields needed for this measure's calculations.
#' @param procedures_table A dataframe or tibble containing at least the
#'   eProcedures fields needed.
#' @param vitals_table A dataframe or tibble containing at least the eVitals
#'   fields needed.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient's numeric age agnostic
#'   of unit.
#' @param epatient_16_col Column representing the patient's age unit ("Years",
#'   "Months", "Days", "Hours", or "Minutes").
#' @param eresponse_05_col Column that contains eResponse.05.
#' @param earrest_01_col  Column representing whether or not the patient is in
#'   arrest.
#' @param evitals_01_col  Date-time or POSIXct column containing vital signs
#'   date/time
#' @param evitals_06_col  Numeric column containing systolic blood pressure
#'   values
#' @param evitals_12_col  Numeric column containing pulse oximetry values.
#' @param eprocedures_01_col  Date-time or POSIXct column for procedures
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col  Column containing procedure codes with or without
#'   procedure names.
#' @param eprocedures_05_col  Column containing a count for how many times
#'   procedure was attempted.
#' @param eprocedures_06_col  Column indicating whether or not procedure was
#'   successful.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were
#' administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#'
#' @examples
#'
#'\dontrun{
#'
#' # If you are sourcing your data from a SQL database connection
#' # or if you have your data in several different tables,
#' # you can pass table inputs versus a single data.frame or tibble
#'
#' # Get the applicable tables from `nemsqar`
#' data("nemsqar_arrest_table")
#' data("nemsqar_patient_scene_table")
#' data("nemsqar_response_table")
#' data("nemsqar_vitals_table")
#' data("nemsqar_procedures_table")
#'
#' # Run the function
#'
#' airway_01(df = NULL,
#'          patient_scene_table = nemsqar_patient_scene_table,
#'          procedures_table = nemsqar_procedures_table,
#'          vitals_table = nemsqar_vitals_table,
#'          arrest_table = nemsqar_arrest_table,
#'          response_table = nemsqar_response_table,
#'          erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
#'          incident_date_col = `Incident Date`,
#'          patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
#'          epatient_15_col = `Patient Age (ePatient.15)`,
#'          epatient_16_col = `Patient Age Units (ePatient.16)`,
#'          eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
#'          eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
#'          eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
#'          eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
#'          eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
#'          eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
#'          earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
#'          evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
#'          evitals_06_col = `Vitals Systolic Blood Pressure SBP (eVitals.06)`,
#'          evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`
#'          )
#'}
#'
#' @author Samuel Kordik, Nicolas Foss Ed.D., MS
#'
#' @export
#'
airway_01 <- function(df = NULL,
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
                      evitals_06_col,
                      evitals_12_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table),
        !is.null(response_table),
        !is.null(arrest_table),
        !is.null(procedures_table),
        !is.null(vitals_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Airway-01")

    # header
    cli::cli_h2("Gathering Records for Airway-01")

  #############################################################################
  #                                                                           #
  #     Get Population Level Information from tables and columns              #
  #                                                                           #
  #############################################################################

  airway_01_population <- airway_01_population(patient_scene_table = patient_scene_table,
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
                                               evitals_06_col = {{ evitals_06_col }},
                                               evitals_12_col = {{ evitals_12_col }},
                                               eprocedures_01_col = {{ eprocedures_01_col }},
                                               eprocedures_02_col = {{ eprocedures_02_col }},
                                               eprocedures_03_col = {{ eprocedures_03_col }},
                                               eprocedures_05_col = {{ eprocedures_05_col }},
                                               eprocedures_06_col = {{ eprocedures_06_col }}
                                               )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-01")

    # summary
    # adults
    adult_population <- airway_01_population$adults |>
      summarize_measure(measure_name = "Airway-01",
                        population_name = "Adult",
                        numerator_1,
                        ...)

    # peds
    peds_population <- airway_01_population$peds |>
      summarize_measure(measure_name = "Airway-01",
                        population_name = "Peds",
                        numerator_1,
                        ...)

    # union
    airway.01 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(airway.01)

    } else if(
      all(is.null(patient_scene_table),
          is.null(response_table),
          is.null(arrest_table),
          is.null(procedures_table),
          is.null(vitals_table)
      ) && !is.null(df)

    ) {

      # Start timing the function execution
      start_time <- Sys.time()

      # header
      cli::cli_h1("Airway-01")

      # header
      cli::cli_h2("Gathering Records for Airway-01")

      #############################################################################
      #                                                                           #
      #     Get Population Level Information from tables and columns              #
      #                                                                           #
      #############################################################################

      airway_01_population <- airway_01_population(df = df,
                                                   erecord_01_col = {{ erecord_01_col }},
                                                   incident_date_col = {{ incident_date_col }},
                                                   patient_dob_col = {{ patient_dob_col }},
                                                   epatient_15_col = {{ epatient_15_col }},
                                                   epatient_16_col = {{ epatient_16_col }},
                                                   earrest_01_col = {{ earrest_01_col }},
                                                   eresponse_05_col = {{ eresponse_05_col }},
                                                   evitals_01_col = {{ evitals_01_col }},
                                                   evitals_06_col = {{ evitals_06_col }},
                                                   evitals_12_col = {{ evitals_12_col }},
                                                   eprocedures_01_col = {{ eprocedures_01_col }},
                                                   eprocedures_02_col = {{ eprocedures_02_col }},
                                                   eprocedures_03_col = {{ eprocedures_03_col }},
                                                   eprocedures_05_col = {{ eprocedures_05_col }},
                                                   eprocedures_06_col = {{ eprocedures_06_col }}
                                                   )

      # create a separator
      cli::cli_text("\n")

      # header for calculations
      cli::cli_h2("Calculating Airway-01")

      # summary
      # adults
      adult_population <- airway_01_population$adults |>
        summarize_measure(measure_name = "Airway-01",
                          population_name = "Adult",
                          numerator_1,
                          ...)

      # peds
      peds_population <- airway_01_population$peds |>
        summarize_measure(measure_name = "Airway-01",
                          population_name = "Peds",
                          numerator_1,
                          ...)

      # union
      airway.01 <- bind_rows(adult_population, peds_population)

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

      return(airway.01)

    }

}
