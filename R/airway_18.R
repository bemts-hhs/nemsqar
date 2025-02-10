#' @title Airway-18 Calculation
#'
#' @description
#'
#' This function processes and analyzes the dataset to calculate the "Airway-18"
#' NEMSQA metric. It includes cleaning and transforming several columns related
#' to patient data, airway procedures, and vital signs, and it returns a cleaned
#' dataset with the relevant calculations. The final calculation is an
#' assessment of the successful last invasive airway procedures performed during
#' an EMS response originating from a 911 request in which waveform capnography
#' is used for tube placement confirmation.
#'
#' @param df A data frame or tibble containing the dataset to be processed.
#'   Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only ePatient
#'   and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eResponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A data frame or tibble containing only the
#'   eProcedures fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param airway_table A data frame or tibble containing only the eAirway fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the eVitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name containing the unique patient record
#'   identifier.
#' @param incident_date_col Column name containing the incident date. Default is
#'   `NULL`.
#' @param patient_DOB_col Column name containing the patient's date of birth.
#'   Default is `NULL`.
#' @param epatient_15_col Column name for patient information (exact purpose
#'   unclear).
#' @param epatient_16_col Column name for patient information (exact purpose
#'   unclear).
#' @param eresponse_05_col Column name for emergency response codes.
#' @param eprocedures_01_col Column name for procedure times or other related
#'   data.
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col Column name for procedure codes.
#' @param eprocedures_05_col Column name for number of procedure attempts.
#' @param eprocedures_06_col Column name for procedure success codes.
#' @param eairway_02_col Column name for airway procedure data (datetime).
#'   Default is `NULL`.
#' @param eairway_04_col Column name for airway procedure data. Default is
#'   `NULL`.
#' @param evitals_01_col Column name for vital signs data (datetime).
#' @param evitals_16_col Column name for additional vital signs data.
#' @param ... Additional arguments passed to other functions if needed.
#'
#' @return A tibble summarizing results for Adults and Peds with the following
#'   columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (Adults, Peds).
#'   `numerator`: Count of incidents where waveform capnography is used for
#'    tube placement confirmation on the last successful invasive airway
#'    procedure.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents where waveform capnography is used for
#'   tube placement confirmation on the last successful invasive airway
#'   procedure.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
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
#' data("nemsqar_airway_table")
#' data("nemsqar_patient_scene_table")
#' data("nemsqar_response_table")
#' data("nemsqar_vitals_table")
#' data("nemsqar_procedures_table")
#'
#' # Run the function
#'
#' airway_18(df = NULL,
#'          patient_scene_table = nemsqar_patient_scene_table,
#'          procedures_table = nemsqar_procedures_table,
#'          vitals_table = nemsqar_vitals_table,
#'          airway_table = nemsqar_airway_table,
#'          response_table = nemsqar_response_table,
#'          erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
#'          incident_date_col = `Incident Date`,
#'          patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
#'          epatient_15_col = `Patient Age (ePatient.15)`,
#'          epatient_16_col = `Patient Age Units (ePatient.16)`,
#'          eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
#'          eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
#'          eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
#'          eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
#'          eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
#'          eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
#'          eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
#'          eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
#'          evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
#'          evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
#'          )
#'
#' # You can also pass a data.frame that has all the fields
#' # necessary to calculate the measure, and `airway_18` will
#' # take care of any one-to-many or many-to-many relationships for you
#'
#' # Load the data.frame from the package
#' data("nemsqar_airway_18_df")
#'
#' # Run the function
#'
#' airway_18(df = nemsqar_airway_18_df,
#'          patient_scene_table = NULL,
#'          procedures_table = NULL,
#'          vitals_table = NULL,
#'          airway_table = NULL,
#'          response_table = NULL,
#'          erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
#'          incident_date_col = `Incident Date`,
#'          patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
#'          epatient_15_col = `Patient Age (ePatient.15)`,
#'          epatient_16_col = `Patient Age Units (ePatient.16)`,
#'          eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
#'          eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
#'          eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
#'          eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
#'          eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
#'          eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
#'          eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
#'          eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
#'          evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
#'          evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
#'          )
#'
#'}
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
airway_18 <- function(df = NULL,
                      patient_scene_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      airway_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      eairway_02_col = NULL,
                      eairway_04_col = NULL,
                      evitals_01_col,
                      evitals_16_col,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(airway_table),
      !is.null(response_table)
    ) && is.null(df)

  ) {

    # header
    cli::cli_h1("Airway-18")

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h2("Gathering Records for Airway-18")

    # gather the population of interest
    airway_18_populations <- airway_18_population(patient_scene_table = patient_scene_table,
                                                  procedures_table = procedures_table,
                                                  vitals_table = vitals_table,
                                                  airway_table = airway_table,
                                                  response_table = response_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eprocedures_01_col = {{ eprocedures_01_col }},
                                                  eprocedures_02_col = {{ eprocedures_02_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  eprocedures_05_col = {{ eprocedures_05_col }},
                                                  eprocedures_06_col = {{ eprocedures_06_col }},
                                                  eairway_02_col = {{ eairway_02_col }},
                                                  eairway_04_col = {{ eairway_04_col }},
                                                  evitals_01_col = {{ evitals_01_col }},
                                                  evitals_16_col = {{ evitals_16_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-18")

    # summary
    # adults
    adult_population <- airway_18_populations$adults |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Adult",
                        NUMERATOR,
                        ...)

    # peds
    peds_population <- airway_18_populations$peds |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Peds",
                        NUMERATOR,
                        ...)

    # union
    airway.18 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(airway.18)

  } else if(
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(airway_table),
      is.null(response_table)
    ) && !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Airway-18")

    # header
    cli::cli_h2("Gathering Records for Airway-18")

    # gather the population of interest
    airway_18_populations <- airway_18_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eprocedures_01_col = {{ eprocedures_01_col }},
                                                  eprocedures_02_col = {{ eprocedures_02_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  eprocedures_05_col = {{ eprocedures_05_col }},
                                                  eprocedures_06_col = {{ eprocedures_06_col }},
                                                  eairway_02_col = {{ eairway_02_col }},
                                                  eairway_04_col = {{ eairway_04_col }},
                                                  evitals_01_col = {{ evitals_01_col }},
                                                  evitals_16_col = {{ evitals_16_col }}
    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Airway-18")

    # summary
    # adults
    adult_population <- airway_18_populations$adults |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Adult",
                        NUMERATOR,
                        ...)

    # peds
    peds_population <- airway_18_populations$peds |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Peds",
                        NUMERATOR,
                        ...)

    # union
    airway.18 <- dplyr::bind_rows(adult_population, peds_population)

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

    return(airway.18)

  }

}
