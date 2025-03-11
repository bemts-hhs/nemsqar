#' @title Airway-01 Calculation
#'
#' @description Calculates the NEMSQA Airway-01 measure.
#'
#' Calculates the proportion of times when the first endotracheal intubation
#' attempt is successful with no peri-intubation hypoxia or hypotension.
#'
#' @param df A dataframe or tibble containing EMS data where each row represents
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
#' @param patient_DOB_col Column that contains the patient's date of birth. This
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
#' @param confidence_interval Logical. If `TRUE`, the function calculates a
#'   confidence interval for the proportion estimate.
#' @param method Character. Specifies the method used to calculate confidence
#'   intervals. Options are `"wilson"` (Wilson score interval) and
#'   `"clopper-pearson"` (exact binomial interval). Partial matching is
#'   supported, so `"w"` and `"c"` can be used as shorthand.
#' @param conf.level Numeric. The confidence level for the interval, expressed
#'   as a proportion (e.g., 0.95 for a 95% confidence interval). Defaults to
#'   0.95.
#' @param correct Logical. If `TRUE`, applies a continuity correction to the
#'   Wilson score interval when `method = "wilson"`. Defaults to `TRUE`.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
#'
#' @return A data.frame summarizing results for two population groups (Adults
#'   and Peds) with the following columns:
#' - `pop`: Population type (Adults and Peds).
#' - `numerator`: Count of incidents meeting the measure.
#' - `denominator`: Total count of included incidents.
#' - `prop`: Proportion of incidents meeting the measure.
#' - `prop_label`: Proportion formatted as a percentage with a specified number
#'    of decimal places.
#' - `lower_ci`: Lower bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#' - `upper_ci`: Upper bound of the confidence interval for `prop`
#'    (if `confidence_interval = TRUE`).
#'
#' @examples
#'
#' # If you are sourcing your data from a SQL database connection
#' # or if you have your data in several different tables,
#' # you can pass table inputs versus a single data.frame or tibble
#'
#' # create tables to test correct functioning
#'
#'   # patient table
#'   patient_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'     "2025-01-01", "2025-06-01")), 2),
#'     patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'                                 "2023-01-01", "1970-06-01")), 2),
#'     epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
#'     epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2)
#'
#'   )
#'
#'   # response table
#'   response_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eresponse_05 = rep(2205001, 10)
#'
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 22:59:00",
#'     "2025-01-05 11:58:00", "2025-02-01 18:57:00", "2025-01-01 04:58:00",
#'     "2025-06-01 12:57:00", "2025-01-01 23:05:00", "2025-01-05 12:04:00",
#'     "2025-02-01 19:03:00", "2025-01-01 05:02:00", "2025-06-01 13:01:00")),
#'     evitals_06 = rep(c(90, 100, 102, 103, 104), 2),
#'     evitals_12 = rep(c(90, 91, 92, 93, 94), 2)
#'
#'   )
#'
#' # arrest table
#'   arrest_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     earrest_01 = rep("No", 10)
#'   )
#'
#'   # procedures table
#'   procedures_table <- tibble::tibble(
#'
#'     erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'     eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
#'     "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
#'     "2025-06-01 13:00:00")), 2),
#'     eprocedures_02 = rep("No", 10),
#'     eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
#'                            673005), 2),
#'     eprocedures_05 = rep(1, 10),
#'     eprocedures_06 = rep(9923003, 10)
#'
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' airway_01(df = NULL,
#'          patient_scene_table = patient_table,
#'          procedures_table = procedures_table,
#'          vitals_table = vitals_table,
#'          arrest_table = arrest_table,
#'          response_table = response_table,
#'          erecord_01_col = erecord_01,
#'          incident_date_col = incident_date,
#'          patient_DOB_col = patient_dob,
#'          epatient_15_col = epatient_15,
#'          epatient_16_col = epatient_16,
#'          eresponse_05_col = eresponse_05,
#'          eprocedures_01_col = eprocedures_01,
#'          eprocedures_02_col = eprocedures_02,
#'          eprocedures_03_col = eprocedures_03,
#'          eprocedures_05_col = eprocedures_05,
#'          eprocedures_06_col = eprocedures_06,
#'          earrest_01_col = earrest_01,
#'          evitals_01_col = evitals_01,
#'          evitals_06_col = evitals_06,
#'          evitals_12_col = evitals_12,
#'          confidence_interval = TRUE
#'          )
#'
#' @author Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS
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
                      patient_DOB_col = NULL,
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
                      confidence_interval = FALSE,
                      method = c("wilson", "clopper-pearson"),
                      conf.level = 0.95,
                      correct = TRUE,
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
                                               patient_DOB_col = {{ patient_DOB_col }},
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
    airway.01 <- results_summarize(
      total_population = NULL,
      adult_population = airway_01_population$adults,
      peds_population = airway_01_population$peds,
      measure_name = "Airway-01",
      population_names = c("adults", "peds"),
      numerator_col = numerator_1,
      confidence_interval = confidence_interval,
      method = method,
      conf.level = conf.level,
      correct = correct,
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
                                                   patient_DOB_col = {{ patient_DOB_col }},
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
      airway.01 <- results_summarize(
        adult_population = airway_01_population$adults,
        peds_population = airway_01_population$peds,
        measure_name = "Airway-01",
        population_names = c("adults", "peds"),
        numerator_col = numerator_1,
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
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
