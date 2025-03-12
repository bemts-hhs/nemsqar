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
#' @param eprocedures_06_col Column name for procedure success codes.
#' @param eairway_02_col Column name for airway procedure data (datetime).
#'   Default is `NULL`.
#' @param eairway_04_col Column name for airway procedure data. Default is
#'   `NULL`.
#' @param evitals_01_col Column name for vital signs data (datetime).
#' @param evitals_16_col Column name for additional vital signs data.
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
#'     evitals_01 = lubridate::as_datetime(c("2025-01-01 23:02:00",
#'     "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
#'     "2025-06-01 13:01:00", "2025-01-01 23:02:00",
#'     "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
#'     "2025-06-01 13:06:00")),
#'     evitals_16 = rep(c(5, 6, 7, 8, 9), 2)
#'
#'   )
#'
#'   # airway table
#'   airway_table <- tibble::tibble(
#'   erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
#'   eairway_02 = rep(lubridate::as_datetime(c("2025-01-01 23:05:00",
#'     "2025-01-05 12:02:00", "2025-02-01 19:03:00", "2025-01-01 05:04:00",
#'     "2025-06-01 13:06:00")), 2),
#'   eairway_04 = rep(4004019, 10)
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
#'     eprocedures_06 = rep(9923003, 10)
#'
#'   )
#'
#' # Run the function
#' # Return 95% confidence intervals using the Wilson method
#' airway_18(df = NULL,
#'          patient_scene_table = patient_table,
#'          procedures_table = procedures_table,
#'          vitals_table = vitals_table,
#'          response_table = response_table,
#'          airway_table = airway_table,
#'          erecord_01_col = erecord_01,
#'          incident_date_col = incident_date,
#'          patient_DOB_col = patient_dob,
#'          epatient_15_col = epatient_15,
#'          epatient_16_col = epatient_16,
#'          eresponse_05_col = eresponse_05,
#'          eprocedures_01_col = eprocedures_01,
#'          eprocedures_02_col = eprocedures_02,
#'          eprocedures_03_col = eprocedures_03,
#'          eprocedures_06_col = eprocedures_06,
#'          evitals_01_col = evitals_01,
#'          evitals_16_col = evitals_16,
#'          eairway_02_col = eairway_02,
#'          eairway_04_col = eairway_04,
#'          confidence_interval = TRUE
#'          )
#'
#' @author Nicolas Foss, Ed.D., MS, Samuel Kordik, BBA, BS
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
                      eprocedures_06_col,
                      eairway_02_col = NULL,
                      eairway_04_col = NULL,
                      evitals_01_col,
                      evitals_16_col,
                      confidence_interval = FALSE,
                      method = c("wilson", "clopper-pearson"),
                      conf.level = 0.95,
                      correct = TRUE,
                      ...) {

  # Set default method and adjustment method
  method <- match.arg(method, choices = c("wilson", "clopper-pearson"))

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
    airway.18 <- results_summarize(
      total_population = NULL,
      adult_population = airway_18_populations$adults,
      peds_population = airway_18_populations$peds,
      measure_name = "Airway-18",
      population_names = c("adults", "peds"),
      numerator_col = NUMERATOR,
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

    # when confidence interval is "wilson", check for n < 10
    # to warn about incorrect Chi-squared approximation
    if (any(airway.18$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

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
    airway.18 <- results_summarize(
      adult_population = airway_18_populations$adults,
      peds_population = airway_18_populations$peds,
      measure_name = "Airway-18",
      population_names = c("adults", "peds"),
      numerator_col = NUMERATOR,
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

    # when confidence interval is "wilson", check for n < 10
    # to warn about incorrect Chi-squared approximation
    if (any(airway.18$denominator < 10) && method == "wilson" && confidence_interval) {

      cli::cli_warn("In {.fn prop.test}: Chi-squared approximation may be incorrect for any n < 10.")

    }

    return(airway.18)

  }

}
