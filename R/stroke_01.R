#' @title Stroke-01 Calculation
#'
#' @description
#'
#' The `stroke_01` function processes EMS dataset to identify potential stroke
#' cases based on specific criteria and calculates the stroke scale measures. It
#' filters the data for 911 response calls, identifies stroke-related
#' impressions and scales, and aggregates results by unique patient encounters.
#'
#' @param df A data frame or tibble containing the dataset. Each row should
#'   represent a unique patient encounter.
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
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param eresponse_05_col The column containing EMS response codes, which
#'   should include 911 response codes.
#' @param esituation_11_col The column containing the primary impression codes
#'   or descriptions related to the situation.
#' @param esituation_12_col The column containing secondary impression codes or
#'   descriptions related to the situation.
#' @param evitals_23_col The column containing the Glasgow Coma Scale (GCS)
#'   score.
#' @param evitals_26_col The column containing the AVPU (alert, verbal, pain,
#'   unresponsive) scale value.
#' @param evitals_29_col The column containing the stroke scale score achieved
#'   during assessment.
#' @param evitals_30_col The column containing stroke scale type descriptors
#'   (e.g., FAST, NIH, etc.).
#' @param ... Additional arguments passed to `dplyr::summarize()` function for
#'   further customization of results.
#'
#' @return A tibble summarizing results for the total population with the
#'   following columns: `pop`: Population type (All). `numerator`: Count of
#'   incidents where beta-agonist medications were administered. `denominator`:
#'   Total count of incidents. `prop`: Proportion of incidents involving
#'   beta-agonist medications. `prop_label`: Proportion formatted as a
#'   percentage with a specified number of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
stroke_01 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      evitals_23_col,
                      evitals_26_col,
                      evitals_29_col,
                      evitals_30_col,
                      ...) {

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
    cli::cli_h1("Stroke-01")

    # header
    cli::cli_h2("Gathering Records for Stroke-01")

    # gather the population of interest
    stroke_01_populations <- stroke_01_population(patient_scene_table = patient_scene_table,
                                                    response_table = response_table,
                                                    situation_table = situation_table,
                                                    vitals_table = vitals_table,
                                                    erecord_01_col = {{ erecord_01_col }},
                                                    eresponse_05_col = {{ eresponse_05_col }},
                                                    esituation_11_col = {{ esituation_11_col }},
                                                    esituation_12_col = {{ esituation_12_col }},
                                                    evitals_23_col = {{ evitals_23_col }},
                                                    evitals_26_col = {{ evitals_26_col }},
                                                    evitals_29_col = {{ evitals_29_col }},
                                                    evitals_30_col = {{ evitals_30_col }}
                                                    )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Stroke-01")

    # summarize
    stroke.01 <- stroke_01_populations$initial_population |>
      summarize_measure(measure_name = "Stroke-01",
                        population_name = "All",
                        numerator_col = STROKE_SCALE,
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
      cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(stroke.01)

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
    cli::cli_h1("Stroke-01")

    # header
    cli::cli_h2("Gathering Records for Stroke-01")

    # gather the population of interest
    stroke_01_populations <- stroke_01_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  evitals_23_col = {{ evitals_23_col }},
                                                  evitals_26_col = {{ evitals_26_col }},
                                                  evitals_29_col = {{ evitals_29_col }},
                                                  evitals_30_col = {{ evitals_30_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Stroke-01")

    # summarize
    stroke.01 <- stroke_01_populations$initial_population |>
      summarize_measure(measure_name = "Stroke-01",
                        population_name = "All",
                        numerator_col = STROKE_SCALE,
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
      cli_alert_success("Function completed in {col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli_alert_success("Function completed in {col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(stroke.01)

  }

}
