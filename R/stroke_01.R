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
#'   following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (All).
#'   `numerator`: Count of incidents where beta-agonist medications were
#'   administered.
#'   `denominator`:
#'   Total count of incidents.
#'   `prop`: Proportion of incidents involving beta-agonist medications.
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
#'     esituation_11 = c(rep("I60", 3), rep("I61", 2)),
#'     esituation_12 = c(rep("I63", 2), rep("I64", 3)),
#'     evitals_23 = c(16, 15, 14, 13, 12),
#'     evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
#'     evitals_29 = rep("positive", 5),
#'     evitals_30 = rep("a pain scale", 5)
#'   )
#'
#'   # Run the function
#'   stroke_01(
#'     df = test_data,
#'     erecord_01_col = erecord_01,
#'     eresponse_05_col = eresponse_05,
#'     esituation_11_col = esituation_11,
#'     esituation_12_col = esituation_12,
#'     evitals_23_col = evitals_23,
#'     evitals_26_col = evitals_26,
#'     evitals_29_col = evitals_29,
#'     evitals_30_col = evitals_30
#'   )
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
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

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
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(stroke.01)

  }

}
