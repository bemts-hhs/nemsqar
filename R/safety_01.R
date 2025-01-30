#' @title Safety-01 Calculation
#'
#' @description
#'
#' The `safety_01` function calculates the proportion of 911 responses where
#' "lights and sirens" were not used in an EMS dataset. It generates age-based
#' population summaries, calculating the count and proportion of "lights and
#' sirens" responses among all incidents, and within adult and pediatric groups.
#'
#' @param df A data frame or tibble containing EMS data.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name containing the unique patient record
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column containing age.
#' @param epatient_16_col Column for age units.
#' @param eresponse_05_col Column containing response mode codes (e.g., 911
#'   response codes).
#' @param eresponse_24_col Column detailing additional response descriptors as
#'   text.
#' @param ... arguments passed on to summarize.
#'
#' @return A tibble summarizing results for the Adults, Peds, and all records
#'   with the following columns: `pop`: Population type (Adults, Peds, All).
#'   `numerator`: Count of 911 responses where "lights and sirens" were not used
#'   in an EMS dataset. `denominator`: Total count of incidents. `prop`:
#'   Proportion of 911 responses where "lights and sirens" were not used in an
#'   EMS dataset. `prop_label`: Proportion formatted as a percentage with a
#'   specified number of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_01 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eresponse_24_col,
                      ...) {

    # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-01")

    # header
    cli::cli_h2("Gathering Records for Safety-01")

    # gather the population of interest
    safety_01_populations <- safety_01_population(patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eresponse_24_col = {{ eresponse_24_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-01")

    # summary
    safety.01 <- results_summarize(total_population = safety_01_populations$initial_population,
                                   adult_population = safety_01_populations$adults,
                                   peds_population = safety_01_populations$peds,
                                   measure_name = "Safety-01",
                                   numerator_col = NO_LS_CHECK,
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

    return(safety.01)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table)
    ) && !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-01")

    # header
    cli::cli_h2("Gathering Records for Safety-01")

    # gather the population of interest
    safety_01_populations <- safety_01_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eresponse_24_col = {{ eresponse_24_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-01")

    # summary
    safety.01 <- results_summarize(total_population = safety_01_populations$initial_population,
                                   adult_population = safety_01_populations$adults,
                                   peds_population = safety_01_populations$peds,
                                   measure_name = "Safety-01",
                                   numerator_col = NO_LS_CHECK,
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

    return(safety.01)

  }

}
