#' @title Safety-02 Calculation
#'
#' @description
#'
#' The `safety_02` function calculates the Safety-02 metric, evaluating the
#' proportion of emergency medical calls involving transport where no lights and
#' sirens were used. This function categorizes the population into adult and
#' pediatric groups based on their age, and summarizes results with a total
#' population count as well.
#'
#' @param df A data frame where each row is an observation, and each column
#'   represents a feature.
#' @param patient_scene_table A data.frame or tibble containing only epatient
#'   and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations.
#' @param disposition_table A data.frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column giving response codes, identifying 911
#'   responses.
#' @param edisposition_18_col Column giving transport mode descriptors,
#'   including possible lights-and-sirens indicators.
#' @param edisposition_28_col Column giving patient evaluation and care
#'   categories for the EMS response.
#' @param transport_disposition_cols One or more unquoted column names (such as
#'   edisposition.12, edisposition.30) containing transport disposition details.
#' @param ... Additional arguments for summary calculation, if needed.
#'
#' @return A tibble summarizing results for three age groups (< 10 yrs, 10–65
#'   yrs, and >= 65 yrs) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (< 18 yrs, >= 18 yrs, all).
#'   `numerator`: Count of incidents from a 911 request during which lights and
#'   sirens were not used during patient transport.
#'   `denominator`: Total count of incidents.
#'   `prop`:Proportion of incidents from a 911 request during which lights and
#'   sirens were not used during patient transport.
#'   `prop_label`: Proportion formatted
#'   as a percentage with a specified number of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_02 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      edisposition_18_col,
                      edisposition_28_col,
                      transport_disposition_cols,
                      ...) {


  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(disposition_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-02")

    # header
    cli::cli_h2("Gathering Records for Safety-02")

    # gather the population of interest
    safety_02_populations <- safety_02_population(patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  disposition_table = disposition_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  edisposition_18_col = {{ edisposition_18_col }},
                                                  edisposition_28_col = {{ edisposition_28_col }},
                                                  transport_disposition_cols = {{ transport_disposition_cols }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-02")

    # summary
    safety.02 <- results_summarize(total_population = safety_02_populations$initial_population,
                                   adult_population = safety_02_populations$adults,
                                   peds_population = safety_02_populations$peds,
                                   measure_name = "Safety-02",
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
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(safety.02)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(disposition_table)
    ) && !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-02")

    # header
    cli::cli_h2("Gathering Records for Safety-02")

    # gather the population of interest
    safety_02_populations <- safety_02_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  edisposition_18_col = {{ edisposition_18_col }},
                                                  edisposition_28_col = {{ edisposition_28_col }},
                                                  transport_disposition_cols = {{ transport_disposition_cols }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-02")

    # summary
    safety.02 <- results_summarize(total_population = safety_02_populations$initial_population,
                                   adult_population = safety_02_populations$adults,
                                   peds_population = safety_02_populations$peds,
                                   measure_name = "Safety-02",
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
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 'm'))}.")

    } else {

      run_time <- round(run_time_secs, 2)  # Keep in seconds and round
      cli::cli_alert_success("Function completed in {cli::col_green(paste0(run_time, 's'))}.")

    }

    # create a separator
    cli::cli_text("\n")

    return(safety.02)

  }

}
