#' @title Safety-04 Calculation
#'
#' @description
#'
#' The `safety_04` function processes EMS incident data for specific safety and
#' transport criteria, filtering by patient age and incident type to identify
#' cases that meet specified exclusion or inclusion criteria. This function
#' accommodates data with various EMS-specific codes, age descriptors, and
#' procedure identifiers.
#'
#' @param df A data frame or tibble containing EMS data where each row
#'   represents an individual observation.
#' @param patient_scene_table A data frame or tibble containing fields from
#'   epatient and escene needed for this measure's calculations.
#' @param response_table A data frame or tibble containing fields from eresponse
#'   needed for this measure's calculations.
#' @param arrest_table A data frame or tibble containing fields from earrest
#'   needed for this measure's calculations.
#' @param injury_table A data frame or tibble containing fields from einjury
#'   needed for this measure's calculations.
#' @param procedures_table A data frame or tibble containing fields from
#'   eprocedures needed for this measure's calculations.
#' @param disposition_table A data frame or tibble containing fields from
#'   edisposition needed for this measure's calculations.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name indicating the patient age.
#' @param epatient_16_col Column name for the unit of age (e.g., "Years,"
#'   "Months").
#' @param eresponse_05_col Column containing response transport codes.
#' @param earrest_01_col Column with cardiac arrest status information.
#' @param einjury_03_col Column describing traumatic injuries, expected as a
#'   list or text-separated entries.
#' @param eprocedures_03_col Column listing procedures, assumed to contain
#'   multiple procedure codes/texts in each cell.
#' @param edisposition_14_col Column for transport dispositions.
#' @param transport_disposition_col Columns for primary and secondary transport
#'   dispositions.
#' @param ... Additional arguments for flexibility in function customization.
#'
#' @return A data.frame summarizing results for three population groups (All,
#'   Adults, and Peds) with the following columns:
#'
#'   `measure`: The name of the measure being calculated.
#'   `pop`: Population type (All, Adults, or Peds).
#'   `numerator`: Count of incidents where beta-agonist medications were
#'   administered.
#'   `denominator`: Total count of incidents.
#'   `prop`: Proportion of incidents involving beta-agonist medications.
#'   `prop_label`: Proportion formatted as a percentage with a specified number
#'   of decimal places.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
safety_04 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      arrest_table = NULL,
                      injury_table = NULL,
                      procedures_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      earrest_01_col,
                      einjury_03_col,
                      eprocedures_03_col,
                      edisposition_14_col,
                      transport_disposition_col,
                      ...) {

  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(arrest_table),
      !is.null(injury_table),
      !is.null(procedures_table),
      !is.null(disposition_table)
    ) && is.null(df)

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-04")

    # header
    cli::cli_h2("Gathering Records for Safety-04")

    # gather the population of interest
    safety_04_populations <- safety_04_population(patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  arrest_table = arrest_table,
                                                  injury_table = injury_table,
                                                  procedures_table = procedures_table,
                                                  disposition_table = disposition_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_dob_col = {{ patient_dob_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  earrest_01_col = {{ earrest_01_col }},
                                                  einjury_03_col = {{ einjury_03_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  edisposition_14_col = {{ edisposition_14_col }},
                                                  transport_disposition_col = {{ transport_disposition_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-04")

    # summary
    safety.04 <- safety_04_populations$peds |>
      summarize_measure(measure_name = "Safety-04",
                        population_name = "Peds",
                        numerator_col = CAR_SEAT,
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

    return(safety.04)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(arrest_table),
      is.null(injury_table),
      is.null(procedures_table),
      is.null(disposition_table)
    ) && !is.null(df)

    # utilize a dataframe to analyze the data for the measure analytics

  ) {

    # Start timing the function execution
    start_time <- Sys.time()

    # header
    cli::cli_h1("Safety-04")

    # header
    cli::cli_h2("Gathering Records for Safety-04")

    # gather the population of interest
    safety_04_populations <- safety_04_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_dob_col = {{ patient_dob_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  earrest_01_col = {{ earrest_01_col }},
                                                  einjury_03_col = {{ einjury_03_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  edisposition_14_col = {{ edisposition_14_col }},
                                                  transport_disposition_col = {{ transport_disposition_col }}
                                                  )

    # create a separator
    cli::cli_text("\n")

    # header for calculations
    cli::cli_h2("Calculating Safety-04")

    # summary
    safety.04 <- safety_04_populations$peds |>
      summarize_measure(measure_name = "Safety-04",
                        population_name = "Peds",
                        numerator_col = CAR_SEAT,
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

    return(safety.04)

  }

}
