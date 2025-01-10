#' TTR-01 Calculation
#'
#' This function calculates the TTR_01 measure, which evaluates the completeness of vitals documentation for patients not experiencing cardiac arrest who were also not transported during a 911 response. It determines the total population, adult population, and pediatric population meeting the criteria for the TTR_01 measure. 
#' 
#' @section Data Assumptions:
#' 
#' This function assumes the input dataset contains the *initial* vital signs for 
#' - respiratory rate, 
#' - systolic blood pressure (SBP), 
#' - diastolic blood pressure (DBP), 
#' - heart rate (HR), 
#' - pulse oximetry (sp02), and 
#' - total Glasgow Coma Scale (GCS) score, respectively.  
#' - Complete AVPU responses are expected.
#'
#' @param df A data frame or tibble containing the dataset to analyze. Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param disposition_table A data frame or tibble containing only the edisposition fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param arrest_table A data frame or tibble containing only the earrest fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> A column specifying unique patient records.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> A column indicating the incident date. Must be of class `Date` or `POSIXct`.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient's date of birth. Must be of class `Date` or `POSIXct`.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> A column indicating the patient’s age in numeric form.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> A column specifying the unit of patient age (e.g., "Years", "Days").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> A column specifying the type of response (e.g., 911 codes).
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> A column specifying transport disposition for the patient.
#' @param earrest_01_col <['tidy-select'][dplyr_tidy_select]> A column containing cardiac arrest data.
#' @param evitals_06_col <['tidy-select'][dplyr_tidy_select]> A column containing systolic blood pressure (SBP) data from initial vital signs.
#' @param evitals_07_col <['tidy-select'][dplyr_tidy_select]> A column containing diastolic blood pressure (DBP) data from initial vital signs.
#' @param evitals_10_col <['tidy-select'][dplyr_tidy_select]> A column containing heart rate data from initial vital signs.
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> A column containing spO2 data from the initial vital signs.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> A column containing respiratory rate data from initial vital signs.
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> A column containing total Glasgow Coma Scale (GCS) scores from initial vital signs.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> A column containing alert, verbal, painful, unresponsive (AVPU) vital signs.
#' @param ... Additional arguments passed to the `summarize_measure` function.
#'
#' @return A tibble summarizing results for three population groups (Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (Adults, Peds).
#' `numerator`: Count of incidents where all applicable vital signs are taken.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where all applicable vital signs are taken.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
ttr_01 <- function(df = NULL,
                   patient_scene_table = NULL,
                   response_table = NULL,
                   disposition_table = NULL,
                   vitals_table = NULL,
                   arrest_table = NULL,
                   erecord_01_col,
                   incident_date_col = NULL,
                   patient_DOB_col = NULL,
                   epatient_15_col,
                   epatient_16_col,
                   eresponse_05_col,
                   transport_disposition_col,
                   earrest_01_col,
                   evitals_06_col,
                   evitals_07_col,
                   evitals_10_col,
                   evitals_12_col,
                   evitals_14_col,
                   evitals_23_col,
                   evitals_26_col,
                   ...
                   ) {
  
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(arrest_table),
      !is.null(disposition_table),
      !is.null(response_table)
    ) &&
    
    is.null(df)
    
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # Header
    cli::cli_h1("TTR-01")
    
    # Header
    cli::cli_h2("Gathering Records for TTR-01")
    
    # Gather the population of interest
    ttr_01_populations <- ttr_01_population(
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      disposition_table = disposition_table,
      vitals_table = vitals_table,
      arrest_table = arrest_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      earrest_01_col = {{ earrest_01_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_07_col = {{ evitals_07_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating TTR-01")
    
    # summarize
    
    # adults
    adult_population <- ttr_01_populations$adults |>
      summarize_measure(measure_name = "TTR_01",
                        population_name = "Adult",
                        VITALS,
                        ...)
    
    # peds
    peds_population <- ttr_01_populations$peds |>
      summarize_measure(measure_name = "TTR_01",
                        population_name = "Peds",
                        VITALS,
                        ...) 
    
    # summary
    ttr.01 <- dplyr::bind_rows(adult_population, peds_population)
    
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
    
    return(ttr.01)
    
  } else if (
    any(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(arrest_table),
      is.null(disposition_table),
      is.null(response_table)
    ) &&
    
    !is.null(df)
    
  ) {
    
    # Start timing the function execution
    start_time <- Sys.time()
    
    # Header
    cli::cli_h1("TTR-01")
    
    # Header
    cli::cli_h2("Gathering Records for TTR-01")
    
    # Gather the population of interest
    ttr_01_populations <- ttr_01_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      earrest_01_col = {{ earrest_01_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_07_col = {{ evitals_07_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_23_col = {{ evitals_23_col }},
      evitals_26_col = {{ evitals_26_col }}
    )
    
    # Create a separator
    cli::cli_text("\n")
    
    # Header for calculations
    cli::cli_h2("Calculating TTR-01")
    
    # summarize
    
    # adults
    adult_population <- ttr_01_populations$adults |>
      summarize_measure(measure_name = "TTR_01",
                        population_name = "Adult",
                        VITALS,
                        ...)
    
    # peds
    peds_population <- ttr_01_populations$peds |>
      summarize_measure(measure_name = "TTR_01",
                        population_name = "Peds",
                        VITALS,
                        ...) 
    
    # summary
    ttr.01 <- dplyr::bind_rows(adult_population, peds_population)
    
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
    
    return(ttr.01)
    
  }
  
}

