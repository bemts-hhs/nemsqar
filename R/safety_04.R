#' Safety-04 Calculation
#'
#' The `safety_04` function processes EMS incident data for specific safety and transport criteria, filtering by patient age and incident type to identify cases that meet specified exclusion or inclusion criteria. This function accommodates data with various EMS-specific codes, age descriptors, and procedure identifiers.
#' 
#' @section Data Assumptions:
#' 
#' This function assumes that:
#' 
#' Data are already loaded into a data frame or tibble where each row represents
#' one observation (e.g., patient) and each column is a distinct feature (field).
#' Alternatively, data may consist of separate datasets referenced by unique columns.
#' 
#' Patient age in years will be calculated using `incident_date_col` and `patient_DOB_col`.
#' Both of these fields must be valid `Date` or `POSIXct` data types.
#' 
#' Missing values in rows are represented as `NA`. "Not known" or "not recorded" values,
#' common to ImageTrend or other non-numeric codes, should be pre-cleaned before use.
#' 
#' The `eresponse_05_col` contains NEMSIS response codes and may also include
#' descriptive text for reference.
#' 
#' The `edisposition_14_col` is expected to contain transport mode descriptors, which
#' may be provided as comma-separated text within each cell, or as a list column.
#' 
#' The `earrest_01_col` includes cardiac arrest information as text and code; typically
#' only one response per patient encounter is recorded.
#' 
#' The `eprocedures_03_col` contains all procedures as a list or a text column with 
#' multiple entries separated by commas or other delimiters. Procedure codes may be present
#' but will not affect function output.
#' 
#' The `einjury_03_col` should include all trauma center triage criteria. This function will
#' detect duplicate entries and filter on unique values per patient encounter.
#' 
#' The `edisposition_12` and `edisposition_30` fields should be list columns or text fields
#' with all relevant transport disposition entries provided in a single cell as comma-separated
#' values for each unique incident.
#' 
#' The first argument to this function is the main data frame. No joins are performed within
#' the function; any necessary joins (e.g., to incorporate vitals or additional fields) should
#' be completed prior to calling this function.
#' 
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @section Practical Tips:
#' 
#' Ensure data are pre-processed, with missing values coded as `NA`, before passing
#' into the function.
#' Prepare necessary joins (e.g., for vitals) in advance; this function does not perform joins.
#' 
#' @section Value:
#' 
#' #' A summarized tibble with counts and proportions of pediatric patients ages <8 yrs
#' using a pediatric restraing device among 911 requests or interfacility request.
#'
#' @param df <['tidy-select'][dplyr_tidy_select]> A data frame or tibble containing EMS data where each row represents an individual observation.
#' @param patient_scene_table A data frame or tibble containing fields from epatient and escene needed for this measure's calculations.
#' @param response_table A data frame or tibble containing fields from eresponse needed for this measure's calculations.
#' @param arrest_table A data frame or tibble containing fields from earrest needed for this measure's calculations.
#' @param injury_table A data frame or tibble containing fields from einjury needed for this measure's calculations.
#' @param procedures_table A data frame or tibble containing fields from eprocedures needed for this measure's calculations.
#' @param disposition_table A data frame or tibble containing fields from edisposition needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name containing the incident dates, expected to be of Date or POSIXct class.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name containing patient birth dates, expected to be of Date or POSIXct class.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name indicating the patient age.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for the unit of age (e.g., "Years," "Months").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing response transport codes.
#' @param earrest_01_col <['tidy-select'][dplyr_tidy_select]> Column with cardiac arrest status information.
#' @param einjury_03_col <['tidy-select'][dplyr_tidy_select]> Column describing traumatic injuries, expected as a list or text-separated entries.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column listing procedures, assumed to contain multiple procedure codes/texts in each cell.
#' @param edisposition_14_col <['tidy-select'][dplyr_tidy_select]> Column for transport dispositions.
#' @param transport_disposition_cols <['tidy-select'][dplyr_tidy_select]> Columns for primary and secondary transport dispositions.
#' @param ... Additional arguments for flexibility in function customization.
#'
#' @section Features:
#' * **Age Calculation**: Calculates patient age in years based on `incident_date_col` and `patient_DOB_col`.
#' * **Transport Identification**: Flags records for EMS transport and interfacility transports based on provided codes.
#' * **Cardiac Arrest & Trauma Triage Filtering**: Identifies cases with severe trauma indicators or cardiac arrest.
#' * **Procedure Exclusion**: Applies exclusion criteria based on specific airway and immobilization procedures.
#' * **Minor Age Check**: Separates pediatric cases (<8 years) by EMS-reported or calculated age.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
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
                      patient_DOB_col = NULL,
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
                                                  patient_DOB_col = {{ patient_DOB_col }},
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
                                                  patient_DOB_col = {{ patient_DOB_col }},
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
