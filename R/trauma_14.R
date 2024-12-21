#' Trauma-14
#'
#' This function processes EMS data to generate a set of binary variables indicating whether specific trauma triage criteria are met. The output #' is a data frame enriched with these indicators for further analysis.  The final outcome is whether or not the EMS record documents the use of #' a pre-hospital trauma activation.
#' 
#' @section Data Assumptions:
#' 
#' - All vitals columns contain the full list of values entered for each record and columns.
#' - Each eexam field is a list column containing all values entered per record
#' that are also comma (or otherwise) separated.
#' - The trauma triage fields are list columns containing all values entered per record
#' that are also comma (or otherwise) separated.
#' - `einjury_01_col` is a list column containing all values entered per record
#' that are also comma (or otherwise) separated.
#' - `eresponse_10_col` is a list column containing all values entered per record
#' that are also comma (or otherwise) separated.
#' - `eprocedures_03_col` is a list column containing all values entered per record
#' that are also comma (or otherwise) separated.
#'
#' @param df A data frame or tibble containing EMS data with all relevant columns.
#' @param patient_scene_table A data frame or tibble containing fields from epatient and escene needed for this measure's calculations.
#' @param situation_table A data frame or tibble containing fields from esituation needed for this measure's calculations.
#' @param response_table A data frame or tibble containing fields from eresponse needed for this measure's calculations.
#' @param disposition_table A data frame or tibble containing fields from edisposition needed for this measure's calculations.
#' @param vitals_table A data frame or tibble containing fields from evitals needed for this measure's calculations.
#' @param exam_table A data frame or tibble containing fields from eexam needed for this measure's calculations.
#' @param procedures_table A data frame or tibble containing fields from eprocedures needed for this measure's calculations.
#' @param injury_table A data frame or tibble containing fields from einjury needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column representing the EMS record unique identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> The column indicating the incident date. Must be of class `Date` or similar.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> The column representing the patient's date of birth. Must be of class `Date` or similar.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> The column for patient age numeric value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> The column for patient age unit (e.g., "Years", "Months").
#' @param esituation_02_col <['tidy-select'][dplyr_tidy_select]> The column containing information on the presence of injury.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> The column representing the 911 response type.
#' @param eresponse_10_col <['tidy-select'][dplyr_tidy_select]> Column name containing scene delay information.
#' @param transport_disposition_col <['tidy-select'][dplyr_tidy_select]> The column for patient transport disposition.
#' @param edisposition_23_col <['tidy-select'][dplyr_tidy_select]> Column name containing pre-hospital trauma alert information.
#' @param evitals_06_col <['tidy-select'][dplyr_tidy_select]> Column name containing systolic blood pressure (SBP) values.
#' @param evitals_10_col <['tidy-select'][dplyr_tidy_select]> Column name containing heart rate values.
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name containing pulse oximetry values.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> Column name containing capillary refill information.
#' @param evitals_15_col <['tidy-select'][dplyr_tidy_select]> Column name containing respiratory effort values.
#' @param evitals_21_col <['tidy-select'][dplyr_tidy_select]> Column name containing Glasgow Coma Scale (GCS) Motor values.
#' @param eexam_16_col <['tidy-select'][dplyr_tidy_select]> Column name containing extremities assessment details.
#' @param eexam_20_col <['tidy-select'][dplyr_tidy_select]> Column name containing neurological assessment details.
#' @param eexam_23_col <['tidy-select'][dplyr_tidy_select]> Column name containing lung assessment details.
#' @param eexam_25_col <['tidy-select'][dplyr_tidy_select]> Column name containing chest assessment details.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column name containing airway management or tourniquet usage details.
#' @param einjury_01_col <['tidy-select'][dplyr_tidy_select]> Column name containing injury cause details.
#' @param einjury_03_col <['tidy-select'][dplyr_tidy_select]> Column name containing trauma triage steps 1 and 2 information.
#' @param einjury_04_col <['tidy-select'][dplyr_tidy_select]> Column name containing trauma triage steps 3 and 4 information.
#' @param einjury_09_col <['tidy-select'][dplyr_tidy_select]> Column name containing fall height information.
#' @param ... Additional arguments passed to helper functions for further customization.
#'
#' @section Features: 
#' 
#' - Handles missing or invalid date formats with error messaging.
#' - Incorporates quasiquotation for flexible column referencing.
#' - Creates reusable dimension tables for efficient filtering and summarization.
#'
#' @return A tibble summarizing results for three age groups (< 10 yrs, 10–65 yrs, and >= 65 yrs) with the following columns:
#'
#' `pop`: Population type (< 10 yrs, 10–65 yrs, >= 65 yrs).
#' `numerator`: Count of incidents where a pre-hospital trauma alert was called.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where a pre-hospital trauma alert was called.
#' `prop_label`: Proportion formatted as a percentage with a specified number of decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
trauma_14 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      medications_table = NULL,
                      vitals_table = NULL,
                      exam_table = NULL,
                      procedures_table = NULL,
                      injury_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      eresponse_10_col,
                      transport_disposition_col,
                      edisposition_23_col,
                      evitals_06_col,
                      evitals_10_col,
                      evitals_12_col,
                      evitals_14_col,
                      evitals_15_col,
                      evitals_21_col,
                      eexam_16_col,
                      eexam_20_col,
                      eexam_23_col,
                      eexam_25_col,
                      eprocedures_03_col,
                      einjury_01_col,
                      einjury_03_col,
                      einjury_04_col,
                      einjury_09_col,
                      ...) {
  
  
  # ensure that not all table arguments AND the df argument are fulfilled
  # user only passes df or all table arguments
  if(
    
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table),
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) && !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn trauma_14} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
  }
  
  # ensure that df or all table arguments are fulfilled
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) && is.null(df)
  ) {
    
    cli::cli_abort("{.fn trauma_14} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
  }
  
  # ensure all *_col arguments are fulfilled
  if(
    
    any(
      
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(esituation_02_col),
      missing(eresponse_05_col),
      missing(eresponse_10_col),
      missing(transport_disposition_col),
      missing(edisposition_23_col),
      missing(evitals_06_col),
      missing(evitals_10_col),
      missing(evitals_12_col),
      missing(evitals_14_col),
      missing(evitals_15_col),
      missing(evitals_21_col),
      missing(eexam_16_col),
      missing(eexam_20_col),
      missing(eexam_23_col),
      missing(eexam_25_col),
      missing(eprocedures_03_col),
      missing(einjury_01_col),
      missing(einjury_03_col),
      missing(einjury_04_col),
      missing(einjury_09_col)
      
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing. Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn trauma_14}.")
    
  }
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # initiate the progress bar process
  progress_bar_main <- cli::cli_progress_bar(
    "Running `trauma_14()`",
    total = 5,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table),
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) && is.null(df)
    
  ) {
    
    # Ensure df is a data frame or tibble
    if (
      
      any(
        !(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||
        
        !(is.data.frame(response_table) && tibble::is_tibble(response_table)) || 
        
        !(is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||
        
        !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table)) ||
        
        !(is.data.frame(medications_table) && tibble::is_tibble(medications_table)) ||
        
        !(is.data.frame(procedures_table) && tibble::is_tibble(procedures_table)) ||
        
        !(is.data.frame(exam_table) && tibble::is_tibble(exam_table)) ||
        
        !(is.data.frame(injury_table) && tibble::is_tibble(injury_table)) ||
        
        !(is.data.frame(disposition_table) && tibble::is_tibble(disposition_table))
        
      )
      
    ) {
      
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required for each of the *_table arguments."
        )
      )
    }
    
    # Only check the date columns if they are in fact passed
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      # Use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_DOB <- rlang::enquo(patient_DOB_col)
      
      # Convert quosures to names and check the column classes
      incident_date_name <- rlang::as_name(incident_date)
      patient_DOB_name <- rlang::as_name(patient_DOB)
      
      if ((!lubridate::is.Date(patient_scene_table[[incident_date_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[incident_date_name]])) ||
          (!lubridate::is.Date(patient_scene_table[[patient_DOB_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[patient_DOB_name]]))) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or a similar class."
        )
      }
    }
    
    # header
    cli::cli_h1("Trauma-14")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Trauma-14")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    trauma_14_populations <- trauma_14_population(
      
      patient_scene_table = patient_scene_table,
      response_table = response_table,
      situation_table = situation_table,
      medications_table = medications_table,
      vitals_table = vitals_table,
      exam_table = exam_table,
      procedures_table = procedures_table,
      injury_table = injury_table,
      disposition_table = disposition_table,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_23_col = {{ edisposition_23_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}
      
    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Trauma-14")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    population_65 <- trauma_14_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = ">= 65 yrs",
        numerator_col = TRAUMA_ALERT_65,
        ...
      )
    
    cli::cli_progress_update(set = 3, id = progress_bar_main, force = T)
    
    # 10 to 64 population
    population_10_65 <- trauma_14_populations$population_10_64 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "10-64 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )
    
    cli::cli_progress_update(set = 4, id = progress_bar_main, force = T)
    
    # patients < 10 yrs
    population_10 <- trauma_14_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "< 10 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )
    
    cli::cli_progress_update(set = 5, id = progress_bar_main, force = T)
    
    # summary
    trauma.04 <- dplyr::bind_rows(population_65, population_10_65, population_10)
    
    cli::cli_progress_done()
    
    # create a separator
    cli::cli_text("\n")
    
    return(trauma.04)
    
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) && !is.null(df)
    
  ) {
    
    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
          "i" = "The passed object is of class {.val {class(df)}}."
        )
      )
    }
    
    # only check the date columns if they are in fact passed
    if(
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) 
      
    {
      
      # use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_DOB <- rlang::enquo(patient_DOB_col)
      
      if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
        )
        
      }
    }
    
    # header
    cli::cli_h1("Trauma-14")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Trauma-14")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    trauma_14_populations <- trauma_14_population(
      df = df,
      erecord_01_col = {{ erecord_01_col }},
      incident_date_col = {{ incident_date_col }},
      patient_DOB_col = {{ patient_DOB_col }},
      epatient_15_col = {{ epatient_15_col }},
      epatient_16_col = {{ epatient_16_col }},
      esituation_02_col = {{ esituation_02_col }},
      eresponse_05_col = {{ eresponse_05_col }},
      eresponse_10_col = {{ eresponse_10_col }},
      transport_disposition_col = {{ transport_disposition_col }},
      edisposition_23_col = {{ edisposition_23_col }},
      evitals_06_col = {{ evitals_06_col }},
      evitals_10_col = {{ evitals_10_col }},
      evitals_12_col = {{ evitals_12_col }},
      evitals_14_col = {{ evitals_14_col }},
      evitals_15_col = {{ evitals_15_col }},
      evitals_21_col = {{ evitals_21_col }},
      eexam_16_col = {{ eexam_16_col }},
      eexam_20_col = {{ eexam_20_col }},
      eexam_23_col = {{ eexam_23_col }},
      eexam_25_col = {{ eexam_25_col }},
      eprocedures_03_col = {{ eprocedures_03_col }},
      einjury_01_col = {{ einjury_01_col }},
      einjury_03_col = {{ einjury_03_col }},
      einjury_04_col = {{ einjury_04_col }},
      einjury_09_col = {{ einjury_09_col }}
      
    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Trauma-14")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    population_65 <- trauma_14_populations$population_65 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = ">= 65 yrs",
        numerator_col = TRAUMA_ALERT_65,
        ...
      )
    
    cli::cli_progress_update(set = 3, id = progress_bar_main, force = T)
    
    # 10 to 64 population
    population_10_65 <- trauma_14_populations$population_10_65 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "10-64 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )
    
    cli::cli_progress_update(set = 4, id = progress_bar_main, force = T)
    
    # patients < 10 yrs
    population_10 <- trauma_14_populations$population_10 |>
      summarize_measure(
        measure_name = "Trauma-14",
        population_name = "< 10 yrs",
        numerator_col = TRAUMA_ALERT_10_64,
        ...
      )
    
    cli::cli_progress_update(set = 5, id = progress_bar_main, force = T)
    
    # summary
    trauma.04 <- dplyr::bind_rows(population_65, population_10_65, population_10)
    
    cli::cli_progress_done()
    
    # create a separator
    cli::cli_text("\n")
    
    return(trauma.04)
    
  }
  
}
