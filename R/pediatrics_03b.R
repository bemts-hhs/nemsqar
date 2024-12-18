#' Pediatrics-03B Measure Calculation
#'
#' The `pediatrics_03b` function calculates a pediatric metric focused on EMS
#' responses, specifically targeting responses that involve patients under 18
#' years of age, where certain weight-based medications were administered. This
#' function filters EMS data to identify relevant 911 responses and further narrows
#' down the dataset to cases involving children, calculating the proportion of
#' cases with documented weight among those where weight-based medications were
#' administered.
#'
#' @section Data Assumptions:
#' Assume data are already loaded
#' The data must be a dataframe or tibble that contains emedications.03
#' as a column where each cell contains all values entered for each respective incident.
#' This is not a list column but can be comma separated values in each cell, and must contain
#' all medications for each incident.
#' emedications.04 is the full list of medication administration routes present in the data
#' can the function will roll up these values so that the table is distinct with each row being
#' 1 observation, and each column a feature.
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that edisposition.18 is a list column or a column that has all
#' text descriptors for additional transport mode descriptors.  These can be separated
#' by commas or other characters as long as all eresponse.18 values are present
#' in one cell for each unique erecord.01 value.  Codes can be present
#' but will be ignored by the function.
#' for the argument transport_disposition_cols, this argument can receive the unquoted
#' column names of edisposition.12 and edisposition.30.  One or both can be entered and
#' the function will evaluate them.  These columns are used to create a `transport`
#' variable that is used to filter the table down furhter.  AS such, these columns
#' edisposition.12 and edisposition.30 must be list columns that and/or contain all values
#' from each unique incident entered for each field.  These can be comma separated values
#' all in one cell to make the table tidy.
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#'
#' @param df A data frame or tibble containing emergency response records.
#' @param patient_scene_table A data.frame or tibble containing only epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse fields needed for this measure's calculations.
#' @param exam_table A data.frame or tibble containing only the eexam fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column for unique EMS record identifiers.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing the EMS response codes.
#' @param eexam_01_col <['tidy-select'][dplyr_tidy_select]> Column containing documented weight information.
#' @param eexam_02_col <['tidy-select'][dplyr_tidy_select]> Another column for weight documentation, if applicable.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column indicating medication administration.
#' @param emedications_04_col <['tidy-select'][dplyr_tidy_select]> Column listing medications administered.
#' @param ... Additional parameters for the `dplyr::summarize` output.
#'
#' @return A tibble summarizing results for three population groups (All, Adults, and Peds) with the following columns:
#' 
#' `pop`: Population type (All, Adults, Peds).
#' `numerator`: Count of incidents where patient weight was documented.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where patient weight was documented.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
pediatrics_03b <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           exam_table = NULL,
                           medications_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
                           ...) {
  
  if(
    
    any(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(exam_table),
      !is.null(medications_table)
    ) 
    
    &&
    
    !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn pediatrics_03b_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all three of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all three table arguments.")
    
  }
  
  # ensure all *_col arguments are fulfilled
  if(
    
    any(
      
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(eexam_01_col),
      missing(eexam_02_col),
      missing(emedications_03_col),
      missing(emedications_04_col)
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn pediatrics_03b_population}.")
    
  }
  
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(exam_table),
      is.null(medications_table)
    )
    
    && is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn pediatrics_03b_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all six of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all six table arguments.")
    
  }
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  progress_bar_main <- cli::cli_progress_bar(
    "Running `pediatrics_03b()`",
    total = 2,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(exam_table),
      !is.null(medications_table)
    ) 
    
    && is.null(df)
    
  ) {
    
    # Ensure df is a data frame or tibble
    if (
      
      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||
          
          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) || 
          
          !(is.data.frame(exam_table) && tibble::is_tibble(exam_table)) ||
          
          !(is.data.frame(medications_table) && tibble::is_tibble(medications_table))
          
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
  cli::cli_h1("Pediatrics-03b")
  
  # initiate the progress bar
  progress_bar_main
  
  # header
  cli::cli_h2("Gathering Records for Pediatrics-03b")
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
  
  # gather the population of interest
  pediatrics03b_populations <- pediatrics_03b_population(patient_scene_table = patient_scene_table,
                                                         response_table = response_table,
                                                         exam_table = exam_table,
                                                         medications_table = medications_table,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")
  
  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
  
  # summary
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                 measure_name = "Pediatrics-03b",
                                 population_name = "Peds",
                                 numerator_col = DOCUMENTED_WEIGHT,
                                 ...)
  
  cli::cli_progress_done(id = progress_bar_main)
  
  # create a separator
  cli::cli_text("\n")
  
  return(pediatrics.03b)
  
  }else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(exam_table),
      is.null(medications_table)
    )
    
    && !is.null(df)
    
  ) 
  
  # utilize a dataframe to analyze the data for the measure analytics
  
  {
    
    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !tibble::is_tibble(df)) {
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
          "i" = "The passed object is of class {.val {class(df)}}."
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
      
      if ((!lubridate::is.Date(df[[incident_date_name]]) &
           !lubridate::is.POSIXct(df[[incident_date_name]])) ||
          (!lubridate::is.Date(df[[patient_DOB_name]]) &
           !lubridate::is.POSIXct(df[[patient_DOB_name]]))) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or a similar class."
        )
      }
    }
  
    # header
    cli::cli_h1("Pediatrics-03b")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Pediatrics-03b")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
  pediatrics03b_populations <- pediatrics_03b_population(df = df,
                                                         erecord_01_col = {{ erecord_01_col }},
                                                         incident_date_col = {{ incident_date_col }},
                                                         patient_DOB_col = {{ patient_DOB_col }},
                                                         epatient_15_col = {{ epatient_15_col }},
                                                         epatient_16_col = {{ epatient_16_col }},
                                                         eresponse_05_col = {{ eresponse_05_col }},
                                                         eexam_01_col = {{ eexam_01_col }},
                                                         eexam_02_col = {{ eexam_02_col }},
                                                         emedications_03_col = {{ emedications_03_col }},
                                                         emedications_04_col = {{ emedications_04_col }}
                                                         )

  # create a separator
  cli::cli_text("\n")
  
  # header for calculations
  cli::cli_h2("Calculating Pediatrics-03b")
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
  
  # summary
  pediatrics.03b <- summarize_measure(data = pediatrics03b_populations$initial_population,
                                      measure_name = "Pediatrics-03b",
                                      population_name = "Peds",
                                      numerator_col = DOCUMENTED_WEIGHT,
                                      ...)
  
  cli::cli_progress_done(id = progress_bar_main)
  
  # create a separator
  cli::cli_text("\n")
  
  return(pediatrics.03b)
  
  }
  
}
