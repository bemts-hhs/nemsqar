#' Respiratory-02 Calculation
#'
#' The `respiratory_02` function calculates metrics for pediatric and adult
#' respiratory populations based on pre-defined criteria, such as low oxygen
#' saturation and specific medication or procedure codes. It returns a summary
#' table of the overall, pediatric, and adult populations, showing counts and
#' proportions.
#'
#' @section Data Assumptions:
#' Assume data are already loaded The data must be a dataframe or tibble that
#' contains emedications.03 and eprocedures.03 as columns where each cell
#' contains all values entered for each respective incident. These are not list
#' columns but can be comma separated values in each cell, and must contain all
#' medications for each incident. The table must also have the erecord.01 values
#' to attempt a best estimate of a unique ID by concatenating the erecord.01,
#' incident date, and patient DOB. The table can include all values entered for
#' evitals.12 in the table.  This will cause row explosion, but the function
#' will find if the patient had sp02 < 90 among any values entered. This
#' function assumes you have the following fields: eRecord.01, eresponse.05
#' (must contain code), evitals.12 (all values), emedications.03 (all with
#' code), eprocedures.03 (with code) this function will calculate age in years
#' for the end user. this function also assumes that rows that are missing any
#' value are NA, not the not known / not recorded values common to ImageTrend or
#' the value codes that correspond to "not values". the first argument is a
#' dataframe, no joining is done in the function. any needed table joins must be
#' done prior to running the function to ensure required columns are present.
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#'
#' @param df A data frame containing incident data with each row representing an observation.
#' @param patient_scene_table A data.frame or tibble containing at least epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing at least the evitals fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing only the eprocedures fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name for eRecord.01, used to form a unique patient ID.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> integer Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for response codes (e.g., incident type).
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name for oxygen saturation (SpO2) values.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column name for medication codes.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column name for procedure codes.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return Returns a tibble summarizing the overall and age-grouped respiratory-02 metrics, formatted for ease of interpretation.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
respiratory_02 <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           vitals_table = NULL,
                           medications_table = NULL,
                           procedures_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col,
                           ...) {
  
  # ensure that not all table arguments AND the df argument are fulfilled
  # user only passes df or all table arguments
  if(
    
    any(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) 
    
    &&
    
    !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn respiratory_02} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
  }
  
  # ensure that df or all table arguments are fulfilled
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table)
    )
    
    && is.null(df)
  ) {
    
    cli::cli_abort("{.fn respiratory_02} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
  }
  
  # ensure all *_col arguments are fulfilled
  if(
    
    any(
      
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(evitals_12_col),
      missing(emedications_03_col),
      missing(eprocedures_03_col)
      
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn respiratory_02}.")
    
  }
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_main_style = "dot")
  
  options(cli.progress_bar_main_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # initiate the progress bar process
  progress_bar_main <- cli::cli_progress_bar(
    "Running `respiratory_02()`",
    total = 12,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) && is.null(df)
    
  ) {
    
    # Ensure df is a data frame or tibble
    if (
      
      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||
          
          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) || 
          
          !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table)) ||
          
          !(is.data.frame(medications_table) && tibble::is_tibble(medications_table)) ||
          
          !(is.data.frame(procedures_table) && tibble::is_tibble(procedures_table))
          
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
           !lubridate::is.POSIXct(patient_scene_table[[patient_DOB_name]]))
          
          ) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or a similar class."
        )
      }
      
    }
      
  # header
  cli::cli_h1("Respiratory-02")
  
  # initiate the progress bar
  progress_bar_main
  
  # header
  cli::cli_h2("Gathering Records for Respiratory-02")
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
  
  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           patient_scene_table = patient_scene_table,
                           response_table = response_table,
                           vitals_table = vitals_table,
                           procedures_table = procedures_table,
                           medications_table = medications_table,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )
  
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Respiratory-02")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                   adult_population = respiratory_02_populations$adults,
                                   peds_population = respiratory_02_populations$peds,
                                   measure_name = "Respiratory-02",
                                   numerator_col = OXYGEN,
                                   ...)
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(respiratory.02)
    
    
    } else if(
      
        all(
          is.null(patient_scene_table), 
          is.null(response_table), 
          is.null(vitals_table), 
          is.null(medications_table),
          is.null(procedures_table)
        ) && !is.null(df)
        
    # utilize a dataframe to analyze the data for the measure analytics

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
               !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))
              
              ) {
            
            cli::cli_abort(
              "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
            )
            
          }
        }
        
        # header
  cli::cli_h1("Respiratory-02")
  
  # initiate the progress bar
  progress_bar_main
  
  # header
  cli::cli_h2("Gathering Records for Respiratory-02")
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
  
  # gather the population of interest
  respiratory_02_populations <- respiratory_02_population(
                           df = df,
                           erecord_01_col = {{ erecord_01_col }},
                           incident_date_col = {{ incident_date_col }},
                           patient_DOB_col = {{ patient_DOB_col}},
                           epatient_15_col = {{ epatient_15_col}},
                           epatient_16_col = {{ epatient_16_col }},
                           eresponse_05_col = {{ eresponse_05_col }},
                           evitals_12_col = {{ evitals_12_col }},
                           emedications_03_col = {{ emedications_03_col }},
                           eprocedures_03_col = {{ eprocedures_03_col }}
                           )
  
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Respiratory-02")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    respiratory.02 <- results_summarize(total_population = respiratory_02_populations$initial_population,
                                   adult_population = respiratory_02_populations$adults,
                                   peds_population = respiratory_02_populations$peds,
                                   measure_name = "Respiratory-02",
                                   numerator_col = OXYGEN,
                                   ...)
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(respiratory.02)
      
    }
        
}
