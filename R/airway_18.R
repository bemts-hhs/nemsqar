#' Airway-18
#' 
#' @description
#' 
#' This function processes and analyzes the dataset to calculate the "Airway-18" NEMSQA metric. 
#' It includes cleaning and transforming several columns related to patient data, airway procedures, 
#' and vital signs, and it returns a cleaned dataset with the relevant calculations.
#' The final calculation is an assessment of the successful last invasive airway procedures 
#' performed during an EMS response originating from a 911 request in which waveform capnography 
#' is used for tube placement confirmation.
#' 
#' @section Data Assumptions:
#' 
#' #' This function assumes that:
#' Data are already loaded. The data needs to be a data.frame or tibble.
#'
#' Age in years will be calculated using the patient date of birth and incident
#' date. These fields must have valid Date or POSIXct data types.
#'
#' When values are missing, they are coded as NA, not the "not known"/"not
#' recorded" values common to ImageTrend or the NEMSIS codes that correspond to
#' "not values".
#'
#' For the eprocedure, eairway, and evitals fields, all responses entered can be
#' included in `df`.  This will result in Cartesian product, but the function
#' will performantly handle that problem.
#' 
#' @section Practical Tips:
#' 
#' Ensure data are pre-processed, with missing values coded as `NA`, before passing
#' into the function.
#' Prepare necessary joins (e.g., for vitals) in advance; this function does not perform joins.
#'
#' @param df A data frame or tibble containing the dataset to be processed.
#' @param patient_scene_table A data frame or tibble containing only epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A data frame or tibble containing only the eprocedures fields needed for this measure's calculations. Default is `NULL`.
#' @param airway_table A data frame or tibble containing only the eairway fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the evitals fields needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column name containing the unique patient record identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name containing the incident date. Default is `NULL`.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name containing the patient's date of birth. Default is `NULL`.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name for patient information (exact purpose unclear).
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for patient information (exact purpose unclear).
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column name for procedure codes.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for emergency response codes.
#' @param eprocedures_06_col <['tidy-select'][dplyr_tidy_select]> Column name for procedure success codes.
#' @param eprocedures_05_col <['tidy-select'][dplyr_tidy_select]> Column name for number of procedure attempts.
#' @param eprocedures_01_col <['tidy-select'][dplyr_tidy_select]> Column name for procedure times or other related data.
#' @param eprocedures_02_col <['tidy-select'][dplyr_tidy_select]> Column name for additional procedure data.
#' @param eairway_04_col <['tidy-select'][dplyr_tidy_select]> Column name for airway procedure data.
#' @param eairway_02_col <['tidy-select'][dplyr_tidy_select]> Column name for airway procedure data (datetime).
#' @param evitals_01_col <['tidy-select'][dplyr_tidy_select]> Column name for vital signs data (datetime).
#' @param evitals_16_col <['tidy-select'][dplyr_tidy_select]> Column name for additional vital signs data.
#' @param ... Additional arguments passed to other functions if needed.
#'
#' @return A tibble summarizing results for Adults and Peds with the following columns:
#' `pop`: Population type (Adults, Peds).
#' `numerator`: Count of incidents where waveform capnography is used for tube placement confirmation on
#' the last successful invasive airway procedure.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents where waveform capnography is used for tube placement confirmation on
#' the last successful invasive airway procedure.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @note
#' This function filters and processes EMS data to:
#' - Identify the last successful airway procedure.
#' - Filter for 911 response codes and relevant vital signs (i.e. ETCO2).
#' - Aggregate results by patient encounter and calculate stroke scale outcomes.
#' - Return a summary of stroke cases by unique patient identifier, including stroke scale measurements.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
airway_18 <- function(df = NULL,
                      patient_scene_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      airway_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      eairway_02_col,
                      eairway_04_col,
                      evitals_01_col,
                      evitals_16_col,
                      ...) {
  
  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both
  
  if (
    any(
      !is.null(patient_scene_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(airway_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn airway_18} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }
  
  # Ensure that df or all table arguments are fulfilled
  
  if (
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(airway_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn airway_18} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }
  
  # Ensure all *_col arguments are fulfilled
  
  if (
    any(
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_DOB_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(eprocedures_01_col),
      missing(eprocedures_02_col),
      missing(eprocedures_03_col),
      missing(eprocedures_05_col),
      missing(eprocedures_06_col),
      missing(eairway_02_col),
      missing(eairway_04_col),
      missing(evitals_01_col),
      missing(evitals_16_col)
    )
  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn airway_18}.")
  }
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # header
  cli::cli_h1("Calculating Airway-18")
  
  # initiate the progress bar process
  progress_bar_main <- cli::cli_progress_bar(
    "Running `airway_18()`",
    total = 3,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(airway_table),
      !is.null(response_table)
    ) && is.null(df)
    
  ) {
    
    # Ensure all tables are of class `data.frame` or `tibble`
    if (
      
      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(procedures_table) || tibble::is_tibble(procedures_table),
        is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
        is.data.frame(airway_table) || tibble::is_tibble(airway_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table)
      )
      
    ) {
      
      cli::cli_abort(
        "One or more of the tables passed to {.fn airway_18_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
      )
      
    }
    
    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      incident_date <- rlang::enquo(incident_date_col)
      patient_DOB <- rlang::enquo(patient_DOB_col)
      
      if (
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_DOB)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_DOB)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }
    
    
    # Use quasiquotation on the vitals, airway, and procedures datetime fields
    airway_datetime <- rlang::enquo(eairway_02_col)
    vitals_datetime <- rlang::enquo(evitals_01_col)
    procedures_datetime <- rlang::enquo(eprocedures_01_col)
    
    # Validate the datetime fields in the patient_scene_table
    if ((!lubridate::is.Date(airway_table[[rlang::as_name(airway_datetime)]]) &
         !lubridate::is.POSIXct(airway_table[[rlang::as_name(airway_datetime)]])) ||
        (!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
         !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
        (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
         !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {
      
      cli::cli_abort(
        "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
      )
    }
    
    
    # header
    cli::cli_h1("Airway-18")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Airway-18")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    # gather the population of interest
    airway_18_populations <- airway_18_population(patient_scene_table = patient_scene_table,
                                                  procedures_table = procedures_table,
                                                  vitals_table = vitals_table,
                                                  airway_table = airway_table,
                                                  response_table = response_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_15_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eprocedures_01_col = {{ eprocedures_01_col }},
                                                  eprocedures_02_col = {{ eprocedures_02_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  eprocedures_05_col = {{ eprocedures_05_col }},
                                                  eprocedures_06_col = {{ eprocedures_06_col }},
                                                  eairway_02_col = {{ eairway_02_col }},
                                                  eairway_04_col = {{ eairway_04_col }},
                                                  evitals_01_col = {{ evitals_01_col }},
                                                  evitals_16_col = {{ evitals_16_col }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Airway-18")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    # adults
    adult_population <- airway_18_populations$adults |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Adult",
                        NUMERATOR,
                        ...)
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    # peds
    peds_population <- airway_18_populations$peds |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Peds",
                        NUMERATOR,
                        ...) 
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(airway.18)
    
  }else if(
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(airway_table),
      is.null(response_table)
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
    
    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      incident_date <- rlang::enquo(incident_date_col)
      patient_DOB <- rlang::enquo(patient_DOB_col)
      
      if (
        (!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }
    
      # Use quasiquotation on the vitals, airway, and procedures datetime fields
      airway_datetime <- rlang::enquo(eairway_02_col)
      vitals_datetime <- rlang::enquo(evitals_01_col)
      procedures_datetime <- rlang::enquo(eprocedures_01_col)
      
      # Validate the datetime fields in the df
      if ((!lubridate::is.Date(df[[rlang::as_name(airway_datetime)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(airway_datetime)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {
        
        cli::cli_abort(
          "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
        )
      }
  
    # header
    cli::cli_h1("Airway-18")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Airway-18")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    # gather the population of interest
    airway_18_populations <- airway_18_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_15_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  eprocedures_01_col = {{ eprocedures_01_col }},
                                                  eprocedures_02_col = {{ eprocedures_02_col }},
                                                  eprocedures_03_col = {{ eprocedures_03_col }},
                                                  eprocedures_05_col = {{ eprocedures_05_col }},
                                                  eprocedures_06_col = {{ eprocedures_06_col }},
                                                  eairway_02_col = {{ eairway_02_col }},
                                                  eairway_04_col = {{ eairway_04_col }},
                                                  evitals_01_col = {{ evitals_01_col }},
                                                  evitals_16_col = {{ evitals_16_col }}
    )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Airway-18")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    # adults
    adult_population <- airway_18_populations$adults |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Adult",
                        NUMERATOR,
                        ...)
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    # peds
    peds_population <- airway_18_populations$peds |>
      summarize_measure(measure_name = "Airway-18",
                        population_name = "Peds",
                        NUMERATOR,
                        ...) 
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(airway.18)
    
  }
  
}
