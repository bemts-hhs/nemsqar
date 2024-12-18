#' Asthma-01
#'
#' Calculates the NEMSQA Asthma-01 measure.
#'
#' Calculates key statistics related to asthma-related incidents in an EMS dataset,
#' specifically focusing on cases where 911 was called for respiratory distress,
#' and certain medications were administered. This function segments the data by
#' age into adult and pediatric populations, computing the proportion of cases that
#' received beta-agonist treatment.
#'
#' @section Data Assumptions:
#'
#' This function assumes that:
#' Data are already loaded. The data need to be a data.frame or tibble.
#'
#' Age in years will be calculated using the patient date of birth and incident
#' date. These fields must have valid Date or POSIXct data types.
#'
#' When values are missing, they are coded as NA, not the "not known"/"not
#' recorded" values common to ImageTrend or the NEMSIS codes that correspond to
#' "not values".
#'
#' The primary and secondary impression fields (eSituation.11 and eSituation.12)
#' have the ICD-10 codes present in them. These fields may optionally contain
#' text for reference. Similarly, this function assumes that the eResponse.05
#' column has NEMSIS codes present, but text can also be included for reference.
#'
#' The eMedications.03 field contains all medications administered and contains
#' a text description of the medication using the generic name. The RxNORM code
#' may also be included for reference, but will not be checked.
#'
#' The secondary impressions field (eSituation.12) is best prepared as a
#' comma-separated list of all values in a single string.
#'
#' @section Practical Tips:
#'
#' The first argument is the dataframe or tables with data elements prepared as above. 
#' No joining is done. Any joins to get vitals, etc. will need to be done outside of this function.
#'
#' @section Features:
#' * Filters for asthma-related incidents (ICD-10 codes starting with 'J45' and
#' 'J98.01').
#' * Distinguishes between adults (age ≥ 18) and pediatric patients (age 2–17).
#' Calculates age in years based on incident date and patient date of birth.
#' Formats proportions as percentages with customizable decimal precision.
#'
#' @param df A data.frame or tibble containing EMS data where each row represents
#' an observation, and columns represent features.
#' @param patient_scene_table A data.frame or tibble containing at least epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing at least the eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing at least the esituation fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing at least the emedications fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column representing the EMS record unique identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's numeric age agnostic of unit.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's age unit ("Years", "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eResponse.05.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eSituation.11.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eSituation.12 values as a single comma-separated list.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eMedications.03 values as a single comma-separated list.
#' @param ... optional additional arguments to pass onto `dplyr::summarize`.
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
asthma_01 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      medications_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      emedications_03_col,
                      ...) {
  
  # ensure that not all table arguments AND the df argument are fulfilled
  # user only passes df or all table arguments
  if(
    
    any(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table), 
      !is.null(medications_table)
    ) 
    
    &&
    
    !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn asthma_01} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")
    
  }
  
  # ensure that df or all table arguments are fulfilled
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(medications_table)
    )
    
    && is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn asthma_01} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all three table arguments.")
    
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
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(emedications_03_col)
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn asthma_01}.")
    
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
    "Running `asthma_01()`",
    total = 2,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]\n"
  )
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table), 
        !is.null(response_table), 
        !is.null(situation_table), 
        !is.null(medications_table)
    ) && is.null(df)
    
  ) {
    
    # Ensure df is a data frame or tibble
    if (
      
      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||
          
          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) || 
          
          !(is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||
          
          !(is.data.frame(medications_table) && tibble::is_tibble(medications_table))
          
      )
      
    ) {
      
      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required for each of the *_table arguments."
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
      
      if ((!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_DOB)]]) &
           !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_DOB)]]))) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
        )
        
      }
    }
    
    # header
    cli::cli_h1("Asthma-01")
    
    # initiate the progress bar
    progress_bar_main

    # header
    cli::cli_h2("Gathering Records for Asthma-01")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    # gather the population of interest
    asthma_01_populations <- asthma_01_population(patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  situation_table = situation_table,
                                                  medications_table = medications_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  emedications_03_col = {{ emedications_03_col }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Asthma-01")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    asthma.01 <- results_summarize(total_population = asthma_01_populations$initial_population,
                                   adult_population = asthma_01_populations$adults,
                                   peds_population = asthma_01_populations$peds,
                                   measure_name = "Asthma-01",
                                   numerator_col = beta_agonist_check,
                                   ...)
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(asthma.01)
    
  } else if(all(is.null(patient_scene_table), is.null(response_table), is.null(situation_table), is.null(medications_table)) && !is.null(df)) 
    
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
    cli::cli_h1("Asthma-01")
    
    # initiate the progress bar
    progress_bar_main
    
    # header
    cli::cli_h2("Gathering Records for Asthma-01")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_main, force = T)
    
    # gather the population of interest
    asthma_01_populations <- asthma_01_population(df = df,
                                                  patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  situation_table = situation_table,
                                                  medications_table = medications_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  esituation_11_col = {{ esituation_11_col }},
                                                  esituation_12_col = {{ esituation_12_col }},
                                                  emedications_03_col = {{ emedications_03_col }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Asthma-01")
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_main, force = T)
    
    # summary
    asthma.01 <- results_summarize(total_population = asthma_01_populations$initial_population,
                                   adult_population = asthma_01_populations$adults,
                                   peds_population = asthma_01_populations$peds,
                                   measure_name = "Asthma-01",
                                   numerator_col = beta_agonist_check,
                                   ...)
    
    cli::cli_progress_done(id = progress_bar_main)
    
    # create a separator
    cli::cli_text("\n")
    
    return(asthma.01)
    
  }
  
}
