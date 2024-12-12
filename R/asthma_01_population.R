#' Asthma-01 Populations
#'
#' Filters data down to the target populations for Asthma-01, and categorizes 
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to asthma-related incidents in an EMS dataset,
#' specifically focusing on cases where 911 was called for respiratory distress,
#' and certain medications were administered. This function segments the data by
#' age into adult and pediatric populations, computing the proportion of cases that
#' received beta-agonist treatment.
#'
#' @section Data Assumptions:
#'
#' This function assumes that:
#' Data are already loaded. The data needs to be a data.frame or tibble where each row is
#' one observation (patient) and each is a feature (field) or distinct datasets
#' that can be references as unique columns.
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
#'
#' @param df A data.frame or tibble containing EMS data where each row represents
#' an observation, and columns represent features.
#' @param patient_scene_table A data.frame or tibble containing only epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations.
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
#'
#' @return
#' #' A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population 
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
asthma_01_population <- function(df = NULL,
                                 patient_scene_table = NULL,
                                 response_table = NULL,
                                 situation_table = NULL,
                                 medications_table = NULL,
                                 erecord_01_col,
                                 incident_date_col,
                                 patient_DOB_col,
                                 epatient_15_col,
                                 epatient_16_col,
                                 eresponse_05_col,
                                 esituation_11_col,
                                 esituation_12_col,
                                 emedications_03_col) {
  
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
    
    cli::cli_abort("{.fn asthma_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all four table arguments.")
    
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
    
    cli::cli_abort("{.fn asthma_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all four of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all three table arguments.")
    
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
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn asthma_01_population}.")
    
  }
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary/secondary impression fields
  beta_agonist <- "albuterol|ipratropium|levalbuterol|metaproterenol"
  
  # codes for asthma or acute bronchospasm
  asthma_codes <- "\\b(?:J45|J98.01)\\b"
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(!is.null(patient_scene_table), 
        !is.null(response_table), 
        !is.null(situation_table), 
        !is.null(medications_table)
    ) && is.null(df)
  ) {
    
    if(!(
      (is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||
      
      (is.data.frame(response_table) && tibble::is_tibble(response_table)) ||
      
      (is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||
      
      (is.data.frame(medications_table) && tibble::is_tibble(medications_table))
      
    )
    ) {
      
      cli::cli_abort("One or more of the tables passed to {.fn asthma_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables to {.fn asthma_01_population}, all tables must be of class {.cls data.frame} or {.cls tibble}.")
      
    }
    
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
    
    # options for the progress bar
    # a green dot for progress
    # a white line for note done yet
    options(cli.progress_bar_style = "dot")
    
    options(cli.progress_bar_style = list(
      complete = cli::col_green("●"),
      incomplete = cli::col_br_white("─")
    ))
    
    # initiate the progress bar process
    progress_bar_population <- cli::cli_progress_bar(
      "Running `asthma_01_population()`",
      total = 10,
      type = "tasks",
      clear = F,
      format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
    )
    
    progress_bar_population
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________
    
    # progress update, these will be repeated throughout the script
    
    # filter the table to get the initial population ages >= 2 years
    final_data <- patient_scene_table |>
      
      # create the age in years variable
      dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{  incident_date_col  }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,
      
      # system age check
      system_age_adult = {{  epatient_15_col }} >= 18 & {{ epatient_16_col  }} == "Years", 
      system_age_minor1 = ({{  epatient_15_col }} < 18 & {{  epatient_15_col }} >= 2) & {{ epatient_16_col  }} == "Years", 
      system_age_minor2 = {{ epatient_15_col}} >= 24 & {{epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2, 
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18, 
      calc_age_minor = patient_age_in_years_col < 18
      
      ) |> 
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T)
    
    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________
    
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
    
    # 911 calls
    call_911_data <- response_table |> 
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)
        
      ) |> 
      distinct({{ erecord_01_col }}) |> 
      pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    # Identify Records that have specified asthma
    asthma_data <- situation_table |> 
      dplyr::select({{ erecord_01_col }}, {{ esituation_11_col}}, {{esituation_12_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        if_any(
          
          c({{ esituation_11_col}}, {{esituation_12_col }}), ~ grepl(pattern = asthma_codes, x = ., ignore.case = T)
          
        )) |>
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
    
    # check to ensure beta agonist was used
    beta_agonist_data <- medications_table |> 
      dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        grepl(pattern = beta_agonist, x = {{ emedications_03_col }}, ignore.case = TRUE)
        
      ) |> 
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
    
    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |> 
      dplyr::mutate(call_911 = {{ erecord_01_col }} %in% call_911_data,
                    asthma = {{ erecord_01_col }} %in% asthma_data,
                    beta_agonist_check = {{ erecord_01_col }} %in% beta_agonist_data
      )
    
    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
    
    # get the initial population
    initial_population <- computing_population |> 
      dplyr::filter(
        
        
        # asthma patients
        asthma,
        
        # 911 calls
        call_911
        
      )
    
    # Adult and Pediatric Populations
    
    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(calc_age_adult | system_age_adult)
    
    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor)
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls", 
                 "Asthma cases", 
                 "Beta agonist cases",
                 "Adults",
                 "Peds", 
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$call_911, na.rm = T),
        sum(computing_population$asthma, na.rm = T),
        sum(computing_population$beta_agonist_check, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )
    
    # get the populations of interest
    
    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
    
    # gather data into a list for multi-use output
    asthma.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )
    
    cli::cli_progress_done(id = progress_bar_population)
    
    return(asthma.01.population)
    
    
  } else if(
    
    all(is.null(patient_scene_table), is.null(response_table), is.null(situation_table), is.null(medications_table)) && !is.null(df)
    
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
    
    # options for the progress bar
    # a green dot for progress
    # a white line for note done yet
    options(cli.progress_bar_style = "dot")
    
    options(cli.progress_bar_style = list(
      complete = cli::col_green("●"),
      incomplete = cli::col_br_white("─")
    ))
    
    # initiate the progress bar process
    progress_bar_population <- cli::cli_progress_bar(
      "Running `asthma_01_population()`",
      total = 10,
      type = "tasks",
      clear = F,
      format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
    )
    
    progress_bar_population
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    # from the full dataframe with all variables
    # create one fact table and several dimension tables
    # to complete calculations and avoid issues due to row
    # explosion
    ###_____________________________________________________________________________
    
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    
    # filter the table to get the total dataset with identified categories
    final_data <- df |>
      dplyr::select(-c({{  eresponse_05_col  }},
                       {{  esituation_11_col  }},
                       {{  esituation_12_col }},
                       {{  emedications_03_col  }}

      )) |> 
      distinct({{ erecord_01_col }}, .keep_all = T) |> 
      # create the age in years variable
      
      dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{  incident_date_col  }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,
      
      # system age check
      system_age_adult = {{  epatient_15_col }} >= 18 & {{ epatient_16_col  }} == "Years", 
      system_age_minor1 = ({{  epatient_15_col }} < 18 & {{  epatient_15_col }} >= 2) & {{ epatient_16_col  }} == "Years", 
      system_age_minor2 = {{ epatient_15_col}} >= 24 & {{epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2, 
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18, 
      calc_age_minor = patient_age_in_years_col < 18
      
      )
    
    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________
    
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
    
    # 911 calls
    call_911_data <- df |> 
      dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = T)) |> 
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    # asthma population
    asthma_data <- df |> 
      dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}, {{ esituation_12_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        # Identify Records that have specified asthma
        dplyr::if_any(c({{ esituation_11_col}}, {{esituation_12_col }}), ~ grepl(pattern = asthma_codes, x = .))
        
        ) |>
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
    
    # beta agonist use
    beta_agonist_data <- df |> 
      dplyr::select({{ erecord_01_col }}, {{  emedications_03_col  }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        # check to ensure beta agonist was used
        grepl(pattern = beta_agonist, x = {{ emedications_03_col }}, ignore.case = TRUE)
        
        ) |> 
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
    
    # get the computing population that is the full dataset with identified categories
    computing_population <- final_data |> 
      dplyr::mutate(call_911 = {{ erecord_01_col }} %in% call_911_data,
                    asthma = {{ erecord_01_col }} %in% asthma_data,
                    beta_agonist_check = {{ erecord_01_col }} %in% beta_agonist_data
      )
    
    cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
    
    # get the initial population
    initial_population <- computing_population |> 
      dplyr::filter(
        
        
        # asthma patients
        asthma,
        
        # 911 calls
        call_911
        
      )
    
    # Adult and Pediatric Populations
    
    cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(calc_age_adult | system_age_adult)
    
    cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor)
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls", 
                 "Asthma cases", 
                 "Beta agonist cases",
                 "Adults",
                 "Peds", 
                 "Initial population",
                 "Total dataset"
      ),
      count = c(
        length(call_911_data),
        length(asthma_data),
        length(beta_agonist_data),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )
    
    # get the populations of interest
    
    cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
    
    # gather data into a list for multi-use output
    asthma.01.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )
    
    cli::cli_progress_done(id = progress_bar_population)
    
    return(asthma.01.population)
    
  }
  
}
