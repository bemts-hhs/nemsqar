#' Respiratory-02 Populations
#'
#' The `respiratory_02_population` function calculates metrics for pediatric and adult
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
respiratory_02_population <- function(df = NULL,
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
                           eprocedures_03_col
                           ) {
  
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
    
    cli::cli_abort("{.fn respiratory_02_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
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
    
    cli::cli_abort("{.fn respiratory_02_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
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
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn respiratory_02_population}.")
    
  }
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  # oxygen
  oxygen_values <- "7806|Oxygen"
  
  # oxygen therapy
  oxygen_therapy_values <- "57485005|Oxygen Therapy"
  
  # not values for meds
  not_med <- "8801001|8801003|8801009|8801019|8801027|Contraindication Noted|Denied by Order|Medication Already Taken|Refused|Order Criteria Not Met"
  
  # not values for procedures
  not_proc <- "8801001|8801023|8801003|8801027|8801019|Contraindicated Noted|Unable to Complete|Denied By Order|Order Criteria Not Met|Refused"
  
  # days, hours, minutes, months
  
  minor_values <- "days|2516001|hours|2516003|minutes|2516005|months|2516007"
  
  year_values <- "2516009|years"
  
  day_values <- "days|2516001"
  
  hour_values <- "hours|2516003"
  
  minute_values <- "minutes|2516005"
  
  month_values <- "months|2516007"
  
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
    "Running `respiratory_02_population()`",
    total = 11,
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
           !lubridate::is.POSIXct(patient_scene_table[[patient_DOB_name]]))) {
        
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or a similar class."
        )
      }
    }
    
    progress_bar_population
    
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
  
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
  
  final_data <- patient_scene_table |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    patient_age_in_days = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years >= 18,
    calc_age_minor = patient_age_in_years < 18 & patient_age_in_days >= 1
    )
  
  } else if(
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) 
    
  {
    
    final_data <- patient_scene_table |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2
    
    )
    
  }
    
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
  
  # pulse oximetry
  pulse_oximetry_data <- vitals_table |> 
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      {{ evitals_12_col }} < 90
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
  
  # oxygen med used
  oxygen_med_data <- medications_table |> 
    dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = oxygen_values, x = {{ emedications_03_col }}) &
        !grepl(pattern = not_med, x = {{ emedications_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
  
  # oxygen procedure used
  oxygen_proc_data <- procedures_table |> 
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = oxygen_therapy_values, x = {{ eprocedures_03_col }}, ignore.case = T) &
        !grepl(pattern = not_proc, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
  
  # 911 calls
  call_911_data <- response_table |> 
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
  
  # assign variables to final data
  computing_population <- final_data |> 
    dplyr::mutate(PULSE_OXIMETRY = {{ erecord_01_col }} %in% pulse_oximetry_data,
                  OXYGEN1 = {{ erecord_01_col }} %in% oxygen_med_data,
                  OXYGEN2 = {{ erecord_01_col }} %in% oxygen_proc_data,
                  OXYGEN = OXYGEN1 | OXYGEN2,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data
    ) 
  
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # get the initial population
  initial_population <- computing_population |> 
    dplyr::filter(
      
      PULSE_OXIMETRY,
      
      CALL_911
      
    )
  
  # Adult and Pediatric Populations
  
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  if(
    
    # use the system generated and calculated ages
    
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    
  # get population 1 for respiratory-02, peds
  peds_pop <- initial_population |>
    dplyr::filter(
      
      (system_age_minor & !system_age_minor_exclusion) | calc_age_minor
      
    )
  
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # get population 2 for respiratory-02, adults
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  } else if(
    
    # only use the system generated values
    
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
    
  ) {
    
    # get population 1 for respiratory-02, peds
    peds_pop <- initial_population |>
      dplyr::filter(
        
        system_age_minor & !system_age_minor_exclusion
        
      )
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
    
    # get population 2 for respiratory-02, adults
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)
    
  }
  
  # summarize
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Oxygen given as med", 
               "Oxygen therapy procedure",
               "Pulse oximetry < 90",
               "911 calls",
               "Adults denominator",
               "Peds denominator", 
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$OXYGEN1, na.rm = T),
      sum(computing_population$OXYGEN2, na.rm = T),
      sum(computing_population$PULSE_OXIMETRY, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )
  
  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
  
  # get the population of interest
  respiratory.02.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population
  )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(respiratory.02.population)
  
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table)
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
  
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
  
  final_data <- df |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ evitals_12_col }},
                     {{ emedications_03_col }},
                     {{ eprocedures_03_col }}
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    patient_age_in_days = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years >= 18,
    calc_age_minor = patient_age_in_years < 18 & patient_age_in_days >= 1
    )
  
  } else if(
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) 
    
  {
    
    final_data <- df |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ evitals_12_col }},
                     {{ emedications_03_col }},
                     {{ eprocedures_03_col }}
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2
    
    )
    
  }
    
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
  
  # pulse oximetry
  pulse_oximetry_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      {{ evitals_12_col }} < 90
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
  
  # oxygen med used
  oxygen_med_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ emedications_03_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = oxygen_values, x = {{ emedications_03_col }}) &
        !grepl(pattern = not_med, x = {{ emedications_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
  
  # oxygen procedure used
  oxygen_proc_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = oxygen_therapy_values, x = {{ eprocedures_03_col }}, ignore.case = T) &
        !grepl(pattern = not_proc, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
  
  # 911 calls
  call_911_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
  
  # assign variables to final data
  computing_population <- final_data |> 
    dplyr::mutate(PULSE_OXIMETRY = {{ erecord_01_col }} %in% pulse_oximetry_data,
                  OXYGEN1 = {{ erecord_01_col }} %in% oxygen_med_data,
                  OXYGEN2 = {{ erecord_01_col }} %in% oxygen_proc_data,
                  OXYGEN = OXYGEN1 | OXYGEN2,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data
    ) 
  
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # get the initial population
  initial_population <- computing_population |> 
    dplyr::filter(
      
      PULSE_OXIMETRY,
      
      CALL_911
      
    )
  
  # Adult and Pediatric Populations
  
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  if(
    
    # use the system generated and calculated ages
    
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    
  # get population 1 for respiratory-02, peds
  peds_pop <- initial_population |>
    dplyr::filter(
      
      (system_age_minor & !system_age_minor_exclusion) | calc_age_minor
      
    )
  
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # get population 2 for respiratory-02, adults
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  } else if(
    
    # only use the system generated values
    
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
    
  ) {
    
    # get population 1 for respiratory-02, peds
    peds_pop <- initial_population |>
      dplyr::filter(
        
        system_age_minor & !system_age_minor_exclusion
        
      )
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
    
    # get population 2 for respiratory-02, adults
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)
    
  }
  
  # summarize
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Oxygen given as med", 
               "Oxygen therapy procedure",
               "Pulse oximetry < 90",
               "911 calls",
               "Adults denominator",
               "Peds denominator", 
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$OXYGEN1, na.rm = T),
      sum(computing_population$OXYGEN2, na.rm = T),
      sum(computing_population$PULSE_OXIMETRY, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )
  
  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
  
  # get the population of interest
  respiratory.02.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population
  )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(respiratory.02.population)
  
  }
  
}
