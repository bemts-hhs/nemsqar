#' Safety-02 Populations
#'
#' Filters data down to the target populations for Safety-02, and categorizes 
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to a 911 request during which lights and sirens were not used during patient 
#' transport. This function segments the data by age into adult and pediatric populations.
#'
#' @section Details:
#'
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
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
#' @param df A data frame where each row is an observation, and each column represents a feature.
#' @param patient_scene_table A data.frame or tibble containing only epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse fields needed for this measure's calculations.
#' @param disposition_table A data.frame or tibble containing only the edisposition fields needed for this measure's calculations.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name representing the date of the incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column giving response codes, identifying 911 responses.
#' @param edisposition_18_col <['tidy-select'][dplyr_tidy_select]> Column giving transport mode descriptors, including possible lights-and-sirens indicators.
#' @param edisposition_28_col <['tidy-select'][dplyr_tidy_select]> Column giving patient evaluation and care categories for the EMS response.
#' @param transport_disposition_cols <['tidy-select'][dplyr_tidy_select]> One or more unquoted column names (such as edisposition.12, edisposition.30) containing transport 
#' disposition details.
#' @param ... Additional arguments for summary calculation, if needed.
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
safety_02_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      edisposition_18_col,
                      edisposition_28_col,
                      transport_disposition_cols,
                      ...) {


 # ensure that not all table arguments AND the df argument are fulfilled
 # user only passes df or all table arguments
  
    if(
    
    any(
      !is.null(patient_scene_table), 
      !is.null(response_table),
      !is.null(disposition_table)
    ) 
    
    &&
    
    !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn safety_02_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
  }

  # ensure that df or all table arguments are fulfilled
  
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table),
      is.null(disposition_table)
    )
    
    && is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn safety_02_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")
    
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
      missing(edisposition_18_col),
      missing(edisposition_28_col),
      missing(transport_disposition_cols)
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn safety_02_population}.")
    
  }
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"
  
  # patient evaluation care
  patient_care <- "4228001|Patient Evaluated and Care Provided"

  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"

  # get codes as a regex to find lights and siren responses
  no_lights_and_sirens <- "4218015|No Lights or Sirens"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  year_values <- "2516009|years"
  
  day_values <- "days|2516001"
  
  hour_values <- "hours|2516003"
  
  minute_values <- "minutes|2516005"
  
  month_values <- "months|2516007"

  # initiate the progress bar process
  progress_bar_population <- cli::cli_progress_bar(
    "Running `safety_02_population()`",
    total = 11,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  progress_bar_population
  
  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(disposition_table)
    ) && is.null(df)
    
  ) {
    
    # Ensure all tables are of class `data.frame` or `tibble`
    if (
      
      !all(
      is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
      is.data.frame(response_table) || tibble::is_tibble(response_table),
      is.data.frame(disposition_table) || tibble::is_tibble(disposition_table)
      )
    
    ) {
      
      cli::cli_abort(
        "One or more of the tables passed to {.fn safety_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
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

    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________
    
    
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      
  # filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    )
  
    } else if(
      
      all(
        is.null(incident_date_col), 
        is.null(patient_DOB_col)
        
      )) {
      
      # filter the table to get the initial population regardless of age
  final_data <- patient_scene_table |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

    )
      
    }
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________
    
    # 911 calls
    call_911_data <- response_table |> 
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)
        
      ) |> 
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    #patient evaluated and care provided
    patient_eval_care_data <- disposition_table |> 
    dplyr::select({{ erecord_01_col }}, {{ edisposition_28_col}}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = patient_care, x = {{ edisposition_28_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
    
    # no lights and sirens check
    no_ls_data <- disposition_table |> 
    dplyr::select({{ erecord_01_col }}, {{ edisposition_18_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = no_lights_and_sirens, x = {{ edisposition_18_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
    
    # transports
    transport_data <- disposition_table |> 
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_cols }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      if_any(
        c({{ transport_disposition_cols }}),
        ~ grepl(
          pattern = transport_responses,
          x = .,
          ignore.case = T
        )
      )
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
 
# assign variables to final data
  computing_population <- final_data |> 
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  PATIENT_CARE = {{ erecord_01_col }} %in% patient_eval_care_data,
                  TRANSPORT = {{ erecord_01_col }} %in% transport_data,
                  NO_LS_CHECK = {{ erecord_01_col }} %in% no_ls_data
                  ) 
  
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # get the initial population
  initial_population <- computing_population |> 
    dplyr::filter(
      
      CALL_911,
      PATIENT_CARE,
      TRANSPORT
      
    )


  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult
                  )
  
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor
                  )
  
  } else if(
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) {
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult
                    )
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
    dplyr::filter(system_age_minor
                  )
  
  }

  # get the summary of results
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Patients evaluated and care provided",
                 "Transport runs",
                 "No lights and sirens",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population", 
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$PATIENT_CARE, na.rm = T),
        sum(computing_population$TRANSPORT, na.rm = T),
        sum(computing_population$NO_LS_CHECK, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )
    
    # get the populations of interest
    
    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
    
    # gather data into a list for multi-use output
    safety.02.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(safety.02.population)  
  
  } else if(
    
    all(
      is.null(patient_scene_table),
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
    
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    # from the full dataframe with all variables
    # create one fact table and several dimension tables
    # to complete calculations and avoid issues due to row
    # explosion
    ###_____________________________________________________________________________
    
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      
  # filter the table to get the initial population regardless of age
  final_data <- df |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ edisposition_18_col}},
                     {{ edisposition_28_col }},
                     {{ transport_disposition_cols }}
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2,

      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    )
  
    } else if(
      
      all(
        is.null(incident_date_col), 
        is.null(patient_DOB_col)
        
      )) {
      
      # filter the table to get the initial population regardless of age
  final_data <- df |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ edisposition_18_col}},
                     {{ edisposition_28_col }},
                     {{ transport_disposition_cols }}
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    # create the age in years variable

    dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} >= 24 & grepl(pattern = month_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

    )
      
    }
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________
    
    # 911 calls
    call_911_data <- df |> 
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |> 
      dplyr::distinct() |> 
      dplyr::filter(
        
        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)
        
      ) |> 
      dplyr::distinct({{ erecord_01_col }}) |> 
      dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
    
    #patient evaluated and care provided
    patient_eval_care_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ edisposition_28_col}}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = patient_care, x = {{ edisposition_28_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
    
    # no lights and sirens check
    no_ls_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ edisposition_18_col }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      grepl(pattern = no_lights_and_sirens, x = {{ edisposition_18_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
    
    cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
    
    # transports
    transport_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_cols }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(
      
      if_any(
        c({{ transport_disposition_cols }}),
        ~ grepl(
          pattern = transport_responses,
          x = .,
          ignore.case = T
        )
      )
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
 
# assign variables to final data
  computing_population <- final_data |> 
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  PATIENT_CARE = {{ erecord_01_col }} %in% patient_eval_care_data,
                  TRANSPORT = {{ erecord_01_col }} %in% transport_data,
                  NO_LS_CHECK = {{ erecord_01_col }} %in% no_ls_data
                  ) 
  
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # get the initial population
  initial_population <- computing_population |> 
    dplyr::filter(
      
      CALL_911,
      PATIENT_CARE,
      TRANSPORT
      
    )


  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult
                  )
  
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor
                  )
  
  } else if(
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) {
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult
                    )
    
    cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
    dplyr::filter(system_age_minor
                  )
  
  }

  # get the summary of results
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("911 calls",
                 "Patients evaluated and care provided",
                 "Transport runs",
                 "No lights and sirens",
                 "Adults denominator",
                 "Peds denominator",
                 "Initial population", 
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$PATIENT_CARE, na.rm = T),
        sum(computing_population$TRANSPORT, na.rm = T),
        sum(computing_population$NO_LS_CHECK, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(initial_population),
        nrow(computing_population)
      )
    )
    
    # get the populations of interest
    
    cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
    
    # gather data into a list for multi-use output
    safety.02.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop,
      initial_population = initial_population
    )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(safety.02.population)
  
  }
    
}
