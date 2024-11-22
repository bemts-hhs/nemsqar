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
#' @param df Data frame or tibble containing EMS records.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Column for unique EMS record identifiers.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column indicating the date of the EMS incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column specifying patient date of birth.
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
pediatrics_03b <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
                           ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
      )
    )
  }
  
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
  
  reduce_date_time <- function() {
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Only show date and time to second
  }
  
  reduced_time <- function() {
    format(Sys.time(), "%H:%M:%S")  # Only show date and time to second
  }
  
  cli::cli_h1("Calculating Pediatrics-03b")
  
  start_time <- Sys.time()
  
  cli::cli_alert_info("Initiated at {reduce_date_time()}")
  
  progress_bar <- cli::cli_progress_bar(
    "Running `pediatrics_03b()`",
    total = 9,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: {paste0(abs(round(difftime(start_time, reduce_date_time(), units = 'mins'), digits = 2)), ' mins')}"
  )
  
  progress_bar
  
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # non-weight-based medications
  non_weight_based_meds <- "Inhalation|Topical|9927049|9927009"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
  core_data <- df |> 
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{  incident_date_col  }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{  patient_DOB_col  }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{  erecord_01_col  }},
                                             INCIDENT_DATE_MISSING,
                                             PATIENT_DOB_MISSING, 
                                             sep = "-"
                  ))
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ eexam_01_col }},
                     {{ eexam_02_col }},
                     {{ emedications_03_col }},
                     {{ emedications_04_col }}
                     
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,
    
    # system age check
    system_age_check1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
    system_age_check2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_check = system_age_check1 | system_age_check2, 
    
    # calculated age check
    calc_age_check = patient_age_in_years_col < 18
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # non-weight based medications
  
  non_weight_based_meds_data <- core_data |> 
    dplyr::select(Unique_ID, {{  emedications_04_col  }}) |> 
    dplyr::filter(grepl(
      pattern = non_weight_based_meds,
      x = {{ emedications_04_col }},
      ignore.case = TRUE
    )) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # meds not missing
  
  meds_not_missing_data <- core_data |> 
    dplyr::select(Unique_ID, {{  emedications_03_col  }}) |> 
    dplyr::filter(!is.na({{ emedications_03_col }})
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{  eresponse_05_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # documented weight
  
  documented_weight_data1 <- core_data |> 
    dplyr::select(Unique_ID, {{  eexam_01_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      !is.na({{ eexam_01_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  documented_weight_data2 <- core_data |> 
    dplyr::select(Unique_ID, {{  eexam_02_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      !is.na({{ eexam_02_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 8, id = progress_bar, force = T)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(NON_WEIGHT_BASED = Unique_ID %in% non_weight_based_meds_data,
                  MEDS_NOT_MISSING = Unique_ID %in% meds_not_missing_data,
                  CALL_911 = Unique_ID %in% call_911_data,
                  DOCUMENTED_WEIGHT1 = Unique_ID %in% documented_weight_data1,
                  DOCUMENTED_WEIGHT2 = Unique_ID %in% documented_weight_data2,
                  DOCUMENTED_WEIGHT = DOCUMENTED_WEIGHT1 | DOCUMENTED_WEIGHT2
    ) |> 
    dplyr::filter(
      
      # only 911 calls
      CALL_911,
      
      # only rows where meds are passed
      MEDS_NOT_MISSING,
      
      # age filter
      system_age_check | calc_age_check,
      
      # exclude non-weight based meds
      !NON_WEIGHT_BASED
      
    )
  
  cli::cli_progress_update(set = 9, id = progress_bar, force = T)
  
  # get the summary of results, already filtered down to the target age group for the measure
  
  # peds
  pediatrics.03b <- initial_population |>
    summarize_measure(measure_name = "Pediatrics-03b",
                      population_name = "Peds",
                      DOCUMENTED_WEIGHT,
                      ...)
  
  cli::cli_progress_done()
  
  # summary
  pediatrics.03b
  
}
