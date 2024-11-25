#' Respiratory-01 Calculation
#'
#' The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.
#'
#' @section Assumptions:
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the primary/secondary impression fields have the
#' ICD-10 code in them.  The text description can be present, too, for reference.
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that vitals in the vital signs columns are likely the
#' first vital signs, or are a list column.  This will give an indication of whether
#' or not any vitals were taken.
#' the esituation_12 is best as a list column of the secondary impressions entered
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame containing incident data with each row representing an observation.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> Unique Patient ID
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for the Incident Date field.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Column name for epatient.17.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column name for 911 response codes (e.g., 2205001, 2205003, 2205009).
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column name for primary impression codes related to respiratory distress.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column name for secondary impression codes related to respiratory distress.
#' @param evitals_12_col <['tidy-select'][dplyr_tidy_select]> Column name for the first vital sign measurement.
#' @param evitals_14_col <['tidy-select'][dplyr_tidy_select]> Column name for the second vital sign measurement.
#' @param ... arguments passed to `dplyr::summarize()`.
#'
#' @return Returns a data frame summarizing the proportion of cases with complete vital sign data, divided by population
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#'
respiratory_01 <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           esituation_11_col,
                           esituation_12_col,
                           evitals_12_col,
                           evitals_14_col,
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
  
  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")
  
  options(cli.progress_bar_style = list(
    complete = cli::col_green("●"),
    incomplete = cli::col_br_white("─")
  ))
  
  # header
  cli::cli_h1("Calculating Respiratory-01")
  
  # initiate the progress bar process
  progress_bar <- cli::cli_progress_bar(
    "Running `respiratory_01()`",
    total = 12,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  progress_bar
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary impression fields
  resp_codes <- "\\b(?:I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9)\\b"
  
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
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{ incident_date_col }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{ patient_DOB_col }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{ erecord_01_col }},
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
                     {{ esituation_11_col }},
                     {{ esituation_12_col }},
                     {{ evitals_12_col }},
                     {{ evitals_14_col }}
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    
    # system age checks
    system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
    system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
    system_age_minor2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    
    # calculated age checks
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
  
  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # respiratory distress
  
  respiratory_distress_data1 <- core_data |> 
    dplyr::select(Unique_ID, {{ esituation_11_col }}) |> 
    dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_11_col }}, ignore.case = T)) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  respiratory_distress_data2 <- core_data |> 
    dplyr::select(Unique_ID, {{ esituation_12_col }}) |> 
    dplyr::filter(grepl(pattern = resp_codes, x = {{ esituation_12_col }}, ignore.case = T)) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # vitals check
  
  vitals_check1 <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_12_col }}) |> 
    dplyr::filter(
      
      !is.na({{ evitals_12_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  vitals_check2 <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_14_col }}) |> 
    dplyr::filter(
      
      !is.na({{ evitals_14_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(RESPIRATORY_DISTRESS1 = Unique_ID %in% respiratory_distress_data1,
                  RESPIRATORY_DISTRESS2 = Unique_ID %in% respiratory_distress_data2,
                  RESPIRATORY_DISTRESS = RESPIRATORY_DISTRESS1 | RESPIRATORY_DISTRESS2,
                  CALL_911 = Unique_ID %in% call_911_data,
                  VITALS_CHECK1 = Unique_ID %in% vitals_check1,
                  VITALS_CHECK2 = Unique_ID %in% vitals_check2,
                  VITALS_CHECK = VITALS_CHECK1 & VITALS_CHECK2
    ) |> 
    dplyr::filter(
      
      RESPIRATORY_DISTRESS,
      
      CALL_911
    )
  
  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  cli::cli_progress_update(set = 8, id = progress_bar, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # get the summary of results
  
  cli::cli_progress_update(set = 9, id = progress_bar, force = T)
  
  # all
  total_population <- initial_population |>
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "All",
                      VITALS_CHECK,
                      ...)
  
  cli::cli_progress_update(set = 10, id = progress_bar, force = T)
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "Adults",
                      VITALS_CHECK,
                      ...)
  
  cli::cli_progress_update(set = 11, id = progress_bar, force = T)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Respiratory-01",
                      population_name = "Peds",
                      VITALS_CHECK,
                      ...)
  
  cli::cli_progress_update(set = 12, id = progress_bar, force = T)
  
  # summary
  resp_01 <- dplyr::bind_rows(adult_population, peds_population, total_population)
  
  cli::cli_progress_done()
  
  resp_01
  
}
