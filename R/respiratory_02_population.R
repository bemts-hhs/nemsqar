#' Respiratory-02 Calculation
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
      !is.null(situation_table),
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) 
    
    &&
    
    !is.null(df)
    
  ) {
    
    cli::cli_abort("{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all three of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all three table arguments.")
    
  }
  
  # ensure that df or all table arguments are fulfilled
  if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
      is.null(vitals_table), 
      is.null(medications_table),
      is.null(procedures_table)
    )
    
    && is.null(df)
  ) {
    
    cli::cli_abort("{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all six of the table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all six table arguments.")
    
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
      missing(evitals_18_col),
      missing(evitals_23_cl),
      missing(evitals_26_col),
      missing(emedications_03_col),
      missing(eprocedures_03_col)
    )
    
  ) {
    
    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn hypoglycemia_01_population}.")
    
  }
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"
  
  # get codes as a regex to filter primary/secondary impression fields
  hypoglycemia_treatment_codes <- "4832|4850|377980|376937|372326|237653|260258|309778|1795610|1795477|1794567|1165823|1165822|1165819|Glucagon|Glucose|Glucose Oral Gel|Glucose Injectable Solution|Glucose Chewable Tablet|Glucose 500 MG/ML Injectable Solution|Glucose 250 MG/ML Injectable Solution|Glucose 50 MG/ML Injectable Solution|250 ML Glucose 50 MG/ML Injection|500 ML Glucose 100 MG ML Injection|Glucose Injection|Glucose Oral Product|Glucose Oral Liquid Product|Glucose Injectable Product"
  
  # hypoglycemia procedures
  
  hypoglycemia_procedure_codes <- "710925007|225285007|Provision of food|Giving oral fluid"
  
  # code(s) for altered mental status
  altered_mental_status <- "\\b(?:R41.82)\\b|Altered Mental Status, unspecified"
  
  # codes for diabetes via primary and secondary impression
  
  diabetes_codes <- "\\b(?:E13.64|E16.2)\\b|Other specified diabetes mellitus with hypoglycemia|Hypoglycemia, unspecified"
  
  # AVPU responses
  
  avpu_responses <- "Unresponsive|Verbal|Painful|3326003|3326005|3326007"
  
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
    "Running `hypoglycemia_01_population()`",
    total = 17,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  # not values for meds
  not_med <- "8801001|8801003|8801009|8801019|8801027"
  
  # not values for procedures
  not_proc <- "8801001|8801023|8801003|8801019|8801027"
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ eresponse_05_col }},
                     {{ evitals_12_col }},
                     {{ emedications_03_col }},
                     {{ eprocedures_03_col }}
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
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
    system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
    system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,
    system_age_minor_exclusion1 = {{ epatient_15_col }} < 24 & {{ epatient_16_col }} == "Hours",
    system_age_minor_exclusion2 = {{ epatient_15_col }} < 120 & {{ epatient_16_col }} == "Minutes",
    system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years >= 18,
    calc_age_minor = patient_age_in_years < 18 & patient_age_in_days >= 1
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # pulse oximetry
  pulse_oximetry_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_12_col }}) |> 
    dplyr::filter(
      
      {{ evitals_12_col }} < 90
      
    ) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # vitals check
  oxygen_med_data <- core_data |> 
    dplyr::select(Unique_ID, {{ emedications_03_col }}) |> 
    dplyr::filter(
      
      grepl(pattern = "7806", x = {{ emedications_03_col }}) &
        !grepl(pattern = not_med, x = {{ emedications_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  oxygen_proc_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eprocedures_03_col }}) |> 
    dplyr::filter(
      
      grepl(pattern = "57485005", x = {{ eprocedures_03_col }}, ignore.case = T) &
        !grepl(pattern = not_proc, x = {{ eprocedures_03_col }}, ignore.case = T)
      
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
  
  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # assign variables to final data
  initial_population <- final_data |> 
    dplyr::mutate(PULSE_OXIMETRY = Unique_ID %in% pulse_oximetry_data,
                  OXYGEN1 = Unique_ID %in% oxygen_med_data,
                  OXYGEN2 = Unique_ID %in% oxygen_proc_data,
                  OXYGEN = OXYGEN1 | OXYGEN2,
                  CALL_911 = Unique_ID %in% call_911_data
    ) |> 
    dplyr::filter(
      
      PULSE_OXIMETRY,
      
      CALL_911
      
    )
  
  cli::cli_progress_update(set = 8, id = progress_bar, force = T)
  
  # get population 1 for respiratory-02, peds
  respiratory_02_population_peds <- initial_population |>
    dplyr::filter(
      
      (system_age_minor & !system_age_minor_exclusion) | calc_age_minor
      
    )
  
  cli::cli_progress_update(set = 9, id = progress_bar, force = T)
  
  # get population 2 for respiratory-02, adults
  respiratory_02_population_adults <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  cli::cli_progress_update(set = 10, id = progress_bar, force = T)
  
  # calculations for peds
  peds_calculation <- respiratory_02_population_peds |>
    dplyr::summarize(
      measure = "Respiratory-02",
      pop = "Peds",
      numerator = sum(OXYGEN, na.rm = T),
      denominator = dplyr::n(),
      prop = sum(numerator, na.rm = T) / dplyr::n(),
      prop_label = pretty_percent(prop, n_decimal = 0.01),
      ...
    )
  
  cli::cli_progress_update(set = 11, id = progress_bar, force = T)
  
  # calculations for adults
  adults_calculation <- respiratory_02_population_adults |>
    dplyr::summarize(
      measure = "Respiratory-02",
      pop = "Adults",
      numerator = sum(OXYGEN, na.rm = T),
      denominator = dplyr::n(),
      prop = sum(numerator, na.rm = T) / dplyr::n(),
      prop_label = pretty_percent(prop, n_decimal = 0.01),
      ...
    )
  
  cli::cli_progress_update(set = 12, id = progress_bar, force = T)
  
  # bind rows of calculations for final table
  respiratory.02 <- dplyr::bind_rows(peds_calculation, adults_calculation)
  
  cli::cli_progress_done()
  
  respiratory.02
  
}
