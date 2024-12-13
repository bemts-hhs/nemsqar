#' Hypoglycemia-01 Populations
#'
#' The `hypoglycemia_01_population` ilters data down to the target populations for Hypoglycemia-01, and categorizes 
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to diabetes/hypoglycemia incidents in an EMS dataset,
#' specifically focusing on cases where 911 was called for diabetes/hypoglycemia distress,
#' certain medications were administered, and a weight is taken. This function segments the data
#' into pediatric populations, computing the proportion of cases that have a documented weight.
#'
#' @section Data Assumptions:
#' The `df` argument and each `*_table` argument should be a dataframe or tibble, with the following assumptions:
#' - The data are already loaded.
#' - The function will calculate an age in years using the incident date and the patient DOB.
#' - The incident date and the patient DOB are `Date` or `POSIXct` data types.
#' - Any missing values are encoded as `NA`, not the "Not Known"/"Not Recorded" text values
#'   or NEMSIS "not value" codes commonly reported by ePCR vendors.
#' - The vitals field may be the full list of values for each field or the lowest estimated
#'   eVitals.18 (must include the "Low Blood Glucose" flag) and lowest estimated patient AVPU
#'   in eVitals.2.
#' - The function assumes that the primary and secondary impression fields (eSituation.11
#'   and eSituation.12) have the ICD-10 codes in them. The test description can be present,
#'   too, for reference.
#' - The function assumes that the eResponse.05 fields have the NEMSIS codes in them,
#'   although text can be also be present for reference.
#' - The function assumes that the eMedications_03 and eProcedures_03 fields contain
#'   all medications/procedures and that it contains the text description of the
#'   generic name of the medication. The codes can be included for reference, but will
#'   not be checked. ALL medications and procedures are in one field per record, as either
#'   a list column or a comma-separated list.
#' - The eSituation.12 (Secondary Impression) field is best as a list column of the secondary
#'   impressions. No joining is done.
#' - Any joins to get vitals, etc., will need to be done outside the function.
#' - Grouping by specific attributes (e.g., region) can be performed inside this function by
#'   utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame or tibble containing emergency response records.
#' @param patient_scene_table A data.frame or tibble containing only epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing only the esituation fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing only the evtials fields needed for this measure's calculations.
#' @param medications_table A data.frame or tibble containing only the emedications fields needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing only the eprocedures fields needed for this measure's calculations.
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column representing the EMS record unique identifier.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the incident date. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains the patient's date of birth. This defaults to `NULL` as it is optional in case not available due to PII restrictions.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's numeric age agnostic of unit.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column representing the patient's age unit ("Years", "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eResponse.05.
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains eSituation.11.
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eSituation.12 values as a single comma-separated list.
#' @param evitals_18_col <['tidy-select'][dplyr_tidy_select]> Column for blood glucose levels.
#' @param evitals_23_cl <['tidy-select'][dplyr_tidy_select]> Column for Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> Column for AVPU alertness levels.
#' @param emedications_03_col <['tidy-select'][dplyr_tidy_select]> Column that
#' contains all eMedications.03 values as a single comma-separated list.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column for procedures performed.
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
hypoglycemia_01_population <- function(df = NULL,
                            patient_scene_table = NULL,
                            response_table = NULL,
                            situation_table = NULL,
                            vitals_table = NULL,
                            medications_table = NULL,
                            procedures_table = NULL,
                            erecord_01_col,
                            incident_date_col = NULL,
                            patient_DOB_col = NULL,
                            epatient_15_col,
                            epatient_16_col,
                            eresponse_05_col,
                            esituation_11_col,
                            esituation_12_col,
                            evitals_18_col,
                            evitals_23_cl,
                            evitals_26_col,
                            emedications_03_col,
                            eprocedures_03_col
                            ) {
  
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
  
  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table), 
      !is.null(situation_table),
      !is.null(vitals_table), 
      !is.null(medications_table),
      !is.null(procedures_table)
    ) && is.null(df)
    
  ) {
    
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
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)
    
    ###_____________________________________________________________________________
    # fact table
    # the user should ensure that variables beyond those supplied for calculations
    # are distinct (i.e. one value or cell per patient)
    ###_____________________________________________________________________________
    
    # progress update, these will be repeated throughout the script
    
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      
    # filter the table to get the initial population
    final_data <- patient_scene_table |> 
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
      dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{  incident_date_col  }},
        time2 = {{  patient_DOB_col  }},
        units = "days"
      )) / 365,
      patient_age_in_days_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )),
      
      # system age check
      system_age_adult = {{  epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col  }}, ignore.case = T), 
      system_age_minor1 = {{  epatient_15_col }} < 18  & grepl(pattern = year_values, x = {{ epatient_16_col  }}, ignore.case = T), 
      system_age_minor2 = !is.na({{ epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
      system_age_minor3 = !({{ epatient_15_col}} < 1 & grepl(pattern = day_values, x = {{ epatient_16_col  }}, ignore.case = T)) &
        !({{ epatient_15_col}} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col  }}, ignore.case = T)) &
        !({{ epatient_15_col}} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col  }}, ignore.case = T)),
      system_age_minor = (system_age_minor1 | system_age_minor2) & system_age_minor3, 
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18, 
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_days_col >= 1
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
      
      # system age check
      system_age_adult = {{  epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col  }}, ignore.case = T), 
      system_age_minor1 = {{  epatient_15_col }} < 18  & grepl(pattern = year_values, x = {{ epatient_16_col  }}, ignore.case = T), 
      system_age_minor2 = !is.na({{ epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
      system_age_minor3 = !({{ epatient_15_col}} < 1 & grepl(pattern = day_values, x = {{ epatient_16_col  }}, ignore.case = T)) &
        !({{ epatient_15_col}} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col  }}, ignore.case = T)) &
        !({{ epatient_15_col}} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col  }}, ignore.case = T)),
      system_age_minor = (system_age_minor1 | system_age_minor2) & system_age_minor3
      
      )
      
    }
    
    ###_____________________________________________________________________________
    ### dimension tables
    ### each dimension table is turned into a vector of unique IDs
    ### that are then utilized on the fact table to create distinct variables
    ### that tell if the patient had the characteristic or not for final
    ### calculations of the numerator and filtering
    ###_____________________________________________________________________________
    
    # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
  
  # altered mental status 1
  altered_data1 <- situation_table |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
  
  # altered mental status 2
  altered_data2 <- situation_table |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
  
  # AVPU
  AVPU_data <- vitals_table |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_26_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T)
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
  
  # GCS
  GCS_data <- vitals_table |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_cl  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter({{ evitals_23_cl }} < 15) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
  
  # diabetes data 1
  diabetes_data1 <- situation_table |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |> 
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # diabetes data 2
  diabetes_data2 <- situation_table |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  # blood glucose
  blood_glucose_data <- vitals_table |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_18_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter({{ evitals_18_col }} < 60) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # 911 calls
  call_911_data <- response_table |> 
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = T)) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # correct treatment 1
  correct_treatment_data1 <- medications_table |> 
    dplyr::select({{ erecord_01_col }}, {{  emedications_03_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ emedications_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
  
  # correct treatment 2
  correct_treatment_data2 <- procedures_table |> 
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)
  
  # assign variables to the final data
  computing_population <- final_data |> 
    dplyr::mutate(GCS = {{ erecord_01_col }} %in% GCS_data,
                  AVPU = {{ erecord_01_col }} %in% AVPU_data,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  ALTERED1 = {{ erecord_01_col }} %in% altered_data1,
                  ALTERED2 = {{ erecord_01_col }} %in% altered_data2,
                  ALTERED = ALTERED1 | ALTERED2,
                  DIABETES1 = {{ erecord_01_col }} %in% diabetes_data1,
                  DIABETES2 = {{ erecord_01_col }} %in% diabetes_data2,
                  DIABETES = DIABETES1 | DIABETES2,
                  BLOOD_GLUCOSE = {{ erecord_01_col }} %in% blood_glucose_data,
                  TREATMENT1 = {{ erecord_01_col }} %in% correct_treatment_data1,
                  TREATMENT2 = {{ erecord_01_col }} %in% correct_treatment_data2,
                  TREATMENT = TREATMENT1 | TREATMENT2
                  )
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)
  
  # get the initial population
  
  initial_population <- computing_population |> 
    dplyr::filter(
      
      (DIABETES & (GCS | AVPU)) |
        
        (ALTERED & BLOOD_GLUCOSE) & 
        
        CALL_911,
      
      system_age_minor3
      
    )
  
  # Adult and Pediatric Populations
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)
  
  if(
    
    # use the system generated and calculated ages
    
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
    )
  ) {
    
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  } else if(
    
    # only use the system generated values
    
    all(
      is.null(incident_date_col),
      is.null(patient_DOB_col)
    )
    
  ) {
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)
    
    
  }
  
  # summarize
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Diabetes/Hypoglycemia and Verbal, Painful, Unresponsive or GCS < 15", 
               "Altered mental status and low blood glucose",
               "911 calls",
               "Adults denominator",
               "Peds denominator", 
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$DIABETES & (computing_population$AVPU | computing_population$GCS), na.rm = T),
      sum(computing_population$ALTERED & computing_population$BLOOD_GLUCOSE, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )
  
  cli::cli_progress_update(set = 17, id = progress_bar_population, force = T)
  
  # get the population of interest
  hypoglycemia.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population
  )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(hypoglycemia.01.population)
    
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table), 
      is.null(situation_table),
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
  
  if(all(
    
    !rlang::quo_is_null(rlang::enquo(incident_date_col)),
    !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
  )) {
    
  final_data <- df |> 
    dplyr::select(-c({{  eresponse_05_col  }},
                     {{  esituation_11_col  }},
                     {{  esituation_12_col }},
                     {{  evitals_18_col  }},
                     {{  evitals_23_cl  }},
                     {{  evitals_26_col  }},
                     {{  emedications_03_col  }},
                     {{  eprocedures_03_col  }}
                     
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,
    patient_age_in_days_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age check
    system_age_adult = {{  epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col}}, ignore.case = T), 
    system_age_minor1 = {{  epatient_15_col }} < 18  & grepl(pattern = year_values, x = {{ epatient_16_col}}, ignore.case = T), 
    system_age_minor2 = !is.na({{ epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor3 = !({{ epatient_15_col}} < 1 & grepl(pattern = day_values, x = {{ epatient_16_col}}, ignore.case = T)) &
      !({{ epatient_15_col}} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col}}, ignore.case = T)) &
      !({{ epatient_15_col}} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col}}, ignore.case = T)),
    system_age_minor = (system_age_minor1 | system_age_minor2) & system_age_minor3, 
    
    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18, 
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_days_col >= 1
    )
  
  } else if(
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) {
    
    final_data <- df |> 
    dplyr::select(-c({{  eresponse_05_col  }},
                     {{  esituation_11_col  }},
                     {{  esituation_12_col }},
                     {{  evitals_18_col  }},
                     {{  evitals_23_cl  }},
                     {{  evitals_26_col  }},
                     {{  emedications_03_col  }},
                     {{  eprocedures_03_col  }}
                     
    )) |> 
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |> 
    dplyr::mutate(
    
    # system age check
    system_age_adult = {{  epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col}}, ignore.case = T), 
    system_age_minor1 = {{  epatient_15_col }} < 18  & grepl(pattern = year_values, x = {{ epatient_16_col}}, ignore.case = T), 
    system_age_minor2 = !is.na({{ epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor3 = !({{ epatient_15_col}} < 1 & grepl(pattern = day_values, x = {{ epatient_16_col}}, ignore.case = T)) &
      !({{ epatient_15_col}} < 24 & grepl(pattern = hour_values, x = {{ epatient_16_col}}, ignore.case = T)) &
      !({{ epatient_15_col}} < 120 & grepl(pattern = minute_values, x = {{ epatient_16_col}}, ignore.case = T)),
    system_age_minor = (system_age_minor1 | system_age_minor2) & system_age_minor3
    
    )
    
  }
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)
  
  # altered mental status 1
  altered_data1 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)
  
  # altered mental status 2
  altered_data2 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  altered_mental_status,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)
  
  # AVPU
  AVPU_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_26_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T)
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)
  
  # GCS
  GCS_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_cl  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter({{ evitals_23_cl }} < 15) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)
  
  # diabetes data 1
  diabetes_data1 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |> 
    dplyr::distinct() |>
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_11_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)
  
  # diabetes data 2
  diabetes_data2 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(
      pattern =  diabetes_codes,
      x = {{ esituation_12_col }},
      ignore.case = T
    )) |> 
    distinct({{ erecord_01_col }}) |> 
    pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)
  
  # blood glucose
  blood_glucose_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  evitals_18_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter({{ evitals_18_col }} < 60) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)
  
  # 911 calls
  call_911_data <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = T)) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)
  
  # correct treatment 1
  correct_treatment_data1 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  emedications_03_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ emedications_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)
  
  # correct treatment 2
  correct_treatment_data2 <- df |> 
    dplyr::select({{ erecord_01_col }}, {{  eprocedures_03_col  }}) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      grepl(
        pattern = hypoglycemia_treatment_codes,
        x = {{ eprocedures_03_col }},
        ignore.case = TRUE
      )
      
    ) |> 
    dplyr::distinct({{ erecord_01_col }}) |> 
    dplyr::pull({{ erecord_01_col }})
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)
  
  # assign variables to the final data
  computing_population <- final_data |> 
    dplyr::mutate(GCS = {{ erecord_01_col }} %in% GCS_data,
                  AVPU = {{ erecord_01_col }} %in% AVPU_data,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  ALTERED1 = {{ erecord_01_col }} %in% altered_data1,
                  ALTERED2 = {{ erecord_01_col }} %in% altered_data2,
                  ALTERED = ALTERED1 | ALTERED2,
                  DIABETES1 = {{ erecord_01_col }} %in% diabetes_data1,
                  DIABETES2 = {{ erecord_01_col }} %in% diabetes_data2,
                  DIABETES = DIABETES1 | DIABETES2,
                  BLOOD_GLUCOSE = {{ erecord_01_col }} %in% blood_glucose_data,
                  TREATMENT1 = {{ erecord_01_col }} %in% correct_treatment_data1,
                  TREATMENT2 = {{ erecord_01_col }} %in% correct_treatment_data2,
                  TREATMENT = TREATMENT1 | TREATMENT2
                  ) |> 
    distinct({{ erecord_01_col }}, .keep_all = T)
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)
  
  # get the initial population
  
  initial_population <- computing_population |> 
    dplyr::filter(
      
      (DIABETES & (GCS | AVPU)) |
        
        (ALTERED & BLOOD_GLUCOSE) & 
        
        CALL_911,
      
      system_age_minor3
      
    ) |> 
    distinct({{ erecord_01_col }}, .keep_all = T)
  
  # Adult and Pediatric Populations
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)
  
  if(
    
    # use the system generated and calculated ages
    
    all(
    
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      
  )) {
    
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  } else if(
    
    # only use the system generated values
    
    all(
      is.null(incident_date_col), 
      is.null(patient_DOB_col)
    )) {
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult)
    
    # progress update, these will be repeated throughout the script
    cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)
    
    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor)

  }
  
  # summarize
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)
  
  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Diabetes/Hypoglycemia and Verbal, Painful, Unresponsive or GCS < 15", 
               "Altered mental status and low blood glucose",
               "911 calls",
               "Adults denominator",
               "Peds denominator", 
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$DIABETES & (computing_population$AVPU | computing_population$GCS), na.rm = T),
      sum(computing_population$ALTERED & computing_population$BLOOD_GLUCOSE, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      nrow(adult_pop),
      nrow(peds_pop),
      nrow(initial_population),
      nrow(computing_population)
    )
  )
  
  cli::cli_progress_update(set = 17, id = progress_bar_population, force = T)
  
  # get the population of interest
  hypoglycemia.01.population <- list(
    filter_process = filter_counts,
    adults = adult_pop,
    peds = peds_pop,
    initial_population = initial_population
  )
  
  cli::cli_progress_done(id = progress_bar_population)
  
  return(hypoglycemia.01.population)
  
  }
  
}

