airway_18 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eprocedures_03_col,
                      eresponse_05_col,
                      eprocedures_06_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eairway_04_col,
                      eairway_02_col,
                      evitals_01_col,
                      evitals_16_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn airway_18}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn airway_18}."
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
  incident_date_col <- rlang::enquo(incident_date_col)
  patient_DOB <- rlang::enquo(patient_DOB_col)
  
  if ((!lubridate::is.Date(df[[rlang::as_name(incident_date_col)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(incident_date_col)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))) {
    
    cli::cli_abort(
      "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
    )
    
  }
  
  # use quasiquotation on the vitals, airway, and procedures datetime fields
  airway_datetime <- rlang::enquo(eairway_02_col)
  vitals_datetime <- rlang::enquo(evitals_01_col)
  procedures_datetime <- rlang::enquo(eprocedures_01_col)
  
  if ((!lubridate::is.Date(df[[rlang::as_name(airway_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(airway_datetime)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))
      
      ) {
    
    cli::cli_abort(
      "For the variables {.var airway_datetime}, {.var procedures_datetime}, and {.var vitals_datetime}, one or a combination of these variables were not of class {.cls Date} or a similar class.  Please format your {.var airway_datetime}, {.var procedures_datetime}, and {.var vitals_datetime} to class {.cls Date} or similar class."
    )
    
  }
  
  progress_bar <- cli_progress_bar("Data Preparation", total = 12, type = "tasks", clear = F,
                                     format = "{cli::pb_name} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} | Progress: {cli::pb_percent}% Finished at {Sys.time()}",
                                     options = list(
                                       bar_fill = "=",    # Fill character
                                       bar_complete = ">", # Complete character (arrow head)
                                       bar_incomplete = "-" # Incomplete part of the bar
                                     )
                                     )
  
  cli_alert("Calculating Airway-18 at {Sys.time()}")
  
  progress_bar
  
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # endotracheal intubation attempts
  endotracheal_intubation <- "673005|Indirect laryngoscopy \\(procedure\\)|49077009|Flexible fiberoptic laryngoscopy \\(procedure\\)|78121007|Direct laryngoscopy \\(procedure\\)|112798008|Insertion of endotracheal tube \\(procedure\\)|16883004|Endotracheal intubation, emergency procedure \\(procedure\\)|182682004|Emergency laryngeal intubation \\(procedure\\)|232674004|Orotracheal intubation \\(procedure\\)|232677006|Tracheal intubation using rigid bronchoscope \\(procedure\\)|232678001|Orotracheal fiberoptic intubation \\(procedure\\)|232679009|Nasotracheal intubation \\(procedure\\)|232682004|Nasotracheal fiberoptic intubation \\(procedure\\)|232680007|Nasal intubation awake \\(procedure\\)|241689008|Rapid sequence induction \\(procedure\\)|304341005|Awake intubation \\(procedure\\)|397892004|Retrograde intubation \\(procedure\\)|418613003|Tracheal intubation through a laryngeal mask airway \\(procedure\\)|429705000|Intubation, combitube \\(procedure\\)|424979004|Laryngeal mask airway insertion \\(procedure\\)|427753009|Insertion of esophageal tracheal double lumen supraglottic airway \\(procedure\\)|429161001|Insertion of endotracheal tube using laryngoscope \\(procedure\\)|450601000124103|Orotracheal intubation using bougie device \\(procedure\\)|450611000124|Insertion of Single Lumen Supraglottic Airway Device \\(procedure\\)|1141752008|Flexible video intubation laryngoscope \\(physical object\\)|285696003|Fiberoptic laryngoscope \\(physical object\\)|420311007|Flexible fiberoptic laryngoscope \\(physical object\\)|421100004|Rigid fiberoptic laryngoscope \\(physical object\\)|44738004|Laryngoscope device \\(physical object\\)|469919007|Flexible video laryngoscope \\(physical object\\)|700640001|Rigid intubation laryngoscope \\(physical object\\)|701054002|Flexible fiberoptic intubation laryngoscope \\(physical object\\)|706013009|Intubation laryngoscope \\(physical object\\)|734928009|Rigid non-bladed video intubation laryngoscope \\(physical object\\)|879788006|Channeled video intubation laryngoscope \\(physical object\\)"
  
  # waveform ETCO2
  waveform_etco2 <- "4004019|Waveform ETCO2"
  
  # answer yes!
  yes_code <- "9923003|Yes"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  cli::cli_progress_update(set = 2, id = progress_bar, force = T)
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  core_data <- airway_18_2023_clean |> 
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{ incident_date_col }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{ patient_DOB_col }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{ erecord_01_col }},
                                             INCIDENT_DATE_MISSING,
                                             PATIENT_DOB_MISSING, 
                                             sep = "-"
                  ),
                  
                  ENDOTRACHEAL_INTUBATION = grepl(pattern = endotracheal_intubation, 
                                                  x = {{ eprocedures_03_col }}, 
                                                  ignore.case = T),
                  CALL_911 = grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T),
                  PROCEDURE_SUCCESSFUL = grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T)
                  
    ) |> 
    filter(
      ENDOTRACHEAL_INTUBATION,
      PROCEDURE_SUCCESSFUL,
      CALL_911
    )
  
  cli::cli_progress_update(set = 3, id = progress_bar, force = T)
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ eprocedures_03_col }},
                     {{ eresponse_05_col }},
                     {{ eprocedures_06_col }},
                     {{ eprocedures_01_col }},
                     {{ eprocedures_02_col }},
                     {{ eairway_04_col }},
                     {{ eairway_02_col }},
                     {{ evitals_01_col }},
                     {{ evitals_16_col }}
                     
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    patient_age_in_days_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )),
    
    # system age check
    system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years", 
    system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years", 
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2, 
    
    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18
    )
  
  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # Last successful invasive airway procedures in the initial population
  # suppress warnings due to an expected occurrence of some Unique_ID groups
  # returning `NA` values within eprocedures_01_col.  This is expected as 
  # in some cases a date/time is not entered, which is a data entry issue but expected
  # we will not get any of the expected warning output flagging these cases
  
  procedures_data <- suppressWarnings(
    core_data |> 
    dplyr::select(Unique_ID, {{ eprocedures_03_col }}, {{ eprocedures_06_col }}, {{ eprocedures_01_col }}, {{ eprocedures_02_col }}
    ) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      (
        
        {{ eprocedures_01_col }} == max({{ eprocedures_01_col }}, na.rm = T) & 
        grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T)
       
       ),
      
      grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T),
      
      !is.na({{ eprocedures_01_col }}), 
      
      !grepl(pattern = yes_code, x = {{ eprocedures_02_col }}, ignore.case = T),
      
      .by = Unique_ID
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
    
  )
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # numerator calculation part 1
  waveform_ETCO2_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eairway_04_col }}, {{ eairway_02_col }}, {{ eprocedures_01_col }}
    ) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      grepl(pattern = waveform_etco2, x = {{ eairway_04_col }}, ignore.case = T)
      
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # numerator calculation part 2
  vitals_procedure_time_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_01_col }}, {{ eprocedures_01_col }}
    ) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      (
        {{ evitals_01_col }} > {{ eprocedures_01_col }}
        
      )
      
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # numerator calculation part 3
  etco2_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_16_col }}
    ) |> 
    dplyr::distinct() |> 
    dplyr::filter( 
      
      {{ evitals_16_col }} >= 5
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 8, id = progress_bar, force = T)
  
  # assign variables to final data
  initial_population <- final_data |> 
    dplyr::mutate(ENDOTRACHEAL_INTUBATION = Unique_ID %in% procedures_data,
                  WAVEFORM_ETCO2 = Unique_ID %in% waveform_ETCO2_data,
                  VITALS_PROCEDURE_TIME = Unique_ID %in% vitals_procedure_time_data,
                  ETCO2 = Unique_ID %in% etco2_data,
                  NUMERATOR = WAVEFORM_ETCO2 & 
                    VITALS_PROCEDURE_TIME & 
                    ETCO2
    ) |> 
    dplyr::filter(
      
      ENDOTRACHEAL_INTUBATION
      
    )
  
  cli::cli_progress_update(set = 9, id = progress_bar, force = T)
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  cli::cli_progress_update(set = 10, id = progress_bar, force = T)
  
  # summarize
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Airway-18",
                      population_name = "Adult",
                      NUMERATOR)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Airway-18",
                      population_name = "Peds",
                      NUMERATOR) 
  
  cli::cli_progress_update(set = 11, id = progress_bar, force = T)
  
  # summary
  airway.18 <- dplyr::bind_rows(adult_population, peds_population)
  
  cli::cli_progress_update(set = 12, id = progress_bar, force = T)
  
  airway.18
  
  cli::cli_progress_done()
  
}
