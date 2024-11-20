airway_01 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      earrest_01_col,
                      eresponse_05_col,
                      evitals_01_col,
                      evitals_06_col,
                      evitals_12_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn airway_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn airway_01}."
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
  
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # endotracheal intubation attempts
  
  endotracheal_intubation <- "673005|Indirect laryngoscopy \\(procedure\\)|49077009|Flexible fiberoptic laryngoscopy \\(procedure\\)|78121007|Direct laryngoscopy \\(procedure\\)|112798008|Insertion of endotracheal tube \\(procedure\\)|16883004|Endotracheal intubation, emergency procedure \\(procedure\\)|182682004|Emergency laryngeal intubation \\(procedure\\)|232674004|Orotracheal intubation \\(procedure\\)|232677006|Tracheal intubation using rigid bronchoscope \\(procedure\\)|232678001|Orotracheal fiberoptic intubation \\(procedure\\)|232679009|Nasotracheal intubation \\(procedure\\)|232682004|Nasotracheal fiberoptic intubation \\(procedure\\)|232680007|Nasal intubation awake \\(procedure\\)|241689008|Intubation, Rapid Sequence Intubation \\(RSI\\) \\(procedure\\)|304341005|Awake intubation \\(procedure\\)|397892004|Retrograde intubation \\(procedure\\)|429161001|Insertion of endotracheal tube using laryngoscope \\(procedure\\)|450601000124103|Orotracheal intubation using bougie device \\(procedure\\)|1141752008|Flexible video intubation laryngoscope \\(physical object\\)|285696003|Fiberoptic laryngoscope \\(physical object\\)|420311007|Flexible fiberoptic laryngoscope \\(physical object\\)|421100004|Rigid fiberoptic laryngoscope \\(physical object\\)|44738004|Laryngoscope device \\(physical object\\)|469919007|Flexible video laryngoscope \\(physical object\\)|700640001|Rigid intubation laryngoscope \\(physical object\\)|701054002|Flexible fiberoptic intubation laryngoscope \\(physical object\\)|706013009|Intubation laryngoscope \\(physical object\\)|734928009|Rigid non-bladed video intubation laryngoscope \\(physical object\\)|879788006|Channeled video intubation laryngoscope \\(physical object\\)"

  
  # cardiac arrest response
  
  cardiac_arrest_response <- "3001003|Yes, Prior to Any EMS Arrival"
  
  # minor values
  minor_values <- "days|hours|minutes|months"
  
  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________
  
  core_data <- df |> 
    dplyr::mutate(INCIDENT_DATE_MISSING = tidyr::replace_na({{ incident_date_col }}, base::as.Date("1984-09-09")),
                  PATIENT_DOB_MISSING = tidyr::replace_na({{ patient_DOB_col }}, base::as.Date("1982-05-19")),
                  Unique_ID = stringr::str_c({{ erecord_01_col }},
                                             INCIDENT_DATE_MISSING,
                                             PATIENT_DOB_MISSING, 
                                             sep = "-"
                  ),
                  
                  ENDOTRACHEAL_INTUBATION = grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T),
                  CALL_911 = grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T),
                  CARDIAC_ARREST = !grepl(pattern = cardiac_arrest_response, x = {{ earrest_01_col }}, ignore.case = T)
                  
                  ) |> 
    dplyr::filter(
      ENDOTRACHEAL_INTUBATION,
      CALL_911,
      CARDIAC_ARREST
    )
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ earrest_01_col }},
                     {{ eresponse_05_col }},
                     {{ evitals_01_col }},
                     {{ evitals_06_col }},
                     {{ evitals_12_col }},
                     {{ eprocedures_01_col }},
                     {{ eprocedures_02_col }},
                     {{ eprocedures_03_col }},
                     {{ eprocedures_05_col }},
                     {{ eprocedures_06_col }}
                     
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
    system_age_exclusion1 = {{ epatient_15_col }} < 1 & {{ epatient_16_col }} == "Days",
    system_age_exclusion2 = {{ epatient_15_col }} < 24 & {{ epatient_16_col }} == "Hours",
    system_age_exclusion3 = {{ epatient_15_col }} < 120 & {{ epatient_16_col }} == "Minutes",
    system_age_exclusion = system_age_exclusion1 | system_age_exclusion2 | system_age_exclusion3,
    system_age_minor = (system_age_minor1 | system_age_minor2) & !system_age_exclusion, 
    
    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_exclusion = patient_age_in_days_col < 1,
    calc_age_minor = patient_age_in_years_col < 18 & !calc_age_exclusion
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # no transports
  
  no_transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      grepl(pattern = no_transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # cardiac arrest
  
  cardiac_arrest_data <- core_data |> 
    dplyr::select(Unique_ID, {{ earrest_01_col }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter( 
      
      !grepl(pattern = cardiac_arrest_response, x = {{ earrest_01_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # SBP, DBP, HR, RR
  
  vitals_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }},
                  {{ evitals_12_col }}, {{ evitals_14_col }}, {{ evitals_23_col }}, {{ evitals_26_col }}
    ) |> 
    dplyr::filter( 
      
      dplyr::if_all(
        c({{ evitals_06_col }}, {{ evitals_07_col }}, {{ evitals_10_col }}, {{ evitals_12_col }}, {{ evitals_14_col }}), ~ !is.na(.)
      ),
      
      !is.na({{ evitals_23_col }}) | 
        
        grepl(pattern = avpu_responses, x = {{ evitals_26_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(CALL_911 = Unique_ID %in% call_911_data,
                  NO_TRANSPORT = Unique_ID %in% no_transport_data,
                  CARDIAC_ARREST = Unique_ID %in% cardiac_arrest_data,
                  VITALS = Unique_ID %in% vitals_data,
    ) |> 
    dplyr::filter(
      CALL_911,
      NO_TRANSPORT,
      CARDIAC_ARREST
    )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # summarize
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "TTR_01",
                      population_name = "Adult",
                      VITALS,
                      ...)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "TTR_01",
                      population_name = "Peds",
                      VITALS,
                      ...) 
  # summary
  ttr.01 <- dplyr::bind_rows(adult_population, peds_population)
  
  ttr.01
  
  
}
