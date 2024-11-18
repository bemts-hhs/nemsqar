trauma_04 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      transport_disposition_col,
                      evitals_21_col,
                      eexam_23_col,
                      eexam_25_col,
                      evitals_15_col,
                      eprocedures_03_col,
                      evitals_12_col,
                      evitals_06_col,
                      evitals_10_col,
                      evitals_06_col,
                      einjury_03_col,
                      eexam_16_col,
                      eexam_20_col,
                      einjury_04_col,
                      einjury_09_col,
                      eresponse_10_col,
                      einjury_01_col,
                      evitals_27_last_col,
                      evitals_01_col,
                      evitals_27_sortorder_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn trauma_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn trauma_01}."
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
  
  
  # Create objects that are filter helpers throughout the function
  
  # injury values
  possible_injury <- "Yes|9922005"
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
  # GCS motor values
  
  GCS_motor_values <- "no motor response|extension to pain|flexion to pain|withdrawal from pain|localizing pain|5|4|3|2|1"
  
  # lung assessment values
  
  lung_assessment_values <- "Breath Sounds-Absent|Breath Sounds-Decreased|Increased Respiratory Effort|3523001|3523003|3523011"
  
  # chest assessment values
  
  chest_assessment_values <- 
  
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
                  ))
  
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  
  final_data <- core_data |> 
    dplyr::select(-c({{ esituation_02_col }},
                     {{ eresponse_05_col }},
                     {{ edisposition_28_col }},
                     {{ transport_disposition_col }},
                     {{ evitals_27_initial_col }},
                     {{ evitals_27_last_col }},
                     {{ evitals_01_col }}
                     
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365,
    
    # system age check
    system_age_65 = {{ epatient_15_col }} >= 65 & {{ epatient_16_col }} == "Years", 
    system_age_10_65 = ({{ epatient_15_col }} < 65 & {{ epatient_15_col }} >= 10) & {{ epatient_16_col }} == "Years", 
    system_age_10_1 = {{ epatient_15_col }} < 10 & {{ epatient_16_col }} == "Years",
    system_age_10_2 = {{ epatient_15_col }} < 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_10 = system_age_10_1 | system_age_10_2, 
    
    # calculated age check
    calc_age_65 = patient_age_in_years_col >= 65, 
    calc_age_10_65 = patient_age_in_years_col < 65 & patient_age_in_years_col >= 10,
    calc_age_10 = patient_age_in_years_col < 10
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # possible injury
  
  possible_injury_data <- core_data |> 
    dplyr::select(Unique_ID, {{ esituation_02_col }}) |> 
    dplyr::filter(grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # patient care provided
  
  patient_care_data <- core_data |> 
    dplyr::select(Unique_ID, {{ edisposition_28_col }}) |> 
    dplyr::filter(grepl(pattern = care_provided, x = {{ edisposition_28_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # 911 calls
  
  call_911_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_05_col }}) |> 
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # transports
  
  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{ transport_disposition_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T) 
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # pain scale time
  
  pain_scale_time_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_27_initial_col }}, {{ evitals_27_last_col }}, {{ evitals_01_col }}) |> 
    dplyr::filter( 
      
      dplyr::if_all(c({{ evitals_27_initial_col }}, {{ evitals_01_col }}), ~ !is.na(.))
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # pain scale change
  
  pain_scale_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_27_initial_col }}, {{ evitals_27_last_col }}) |> 
    dplyr::filter( 
      
      {{ evitals_27_last_col }} < {{ evitals_27_initial_col }}
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # pain scale sort order
  
  pain_scale_sortorder_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_27_initial_col }}, {{ evitals_27_sortorder_col }}) |> 
    dplyr::filter( 
      
      !is.na({{ evitals_27_initial_col }}) & {{ evitals_27_sortorder_col }} > 0
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(PAIN_SCALE_TIME = Unique_ID %in% pain_scale_time_data,
                  CALL_911 = Unique_ID %in% call_911_data,
                  TRANSPORT = Unique_ID %in% transport_data,
                  INJURY = Unique_ID %in% possible_injury_data,
                  PATIENT_CARE = Unique_ID %in% patient_care_data,
                  PAIN_SCALE = Unique_ID %in% pain_scale_data,
                  PAIN_SCALE_SORTORDER = Unique_ID %in% pain_scale_sortorder_data
    ) |> 
    dplyr::filter(
      
      dplyr::if_all(c(
        INJURY, PAIN_SCALE_TIME, PAIN_SCALE_SORTORDER, CALL_911, PATIENT_CARE, TRANSPORT), ~ .)
    )
  
  # Adult and Pediatric Populations
  
  # filter older adult
  pop_65 <- initial_population |>
    dplyr::filter(system_age_65 | calc_age_65)
  
  # filter ages 10 to 65
  pop_10_65 <- initial_population |>
    dplyr::filter(system_age_10_65 | calc_age_10_65)
  
  # filter ages 10 to 65
  pop_10 <- initial_population |>
    dplyr::filter(system_age_10 | calc_age_10)
  
  # summarize
  
  # total population
  
  population_65 <- pop_65 |> 
    dplyr::summarize(
      measure = "Trauma-04",
      pop = ">= 65 yrs old",
      numerator = sum({{numerator_col}}, na.rm=T),
      denominator = dplyr::n(),
      prop = sum(numerator / denominator),
      prop_label = pretty_percent(prop,
                                  n_decimal = 0.01),
      ...
    )
  
  # adults
  population_10_65 <- pop_10_65 |>
    dplyr::summarize(
      measure = "Trauma-04",
      pop = "10-65 yrs",
      numerator = sum({{numerator_col}}, na.rm=T),
      denominator = dplyr::n(),
      prop = sum(numerator / denominator),
      prop_label = pretty_percent(prop,
                                  n_decimal = 0.01),
      ...
    )
  
  # peds
  population_10 <- pop_10 |>
    dplyr::summarize(
      measure = "Trauma-04",
      pop = "< 10 yrs",
      numerator = sum({{numerator_col}}, na.rm=T),
      denominator = dplyr::n(),
      prop = sum(numerator / denominator),
      prop_label = pretty_percent(prop,
                                  n_decimal = 0.01),
      ...
    )
  
  # summary
  trauma.04 <- dplyr::bind_rows(population_65, population_10_65, population_10)
  
  trauma.04
  
  
}


