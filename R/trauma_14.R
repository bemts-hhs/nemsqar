trauma_14 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      transport_disposition_col,
                      evitals_21_col,
                      evitals_14_col,
                      eexam_23_col,
                      eexam_25_col,
                      evitals_15_col,
                      eprocedures_03_col,
                      evitals_12_col,
                      evitals_06_col,
                      evitals_10_col,
                      einjury_03_col,
                      eexam_16_col,
                      eexam_20_col,
                      einjury_04_col,
                      einjury_09_col,
                      eresponse_10_col,
                      einjury_01_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn trauma_14}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn trauma_14}."
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
  chest_assessment_values <- "3525005|Accessory Muscles Used with Breathing|3525023|Flail Segment|3525039|Retraction"
  
  # respiratory effort values
  respiratory_effort_values <- "Apneic|Labored|Mechanically Assisted \\(BVM, CPAP, etc\\.\\)|Rapid|Shallow|Weak/Agonal|3315001|3315003|3315005|3315009|3315011|3315013"
  
  # airway management values
  airway_management_values <- "243142003|Dual pressure spontaneous ventilation support \\(regime/therapy\\)|47545007|Continuous positive airway pressure ventilation treatment \\(regime/therapy\\)|429705000|Insertion of esophageal tracheal combitube \\(procedure\\)|427753009|Insertion of esophageal tracheal double lumen supraglottic airway \\(procedure\\)|424979004|Laryngeal mask airway insertion \\(procedure\\)|23674004|Orotracheal intubation \\(procedure\\)|450601000124103|Orotracheal intubation using bougie device \\(procedure\\)|241689008|Rapid sequence induction \\(procedure\\)|450611000124100|Insertion of single lumen supraglottic airway device \\(procedure\\)"
  
  
  # trauma triage criteria values for 65+ age group
  trauma_triage_1_2_values_65 <- "2903001|Amputation proximal to wrist or ankle|3903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity \\(e.g., flail chest\\)|2903009|Open or depressed skull fracture|2903011|Paralysis|3903013|Pelvic fractures|2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1\\) or need for ventilatory support|3903021|Two or more proximal long-bone fractures"
  
  # trauma triage criteria values
  trauma_triage_1_2_values_10_65 <- "2903001|Amputation proximal to wrist or ankle|3903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity \\(e.g., flail chest\\)|2903009|Open or depressed skull fracture|2903011|Paralysis|3903013|Pelvic fractures|2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1\\) or need for ventilatory support|3903021|Two or more proximal long-bone fractures|2903019|Systolic Blood Pressure <90 mmHg"
  
  # trauma triage criteria values for < 10 age group
  trauma_triage_1_2_values_10 <- "2903001|Amputation proximal to wrist or ankle|3903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity \\(e.g., flail chest\\)|2903009|Open or depressed skull fracture|2903011|Paralysis|3903013|Pelvic fractures|2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|3903021|Two or more proximal long-bone fractures"
  
  # extremities assessment values
  extremities_assessment_values <- "3516043|Motor Function-Abnormal/Weakness|3516067|Sensation-Absent"
  
  # neurological assessment values
  neurological_assessment_values <- "3520017|Hemiplegia-Left|3520019|Hemiplegia-Right|3520043|Weakness-Left Sided|3520045|Weakness-Right Sided"
  
  # procedures values
  tourniquet_values <- "20655006|Application of tourniquet \\(procedure\\)|24173005|Tourniquet procedure \\(procedure\\)|241731009|Tourniquet positioning \\(uninflated\\) \\(procedure\\)|241733007|Tourniquet cuff inflation \\(procedure\\)|241734001|Upper tourniquet cuff inflation \\(procedure\\)|241735000|Lower tourniquet cuff inflation \\(procedure\\)|241736004|Manual tourniquet application \\(procedure\\)|398260007|Tourniquet positioned on patient \\(procedure\\)|447686008|Application of pressure to wound \\(procedure\\)"
  
  # trauma triage criteria (steps 3 and 4) values
  trauma_triage_3_4_values <- "2904001|Auto v\\. Pedestrian/Bicyclist Thrown, Run Over, or >20 MPH Accident|2904007|Crash Death in Same Passenger Compartment|2904009|Crash Ejection \\(partial or complete\\) from automobile|2904011|Crash Intrusion, Including roof: > 12 in\\. occupant site; > 18 in\\. any site|2904013|Crash Vehicle Telemetry Data \\(AACN\\) Consistent with High Risk of Injury"
  
  # type of scene delay values
  scene_delay_values <- "2210011|Extrication"
  
  # cause of injury matches values
  cause_of_injury_values <- "\\b(V20|V21|V22|V23|V24|V25|V26|V27|V28|V29|V30|V31|V32|V33|V34|V35|V36|V37|V38|V39|V80|V86)\\b|Motorcycle rider injured in collision with pedestrian or animal|Motorcycle rider injured in collision with pedal cycle|Motorcycle rider injured in collision with two- or three- wheeled motor vehicle|Motorcycle rider injured in collision with car, pick-up truck or van|Motorcycle rider injured in collision with heavy transport vehicle or bus|Motorcycle rider injured in collision with railway train or railway vehicle|Motorcycle rider injured in collision with other nonmotor vehicle|Motorcycle rider injured in collision with fixed or stationary object|Motorcycle rider injured in noncollision transport accident|Motorcycle rider injured in other and unspecified transport accidents|Occupant of three-wheeled motor vehicle injured in collision with pedestrian or animal|Occupant of three-wheeled motor vehicle injured in collision with pedal cycle|Occupant of three-wheeled motor vehicle injured in collision with two- or three- wheeled motor vehicle|Occupant of three-wheeled motor vehicle injured in collision with car, pick-up truck or van|Occupant of three-wheeled motor vehicle injured in collision with heavy transport vehicle or bus|Occupant of three-wheeled motor vehicle injured in collision with railway train or railway vehicle|Occupant of three-wheeled motor vehicle injured in collision with other nonmotor vehicle|Occupant of three-wheeled motor vehicle injured in collision with fixed or stationary object|Occupant of three-wheeled motor vehicle injured in noncollision transport accident|Occupant of three-wheeled motor vehicle injured in other and unspecified transport accidents|Animal-rider or occupant of animal drawn vehicle injured in transport accident|Occupant of special all-terrain or other off-road motor vehicle, injured in transport accident"
  
  # hospital capability values 
  hospital_capability_values <- "9908021|Trauma Center Level 1|9908023|Trauma Center Level 2|9908025|Trauma Center Level 3|9908027|Trauma Center Level 4|9908029|Trauma Center Level 5"
  
  
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
    dplyr::select(-c({{ erecord_01_col }},
                     {{ incident_date_col }},
                     {{ patient_DOB_col }},
                     {{ epatient_15_col }},
                     {{ epatient_16_col }},
                     {{ esituation_02_col }},
                     {{ eresponse_05_col }},
                     {{ transport_disposition_col }},
                     {{ evitals_21_col }},
                     {{ evitals_14_col }},
                     {{ eexam_23_col }},
                     {{ eexam_25_col }},
                     {{ evitals_15_col }},
                     {{ eprocedures_03_col }},
                     {{ evitals_12_col }},
                     {{ evitals_06_col }},
                     {{ evitals_10_col }},
                     {{ einjury_03_col }},
                     {{ eexam_16_col }},
                     {{ eexam_20_col }},
                     {{ einjury_04_col }},
                     {{ einjury_09_col }},
                     {{ eresponse_10_col }},
                     {{ einjury_01_col }}
                     
                     
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
    calc_age_10 = patient_age_in_years_col < 10,
    
    # SBP check variable for ages < 10 years
    SBP_age_10 = {{ evitals_06_col }} + ({{ epatient_15_col }} * 2)
    ) |> 
    filter(grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T),
           grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T),
           grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T)
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  # GCS 
  GCS_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_21_col }}) |> 
    dplyr::filter(grepl(pattern = care_provided, x = {{ evitals_21_col }}, ignore.case = T)) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # lung assessment
  lung_assessment_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eexam_23_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = lung_assessment_values, x = {{ eexam_23_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # chest assessment
  chest_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eexam_25_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = chest_assessment_values, x = {{ eexam_25_col }})
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # respiratory effort
  respiratory_effort_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_15_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = respiratory_effort_values, x = {{ evitals_15_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # airway management
  airway_management_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eprocedures_03_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = airway_management_values, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # pulse oximetry
  pulse_oximetry_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_12_col }}) |> 
    dplyr::filter( 
      
      {{ evitals_12_col }} < 90
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # SBP
  SBP_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_06_col }}) |> 
    dplyr::filter( 
      
      {{ evitals_06_col }} < 110
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # heart rate and SBP
  HR_SBP_data_10_65_plus <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_12_col }}, {{ evitals_06_col}}) |> 
    dplyr::distinct(Unique_ID, {{ evitals_12_col }}, {{ evitals_06_col}}, .keep_all = T) |> 
    dplyr::filter( 
      
      {{ evitals_10_col }} > {{ evitals_06_col}}
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # trauma triage criteria steps 1 and 2 age 65+
  trauma_triage_1_2_data_65 <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_03_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = trauma_triage_1_2_values_65, x = {{ einjury_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # trauma triage criteria steps 1 and 2 age 10 - 65
  trauma_triage_1_2_data_10_65 <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_03_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = trauma_triage_1_2_values_10_65, x = {{ einjury_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # trauma triage criteria steps 1 and 2 age < 10
  trauma_triage_1_2_data_10 <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_03_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = trauma_triage_1_2_values_10, x = {{ einjury_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # extremities assessment
  extremities_assessment_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eexam_16_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = extremities_assessment_values, x = {{ eexam_16_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # neurological assessment
  
  neurological_assessment_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eexam_20_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = neurological_assessment_values, x = {{ eexam_20_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # tourniquet 
  
  tourniquet_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eprocedures_03_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = tourniquet_values, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # trauma triage criteria steps 3 and 4
  
  trauma_triage_3_4_data <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_04_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = trauma_triage_3_4_values, x = {{ einjury_04_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # fall height
  
  fall_height_data <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_09_col }}) |> 
    dplyr::filter( 
      
      {{ einjury_09_col }} > 10
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # scene delay
  
  scene_delay_data <- core_data |> 
    dplyr::select(Unique_ID, {{ eresponse_10_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = scene_delay_values, x = {{ eresponse_10_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # cause of injury
  
  cause_of_injury_data <- core_data |> 
    dplyr::select(Unique_ID, {{ einjury_01_col }}) |> 
    dplyr::filter( 
      
      grepl(pattern = cause_of_injury_values, x = {{ einjury_01_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # respiratory rate for < 10 yrs population
  
  respiratory_rate_data <- core_data |> 
    dplyr::select(Unique_ID, {{ evitals_14_col }}) |> 
    dplyr::filter( 
      
      {{ evitals_14_col }} < 10 | {{ evitals_14_col }} > 29
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # SBP for age < 10 yrs
  
  SBP_data_10 <- core_data |> 
    dplyr::select(Unique_ID, SBP_age_10) |> 
    dplyr::filter( 
      
      SBP_age_10 > 70
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(
      
      GCS = Unique_ID %in% GCS_data,
      LUNG = Unique_ID %in% lung_assessment_data,
      CHEST = Unique_ID %in% chest_data,
      RESPIRATORY_EFFORT = Unique_ID %in% respiratory_effort_data,
      AIRWAY_MANAGEMENT = Unique_ID %in% airway_management_data,
      EXTREMITIES = Unique_ID %in% extremities_assessment_data,
      NEURO = Unique_ID %in% neurological_assessment_data,
      TOURNIQUET = Unique_ID %in% tourniquet_data,
      TRAUMA_TRIAGE_3_4 = Unique_ID %in% trauma_triage_3_4_data,
      FALL_HEIGHT = Unique_ID %in% fall_height_data,
      SCENE_DELAY = Unique_ID %in% scene_delay_data,
      INJURY_CAUSE = Unique_ID %in% cause_of_injury_data,
      PULSE_OXIMETRY = Unique_ID %in% pulse_oximetry_data,
      SBP = Unique_ID %in% SBP_data,
      SBP_10 = Unique_ID %in% SBP_data_10,
      HR_SBP_10_65_PLUS = Unique_ID %in% HR_SBP_data_10_65_plus,
      TRAUMA_TRIAGE_1_2_65 = Unique_ID %in% trauma_triage_1_2_data_65,
      TRAUMA_TRIAGE_1_2_10_65 = Unique_ID %in% trauma_triage_1_2_data_10_65,
      TRAUMA_TRIAGE_1_2_10 = Unique_ID %in% trauma_triage_1_2_data_10,
      RESPIRATORY_RATE_10 = Unique_ID %in% respiratory_rate_data,
      HOSPITAL_CAPABILITY = Unique_ID %in% hospital_capability_values
      
    )
  
  # Adult and Pediatric Populations
  
  # filter older adult
  pop_65 <- initial_population |>
    dplyr::filter(system_age_65 | calc_age_65) |> 
    dplyr::filter(
      
      GCS | 
        LUNG | 
        CHEST | 
        RESPIRATORY_EFFORT | 
        AIRWAY_MANAGEMENT | 
        PULSE_OXIMETRY | 
        HR_SBP_10_65_PLUS | 
        TRAUMA_TRIAGE_1_2_10_65 | 
        EXTREMITIES | 
        NEURO | 
        TOURNIQUET |
        TRAUMA_TRIAGE_3_4 | 
        FALL_HEIGHT | 
        SCENE_DELAY | 
        INJURY_CAUSE
      
    )
  
  # filter ages 10 to 65
  pop_10_65 <- initial_population |>
    dplyr::filter(system_age_10_65 | calc_age_10_65) |> 
    dplyr::filter(
      
      GCS | 
        LUNG | 
        CHEST | 
        RESPIRATORY_EFFORT | 
        AIRWAY_MANAGEMENT | 
        HR_SBP_10_65_PLUS | 
        TRAUMA_TRIAGE_1_2_10_65 | 
        EXTREMITIES | 
        NEURO | 
        TOURNIQUET |
        TRAUMA_TRIAGE_3_4 | 
        FALL_HEIGHT | 
        SCENE_DELAY | 
        INJURY_CAUSE
      
    )
  
  # filter ages < 10
  pop_10 <- initial_population |>
    dplyr::filter(system_age_10 | calc_age_10) |> 
    dplyr::filter(
      
      GCS | 
        RESPIRATORY_RATE_10 |
        LUNG | 
        CHEST | 
        RESPIRATORY_EFFORT | 
        AIRWAY_MANAGEMENT | 
        PULSE_OXIMETRY | 
        SBP_10 | 
        TRAUMA_TRIAGE_1_2_10 | 
        EXTREMITIES | 
        NEURO | 
        TOURNIQUET |
        TRAUMA_TRIAGE_3_4 | 
        FALL_HEIGHT | 
        SCENE_DELAY | 
        INJURY_CAUSE
      
    )
  
  # summarize
  
  # older adult population
  
  population_65 <- pop_65 |> 
    summarize_measure(measure_name = "Trauma-04",
                      population_name = ">= 65 yrs old",
                      numerator_col = HOSPITAL_CAPABILITY,
                      ...
    )
  
  # 10 to 64 population
  population_10_65 <- pop_10_65 |>
    summarize_measure(measure_name = "Trauma-04",
                      population_name = "10-65 yrs",
                      numerator_col = HOSPITAL_CAPABILITY,
                      ...
    )
  
  # patients < 10 yrs
  population_10 <- pop_10 |>
    summarize_measure(measure_name = "Trauma-04",
                      population_name = "< 10 yrs",
                      numerator_col = HOSPITAL_CAPABILITY,
                      ...
    )
  
  # summary
  trauma.14 <- dplyr::bind_rows(population_65, population_10_65, population_10)
  
  trauma.14
  
  
}


