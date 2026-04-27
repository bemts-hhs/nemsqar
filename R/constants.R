#' Internal regex and code patterns for nemsqar
#'
#' This file contains internal constant objects used throughout the package.
#' These include regular expression patterns and lookup values that support
#' EMS clinical logic within nemsqar. Centralizing these values improves
#' maintainability and prevents duplication across functions.
#'
#' All objects defined in this file are internal and are not exported.
#'
#' @keywords internal
#' @noRd

# 911 codes for eResponse.05 ----
codes_911 <- paste(
  "2205001",
  "2205003",
  "2205009",
  "Emergency Response \\(Primary Response Area\\)",
  "Emergency Response \\(Intercept\\)",
  "Emergency Response \\(Mutual Aid\\)",
  "911 Response \\(Scene\\)",
  "Intercept",
  "Mutual Aid",
  sep = "|"
)

# procedure codes for airway-01 ----
procedures_code_airway_01 <- paste(
  "673005|Indirect laryngoscopy",
  "49077009|Flexible fiberoptic laryngoscopy",
  "78121007|Direct laryngoscopy",
  "112798008|Insertion of endotracheal tube",
  "16883004|Endotracheal intubation, emergency procedure",
  "182682004|Emergency laryngeal intubation",
  "232674004|Orotracheal intubation",
  "232677006|Tracheal intubation using rigid bronchoscope",
  "232678001|Orotracheal fiberoptic intubation",
  "232679009|Nasotracheal intubation",
  "232682004|Nasotracheal fiberoptic intubation",
  "232680007|Nasal intubation awake",
  "241689008|Intubation, Rapid Sequence Intubation (RSI)",
  "304341005|Awake intubation",
  "397892004|Retrograde intubation",
  "429161001|Insertion of endotracheal tube using laryngoscope",
  "450601000124103|Orotracheal intubation using bougie device",
  "1141752008|Flexible video intubation laryngoscope",
  "285696003|Fiberoptic laryngoscope",
  "420311007|Flexible fiberoptic laryngoscope",
  "421100004|Rigid fiberoptic laryngoscope",
  "44738004|Laryngoscope device",
  "469919007|Flexible video laryngoscope",
  "700640001|Rigid intubation laryngoscope",
  "701054002|Flexible fiberoptic intubation laryngoscope",
  "706013009|Intubation laryngoscope",
  "734928009|Rigid non-bladed video intubation laryngoscope",
  "879788006|Channeled video intubation laryngoscope",
  sep = "|"
)

# procedure codes for airway-05 ----
procedures_code_airway_05 <- paste(
  "673005|Indirect laryngoscopy",
  "49077009|Flexible fiberoptic laryngoscopy",
  "78121007|Direct laryngoscopy",
  "112798008|Insertion of endotracheal tube",
  "16883004|Endotracheal intubation, emergency procedure",
  "182682004|Emergency laryngeal intubation",
  "232674004|Orotracheal intubation",
  "232677006|Tracheal intubation using rigid bronchoscope",
  "232678001|Orotracheal fiberoptic intubation",
  "232679009|Nasotracheal intubation",
  "232682004|Nasotracheal fiberoptic intubation",
  "232680007|Nasal intubation awake",
  "241689008|Intubation, Rapid Sequence Intubation (RSI)",
  "304341005|Awake intubation",
  "397892004|Retrograde intubation",
  "429161001|Insertion of endotracheal tube using laryngoscope",
  "450601000124103|Orotracheal intubation using bougie device",
  "1141752008|Flexible video intubation laryngoscope",
  "285696003|Fiberoptic laryngoscope",
  "420311007|Flexible fiberoptic laryngoscope",
  "421100004|Rigid fiberoptic laryngoscope",
  "44738004|Laryngoscope device",
  "469919007|Flexible video laryngoscope",
  "700640001|Rigid intubation laryngoscope",
  "701054002|Flexible fiberoptic intubation laryngoscope",
  "706013009|Intubation laryngoscope",
  "734928009|Rigid non-bladed video intubation laryngoscope",
  "879788006|Channeled video intubation laryngoscope",
  sep = "|"
)

# endotracheal intubation attempts ----
endotracheal_intubation_airway_18 <- paste(
  "673005|Indirect laryngoscopy",
  "49077009|Flexible fiberoptic laryngoscopy",
  "78121007|Direct laryngoscopy",
  "112798008|Insertion of endotracheal tube",
  "16883004|Endotracheal intubation, emergency procedure",
  "182682004|Emergency laryngeal intubation",
  "232674004|Orotracheal intubation",
  "232677006|Tracheal intubation using rigid bronchoscope",
  "232678001|Orotracheal fiberoptic intubation",
  "232679009|Nasotracheal intubation",
  "232682004|Nasotracheal fiberoptic intubation",
  "232680007|Nasal intubation awake",
  "241689008|Rapid sequence induction",
  "304341005|Awake intubation",
  "397892004|Retrograde intubation",
  "418613003|Tracheal intubation through a laryngeal mask airway",
  "429705000|Intubation, combitube",
  "424979004|Laryngeal mask airway insertion",
  "427753009|Insertion of esophageal tracheal double lumen supraglottic airway",
  "429161001|Insertion of endotracheal tube using laryngoscope",
  "450601000124103|Orotracheal intubation using bougie device",
  "450611000124|Insertion of Single Lumen Supraglottic Airway Device",
  "1141752008|Flexible video intubation laryngoscope",
  "285696003|Fiberoptic laryngoscope",
  "420311007|Flexible fiberoptic laryngoscope",
  "421100004|Rigid fiberoptic laryngoscope",
  "44738004|Laryngoscope device",
  "469919007|Flexible video laryngoscope",
  "700640001|Rigid intubation laryngoscope",
  "701054002|Flexible fiberoptic intubation laryngoscope",
  "706013009|Intubation laryngoscope",
  "734928009|Rigid non-bladed video intubation laryngoscope",
  "879788006|Channeled video intubation laryngoscope",
  sep = "|"
)

# additional procedures in the exclusion ----
airway_procedures_safety_04 <- paste(
  "16883004",
  "Endotracheal intubation, emergency procedure",
  "182682004",
  "Emergency laryngeal intubation",
  "232674004",
  "Orotracheal intubation",
  "232678001",
  "Orotracheal fiberoptic intubation",
  "232682004",
  "Nasotracheal fiberoptic intubation",
  "232685002",
  "Insertion of tracheostomy tube",
  "304341005",
  "Awake intubation",
  "418613003",
  "Tracheal intubation through a laryngeal mask airway",
  "424979004",
  "Laryngeal mask airway insertion",
  "427753009",
  "Insertion of esophageal tracheal double lumen supraglottic airway",
  "429161001",
  "Insertion of endotracheal tube using laryngoscope",
  "450611000124",
  "Insertion of Single Lumen Supraglottic Airway Device",
  sep = "|"
)

# get codes as a regex to filter primary/secondary impression fields ----
beta_agonist <- paste(
  "435",
  "7688",
  "214199",
  "237159",
  "487066",
  "1154062",
  "1163444",
  "1649559",
  "1165719",
  "2108209",
  "2108252",
  "albuterol",
  "ipratropium",
  "levalbuterol",
  "metaproterenol",
  sep = "|"
)

# codes for asthma or acute bronchospasm ----
asthma_codes <- "(?:J45|J98.01)|asthma|acute bronchospasm"

# waveform ETCO2 ----
waveform_etc02_codes <- "4004019|Waveform ETCO2"

# get codes as a regex to filter primary/secondary impression fields ----
hypoglycemia_treatment_codes <- paste(
  "4832",
  "4850",
  "377980",
  "376937",
  "372326",
  "237653",
  "260258",
  "309778",
  "1795610",
  "1795477",
  "1794567",
  "1165823",
  "1165822",
  "1165819",
  "Glucagon",
  "Glucose",
  "Glucose Oral Gel",
  "Glucose Injectable Solution",
  "Glucose Chewable Tablet",
  "Glucose 500 MG/ML Injectable Solution",
  "Glucose 250 MG/ML Injectable Solution",
  "Glucose 50 MG/ML Injectable Solution",
  "250 ML Glucose 50 MG/ML Injection",
  "500 ML Glucose 100 MG ML Injection",
  "Glucose Injection",
  "Glucose Oral Product",
  "Glucose Oral Liquid Product",
  "Glucose Injectable Product",
  sep = "|"
)

# hypoglycemia procedures ----
hypoglycemia_procedure_codes <- paste(
  "710925007",
  "225285007",
  "Provision of food",
  "Giving oral fluid",
  sep = "|"
)

# altered mental status ----
altered_mental_status <- paste(
  "R41.82",
  "Altered Mental Status, unspecified",
  sep = "|"
)

# diabetes impression codes ----
diabetes_codes <- paste(
  "(?:E13.64|E16.2)",
  "Other specified diabetes mellitus with hypoglycemia",
  "Hypoglycemia, unspecified",
  sep = "|"
)

# non-weight-based medications ----
non_weight_based_meds <- paste(
  "inhalation",
  "topical",
  "9927049",
  "9927009",
  sep = "|"
)


# respiratory impression codes ----
resp_codes <- paste(
  "(?:",
  paste(
    "I50.9",
    "J00",
    "J05",
    "J18.9",
    "J20.9",
    "J44.1",
    "J45.901",
    "J80",
    "J81",
    "J93.9",
    "J96",
    "J98.01",
    "J98.9",
    "R05",
    "R06",
    "R09.2",
    "T17.9",
    sep = "|"
  ),
  ")",
  sep = ""
)

# oxygen ----
oxygen_values <- "7806|Oxygen"

# oxygen therapy ----
oxygen_therapy_values <- "57485005|Oxygen Therapy"

# not values for meds ----
not_med <- paste(
  "8801001",
  "8801003",
  "8801009",
  "8801019",
  "8801027",
  "Contraindication Noted",
  "Denied by Order",
  "Medication Already Taken",
  "Refused",
  "Order Criteria Not Met",
  sep = "|"
)

# not values for procedures ----
not_proc <- paste(
  "8801001",
  "8801023",
  "8801003",
  "8801027",
  "8801019",
  "Contraindicated Noted",
  "Unable to Complete",
  "Denied By Order",
  "Order Criteria Not Met",
  "Refused",
  sep = "|"
)

# get codes as a regex to find lights and siren responses (eresponse_24) ----
response_no_lights_and_sirens <- "No Lights or Sirens|2224019"

# get codes as a regex to find lights and siren responses (edisposition_18) ----
disposition_no_lights_and_sirens <- "4218015|No Lights or Sirens"

# patient evaluation care ----
patient_care <- "4228001|Patient Evaluated and Care Provided"

# get codes as a regex to find cardiac arrest responses ----
cardiac_arrest_responses <- paste(
  "3001005",
  "3001003",
  "Yes, Prior to Any EMS Arrival",
  "Yes, After Any EMS Arrival",
  sep = "|"
)

# get applicable trauma triage codes for steps 1 and 2 ----
trauma_triage_crit_safety_04 <- paste(
  "2903001",
  "Amputation proximal to wrist or ankle",
  "2903003",
  "Crushed, degloved, mangled, or pulseless extremity",
  "2903005",
  "Chest wall instability or deformity",
  "2903007",
  "Glasgow Coma Score <=13",
  "2903009",
  "Open or depressed skull fracture",
  "2903011",
  "Paralysis",
  "2903013",
  "Pelvic fractures",
  "2903015",
  "All penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee",
  "2903017",
  "Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1 year\\) or need for ventilatory support",
  "3903019",
  "Systolic Blood Pressure <90 mmHg",
  "2903021",
  "Two or more long-bone fractures",
  sep = "|"
)

# car seat code for edisposition.14 ----
car_seat <- "4214001|Car Seat"

# procedure exclusion related to long board ----
long_board <- "450591000124106|Immobilization using long board"

# get codes as a regex to filter primary/secondary impression fields ----
epilepsy_pattern <- paste(
  # Base epilepsy terms, excluding "without"
  "(?:\\bepilep(sy|tic)\\b)(?!.*without)(?:status\\sepilepticus)?",

  # Neuro or seizure terms linked to status epilepticus
  "(?:neuro|seizure)(?!.*without).*status\\sepilepticus",

  # Other seizure category
  "other\\sseizure",

  # ICD-10 G40 codes, excluding specific subcodes
  "G40(?!\\.[a-z\\d]\\d[249])",

  sep = "|"
)

# medication values for seizure_02 ----
seizure_medication_pattern <- paste(
  "3322",
  "6960",
  "203128",
  "6470",
  "diazepam",
  "midazolam",
  "midazolam hydrochloride",
  "lorazepam",

  sep = "|"
)

# define transports ----
transport_responses <- paste(
  "Transport by This EMS Unit \\(This Crew Only\\)",
  "Transport by This EMS Unit, with a Member of Another Crew",
  "Transport by Another EMS Unit, with a Member of This Crew",
  "Patient Treated, Transported by this EMS Unit",
  "Patient Treated, Transported with this EMS Crew in Another Vehicle",
  "Treat / Transport ALS by this unit",
  "Treat / Transport BLS by this unit",
  "Mutual Aid Tx & Transport",
  "4212033",
  "4230001",
  "4230003",
  "4230007",
  "itDisposition\\.112\\.116",
  "it4212\\.142",
  "itDisposition\\.112\\.165",
  "itDisposition\\.112\\.141",
  "itDisposition\\.112\\.142",
  sep = "|"
)

# define transports ----
no_transport_responses <- "4230009|patient refused transport|no transport|4230013"

# transport code eresponse.05 ----
interfacility_transport_code <- "2205005|Interfacility Transport"

# primary and secondary provider impression values ----
stroke_pattern <- paste(
  "(?:I6[013]|G4[56])",
  "Nontraumatic subarachnoid hemorrhage",
  "Nontraumatic intracerebral hemorrhage",
  "Cerebral infarction",
  "Transient cerebral ischemic attacks",
  "Vascular syndromes of brain in cerebrovascular diseases",
  sep = "|"
)

# AVPU unresponsive ----
avpu_unresponsive <- "3326007|Unresponsive"

# avpu alert values ----
avpu_alert <- "Alert|3326001"

# AVPU responses ----
avpu_responses <- "Unresponsive|Verbal|Painful|3326003|3326005|3326007"

# stroke score not values ----
stroke_values <- "positive|negative|non-conclusive"

# stroke scale_values ----
stroke_scale_values <- paste(
  "F\\.A\\.S\\.T\\. Exam",
  "Miami Emergency Neurologic Deficit \\(MEND\\)",
  "Cincinnati",
  "Other Stroke Scale Type",
  "NIH",
  "Los Angeles",
  "RACE \\(Rapid Arterial Occlusion Evaluation\\)",
  "Los Angeles Motor Score \\(LAMS\\)",
  "Massachusetts",
  sep = "|"
)

# primary and secondary provider impression values ----
syncope_pattern <- paste(
  "(?:R(?:55|40.4))",
  "Syncope and collapse",
  "Transient alteration of awareness",
  sep = "|"
)

# ECG pattern ----
ecg_pattern <- paste(
  "12 Lead-Left Sided \\(Normal\\)",
  "12 Lead-Right Sided",
  "15 Lead",
  "18 Lead",
  "3304007",
  "3304009",
  "3304011",
  "3304013",
  sep = "|"
)

# TBI injuries ----
tbi_injuries <- paste(
  "(?:S02)",
  "(?:S04\\.4)",
  "(?:S06)",
  "(?:S06\\.X9)",
  "(?:S06\\.0)",
  "(?:S07\\.1)",
  "(?:S09\\.90)",
  "(?:T74\\.4)",
  sep = "|"
)

# injury values ----
possible_injury <- "Yes|9922005"

# GCS motor values ----
#' @keywords internal
GCS_motor_values <- paste(
  "no motor response",
  "extension to pain",
  "flexion to pain",
  "withdrawal from pain",
  "localizing pain",
  "5",
  "4",
  "3",
  "2",
  "1",
  sep = "|"
)

# lung assessment values ----
lung_assessment_values <- paste(
  "Breath Sounds-Absent",
  "Breath Sounds-Decreased",
  "Increased Respiratory Effort",
  "3523001",
  "3523003",
  "3523011",
  sep = "|"
)

# chest assessment values ----
chest_assessment_values <- paste(
  "3525005",
  "Accessory Muscles Used with Breathing",
  "3525023",
  "Flail Segment",
  "3525039",
  "Retraction",
  sep = "|"
)

# respiratory effort values ----
respiratory_effort_values <- paste(
  "Apneic",
  "Labored",
  "Mechanically Assisted",
  "Rapid",
  "Shallow",
  "Weak/Agonal",
  "3315001",
  "3315003",
  "3315005",
  "3315009",
  "3315011",
  "3315013",
  sep = "|"
)

# airway management values ----
airway_management_values <- paste(
  "243142003", # Dual pressure spontaneous ventilation support
  "Dual pressure spontaneous ventilation support",

  "47545007", # CPAP ventilation treatment
  "Continuous positive airway pressure ventilation treatment",

  "429705000", # Esophageal tracheal combitube
  "Insertion of esophageal tracheal combitube",

  "427753009", # Double‑lumen supraglottic airway
  "Insertion of esophageal tracheal double lumen supraglottic airway",

  "424979004", # LMA insertion
  "Laryngeal mask airway insertion",

  "23674004", # Orotracheal intubation
  "Orotracheal intubation",

  "450601000124103", # Orotracheal with bougie
  "Orotracheal intubation using bougie device",

  "241689008", # RSI
  "Rapid sequence induction",

  "450611000124100", # Single‑lumen supraglottic airway
  "Insertion of single lumen supraglottic airway device",

  sep = "|"
)

# trauma triage criteria values for 65+ age group ----
trauma_triage_1_2_values_65 <- paste(
  "2903001|Amputation proximal to wrist or ankle",
  "3903003|Crushed, degloved, mangled, or pulseless extremity",
  "2903005|Chest wall instability or deformity",
  "2903009|Open or depressed skull fracture",
  "2903011|Paralysis",
  "3903013|Pelvic fractures",
  "2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee",
  "2903017|Respiratory Rate <10 or >29 breaths per minute (<20 in infants aged <1) or need for ventilatory support",
  "3903021|Two or more proximal long-bone fractures",
  sep = "|"
)

# trauma triage criteria values ----
trauma_triage_1_2_values_10_64 <- paste(
  "2903001|Amputation proximal to wrist or ankle",
  "3903003|Crushed, degloved, mangled, or pulseless extremity",
  "2903005|Chest wall instability or deformity",
  "2903009|Open or depressed skull fracture",
  "2903011|Paralysis",
  "3903013|Pelvic fractures",
  "2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee",
  "2903017|Respiratory Rate <10 or >29 breaths per minute (<20 in infants aged <1) or need for ventilatory support",
  "3903021|Two or more proximal long-bone fractures",
  "2903019|Systolic Blood Pressure <90 mmHg",
  sep = "|"
)

# trauma triage criteria values for <10 age group ----
trauma_triage_1_2_values_10 <- paste(
  "2903001|Amputation proximal to wrist or ankle",
  "3903003|Crushed, degloved, mangled, or pulseless extremity",
  "2903005|Chest wall instability or deformity",
  "2903009|Open or depressed skull fracture",
  "2903011|Paralysis",
  "3903013|Pelvic fractures",
  "2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee",
  "3903021|Two or more proximal long-bone fractures",
  sep = "|"
)

# extremities assessment values ----
extremities_assessment_values <- paste(
  "3516043|Motor Function-Abnormal/Weakness",
  "3516067|Sensation-Absent",
  sep = "|"
)

# neurological assessment values ----
neurological_assessment_values <- paste(
  "3520017|Hemiplegia-Left",
  "3520019|Hemiplegia-Right",
  "3520043|Weakness-Left Sided",
  "3520045|Weakness-Right Sided",
  sep = "|"
)

# procedures values ----
tourniquet_values <- paste(
  "20655006|Application of tourniquet",
  "24173005|Tourniquet procedure",
  "241731009|Tourniquet positioning",
  "241733007|Tourniquet cuff inflation",
  "241734001|Upper tourniquet cuff inflation",
  "241735000|Lower tourniquet cuff inflation",
  "241736004|Manual tourniquet application",
  "398260007|Tourniquet positioned on patient",
  "447686008|Application of pressure to wound",
  sep = "|"
)

# trauma triage criteria (steps 3 and 4) values ----
trauma_triage_3_4_values <- paste(
  "2904001|Auto v\\. Pedestrian/Bicyclist Thrown, Run Over, or >20 MPH Accident",
  "2904007|Crash Death in Same Passenger Compartment",
  "2904009|Crash Ejection (partial or complete) from automobile",
  "2904011|Crash Intrusion, Including roof: > 12 in\\. occupant site; > 18 in\\. any site",
  "2904013|Crash Vehicle Telemetry Data (AACN) Consistent with High Risk of Injury",
  sep = "|"
)

# cause of injury matches values ----
cause_of_injury_values <- paste(
  # ICD‑10 transport injury block
  "(?:V20|V21|V22|V23|V24|V25|V26|V27|V28|V29|V30|V31|V32|V33|V34|V35|V36|V37|V38|V39|V80|V86)",

  # Motorcycle rider injuries
  "Motorcycle rider injured in collision with pedestrian or animal",
  "Motorcycle rider injured in collision with pedal cycle",
  "Motorcycle rider injured in collision with two- or three- wheeled motor vehicle",
  "Motorcycle rider injured in collision with car, pick-up truck or van",
  "Motorcycle rider injured in collision with heavy transport vehicle or bus",
  "Motorcycle rider injured in collision with railway train or railway vehicle",
  "Motorcycle rider injured in collision with other nonmotor vehicle",
  "Motorcycle rider injured in collision with fixed or stationary object",
  "Motorcycle rider injured in noncollision transport accident",
  "Motorcycle rider injured in other and unspecified transport accidents",

  # Three‑wheeled motor vehicle occupants
  "Occupant of three-wheeled motor vehicle injured in collision with pedestrian or animal",
  "Occupant of three-wheeled motor vehicle injured in collision with pedal cycle",
  "Occupant of three-wheeled motor vehicle injured in collision with two- or three- wheeled motor vehicle",
  "Occupant of three-wheeled motor vehicle injured in collision with car, pick-up truck or van",
  "Occupant of three-wheeled motor vehicle injured in collision with heavy transport vehicle or bus",
  "Occupant of three-wheeled motor vehicle injured in collision with railway train or railway vehicle",
  "Occupant of three-wheeled motor vehicle injured in collision with other nonmotor vehicle",
  "Occupant of three-wheeled motor vehicle injured in collision with fixed or stationary object",
  "Occupant of three-wheeled motor vehicle injured in noncollision transport accident",
  "Occupant of three-wheeled motor vehicle injured in other and unspecified transport accidents",

  # Other mechanisms
  "Animal-rider or occupant of animal drawn vehicle injured in transport accident",
  "Occupant of special all-terrain or other off-road motor vehicle, injured in transport accident",

  sep = "|"
)

# hospital capability values ----
hospital_capability_values <- paste(
  "9908021",
  "9908023",
  "9908025",
  "9908027",
  "9908029",
  "trauma center",
  sep = "|"
)

# trauma alert values ages 65+ ----
trauma_alert_values_65 <- paste(
  "4224003|Yes-Adult Trauma",
  "4224017|Yes-Trauma",
  sep = "|"
)

# trauma alert values ages 10-64 ----
trauma_alert_values_10_64 <- paste(
  "4224003|Yes-Adult Trauma",
  "4224017|Yes-Trauma (General)",
  "4224011|Yes-Pediatric Trauma",
  sep = "|"
)

# cardiac arrest response ----
cardiac_arrest_response_prior <- "3001003|Yes, Prior to Any EMS Arrival"

# type of scene delay values ----
scene_delay_values <- "2210011|Extrication"

# answer yes! ----
yes_code <- "9923003|Yes"

# minor values ----
minor_values <- "days|2516001|hours|2516003|minutes|2516005|months|2516007"

year_values <- "2516009|years"

day_values <- "days|2516001"

hour_values <- "hours|2516003"

minute_values <- "minutes|2516005"

month_values <- "months|2516007"
