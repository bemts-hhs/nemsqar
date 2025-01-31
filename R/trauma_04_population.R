#' @title Trauma-04 Populations
#'
#' @description
#'
#' This function processes EMS data to generate the population needed to
#' calculated the Trauma-04 NEMSQA measure.
#'
#' @param df A data frame or tibble containing EMS data with all relevant
#'   columns.
#' @param patient_scene_table A data.frame or tibble containing only epatient
#'   and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations.
#' @param exam_table A data.frame or tibble containing only the eexam fields
#'   needed for this measure's calculations.
#' @param procedures_table A data.frame or tibble containing only the
#'   eprocedures fields needed for this measure's calculations.
#' @param injury_table A data.frame or tibble containing only the einjury fields
#'   needed for this measure's calculations.
#' @param disposition_table A data.frame or tibble containing only the
#'   edisposition fields needed for this measure's calculations.
#' @param erecord_01_col The column representing the EMS record unique
#'   identifier.
#' @param incident_date_col The column indicating the incident date. Must be of
#'   class `Date` or similar.
#' @param patient_dob_col The column representing the patient's date of birth.
#'   Must be of class `Date` or similar.
#' @param epatient_15_col The column for patient age numeric value.
#' @param epatient_16_col The column for patient age unit (e.g., "Years",
#'   "Months").
#' @param esituation_02_col The column containing information on the presence of
#'   injury.
#' @param eresponse_05_col The column representing the 911 response type.
#' @param eresponse_10_col Column name containing scene delay information.
#' @param transport_disposition_col The column for patient transport
#'   disposition.
#' @param edisposition_23_col Column name containing trauma hospital
#'   verification information.
#' @param evitals_06_col Column name containing systolic blood pressure (SBP)
#'   values.
#' @param evitals_10_col Column name containing heart rate values.
#' @param evitals_12_col Column name containing pulse oximetry values.
#' @param evitals_14_col Column name containing capillary refill information.
#' @param evitals_15_col Column name containing respiratory effort values.
#' @param evitals_21_col Column name containing Glasgow Coma Scale (GCS) Motor
#'   values.
#' @param eexam_16_col Column name containing extremities assessment details.
#' @param eexam_20_col Column name containing neurological assessment details.
#' @param eexam_23_col Column name containing lung assessment details.
#' @param eexam_25_col Column name containing chest assessment details.
#' @param eprocedures_03_col Column name containing airway management or
#'   tourniquet usage details.
#' @param einjury_01_col Column name containing injury cause details.
#' @param einjury_03_col Column name containing trauma triage steps 1 and 2
#'   information.
#' @param einjury_04_col Column name containing trauma triage steps 3 and 4
#'   information.
#' @param einjury_09_col Column name containing fall height information.
#'
#' @return #' A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
trauma_04_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      vitals_table = NULL,
                      exam_table = NULL,
                      procedures_table = NULL,
                      injury_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      esituation_02_col,
                      eresponse_05_col,
                      eresponse_10_col,
                      transport_disposition_col,
                      edisposition_23_col,
                      evitals_06_col,
                      evitals_10_col,
                      evitals_12_col,
                      evitals_14_col,
                      evitals_15_col,
                      evitals_21_col,
                      eexam_16_col,
                      eexam_20_col,
                      eexam_23_col,
                      eexam_25_col,
                      eprocedures_03_col,
                      einjury_01_col,
                      einjury_03_col,
                      einjury_04_col,
                      einjury_09_col
                      ) {

  # ensure that not all table arguments AND the df argument are fulfilled
  # user only passes df or all table arguments
  if(

    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) && !is.null(df)

  ) {

    cli::cli_abort("{.fn trauma_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # ensure that df or all table arguments are fulfilled
  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
    ) && is.null(df)

  ) {

    cli::cli_abort("{.fn trauma_04_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # ensure all *_col arguments are fulfilled
  if(

    any(

      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_dob_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(esituation_02_col),
      missing(eresponse_05_col),
      missing(eresponse_10_col),
      missing(transport_disposition_col),
      missing(edisposition_23_col),
      missing(evitals_06_col),
      missing(evitals_10_col),
      missing(evitals_12_col),
      missing(evitals_14_col),
      missing(evitals_15_col),
      missing(evitals_21_col),
      missing(eexam_16_col),
      missing(eexam_20_col),
      missing(eexam_23_col),
      missing(eexam_25_col),
      missing(eprocedures_03_col),
      missing(einjury_01_col),
      missing(einjury_03_col),
      missing(einjury_04_col),
      missing(einjury_09_col)

    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing. Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn trauma_04_population}.")

  }


  # options for the progress bar
  # a green dot for progress
  # a white line for note done yet
  options(cli.progress_bar_style = "dot")

  options(cli.progress_bar_style = list(
    complete = cli::col_green("\u25CF"),  # Black Circle
    incomplete = cli::col_br_white("\u2500")  # Light Horizontal Line
  ))

  # initiate the progress bar process
  progress_bar_population <- cli::cli_progress_bar(
    "Running `trauma_04_population()`",
    total = 31,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  # Create objects that are filter helpers throughout the function

  # injury values
  possible_injury <- "Yes|9922005"

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

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
  trauma_triage_1_2_values_10_64 <- "2903001|Amputation proximal to wrist or ankle|3903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity \\(e.g., flail chest\\)|2903009|Open or depressed skull fracture|2903011|Paralysis|3903013|Pelvic fractures|2903015|All Penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1\\) or need for ventilatory support|3903021|Two or more proximal long-bone fractures|2903019|Systolic Blood Pressure <90 mmHg"

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
  cause_of_injury_values <- "(?:V20|V21|V22|V23|V24|V25|V26|V27|V28|V29|V30|V31|V32|V33|V34|V35|V36|V37|V38|V39|V80|V86)|Motorcycle rider injured in collision with pedestrian or animal|Motorcycle rider injured in collision with pedal cycle|Motorcycle rider injured in collision with two- or three- wheeled motor vehicle|Motorcycle rider injured in collision with car, pick-up truck or van|Motorcycle rider injured in collision with heavy transport vehicle or bus|Motorcycle rider injured in collision with railway train or railway vehicle|Motorcycle rider injured in collision with other nonmotor vehicle|Motorcycle rider injured in collision with fixed or stationary object|Motorcycle rider injured in noncollision transport accident|Motorcycle rider injured in other and unspecified transport accidents|Occupant of three-wheeled motor vehicle injured in collision with pedestrian or animal|Occupant of three-wheeled motor vehicle injured in collision with pedal cycle|Occupant of three-wheeled motor vehicle injured in collision with two- or three- wheeled motor vehicle|Occupant of three-wheeled motor vehicle injured in collision with car, pick-up truck or van|Occupant of three-wheeled motor vehicle injured in collision with heavy transport vehicle or bus|Occupant of three-wheeled motor vehicle injured in collision with railway train or railway vehicle|Occupant of three-wheeled motor vehicle injured in collision with other nonmotor vehicle|Occupant of three-wheeled motor vehicle injured in collision with fixed or stationary object|Occupant of three-wheeled motor vehicle injured in noncollision transport accident|Occupant of three-wheeled motor vehicle injured in other and unspecified transport accidents|Animal-rider or occupant of animal drawn vehicle injured in transport accident|Occupant of special all-terrain or other off-road motor vehicle, injured in transport accident"

  # hospital capability values
  hospital_capability_values <- "9908021|Trauma Center Level 1|9908023|Trauma Center Level 2|9908025|Trauma Center Level 3|9908027|Trauma Center Level 4|9908029|Trauma Center Level 5"

  # days, hours, minutes, months
  minor_values <- "days|2516001|hours|2516003|minutes|2516005|months|2516007"

  year_values <- "2516009|years"

  day_values <- "days|2516001"

  hour_values <- "hours|2516003"

  minute_values <- "minutes|2516005"

  month_values <- "months|2516007"

  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(situation_table),
      !is.null(vitals_table),
      !is.null(procedures_table),
      !is.null(exam_table),
      !is.null(injury_table),
      !is.null(disposition_table)
    ) && is.null(df)

  ) {

    # Ensure df is a data frame or tibble
    if (

      any(
        !(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||

        !(is.data.frame(response_table) && tibble::is_tibble(response_table)) ||

        !(is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||

        !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table)) ||

        !(is.data.frame(procedures_table) && tibble::is_tibble(procedures_table)) ||

        !(is.data.frame(exam_table) && tibble::is_tibble(exam_table)) ||

        !(is.data.frame(injury_table) && tibble::is_tibble(injury_table)) ||

        !(is.data.frame(disposition_table) && tibble::is_tibble(disposition_table))

      )

    ) {

      cli::cli_abort(
        c(
          "An object of class {.cls data.frame} or {.cls tibble} is required for each of the *_table arguments."
        )
      )
    }

    # Only check the date columns if they are in fact passed
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    ) {
      # Use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_dob_col)

      # Convert quosures to names and check the column classes
      incident_date_name <- rlang::as_name(incident_date)
      patient_dob_name <- rlang::as_name(patient_dob)

      if ((!lubridate::is.Date(patient_scene_table[[incident_date_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[incident_date_name]])) ||
          (!lubridate::is.Date(patient_scene_table[[patient_dob_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[patient_dob_name]]))) {

        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both of these variables were not of class {.cls Date} or a similar class. Please format your {.var incident_date_col} and {.var patient_dob_col} to class {.cls Date} or a similar class."
        )
      }
    }

    progress_bar_population

  ###_____________________________________________________________________________
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_dob_col }},
      units = "days"
    )) / 365,

    # system age check
    system_age_65 = {{ epatient_15_col }} >= 65 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_64 = ({{ epatient_15_col }} < 65 & {{ epatient_15_col }} >= 10) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_1 = {{ epatient_15_col }} < 10 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_2 = {{ epatient_15_col }} < 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_10 = system_age_10_1 | system_age_10_2,

    # calculated age check
    calc_age_65 = patient_age_in_years_col >= 65,
    calc_age_10_64 = patient_age_in_years_col < 65 & patient_age_in_years_col >= 10,
    calc_age_10 = patient_age_in_years_col < 10

    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    ))

  {

    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
      dplyr::mutate(

      # system age check
      system_age_65 = {{ epatient_15_col }} >= 65 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_64 = ({{ epatient_15_col }} < 65 & {{ epatient_15_col }} >= 10) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_1 = {{ epatient_15_col }} < 10 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_2 = {{ epatient_15_col }} < 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
      system_age_10 = system_age_10_1 | system_age_10_2

      )

  }

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # possible injury
  possible_injury_data <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_02_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)

      ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # transports
  transport_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # GCS
  GCS_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_21_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = GCS_motor_values, x = {{ evitals_21_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # lung assessment
  lung_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = lung_assessment_values, x = {{ eexam_23_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # chest assessment
  chest_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_25_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = chest_assessment_values, x = {{ eexam_25_col }})

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # respiratory effort
  respiratory_effort_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_15_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = respiratory_effort_values, x = {{ evitals_15_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # airway management
  airway_management_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = airway_management_values, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # pulse oximetry
  pulse_oximetry_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_12_col }} < 90

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # SBP
  SBP_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_06_col }} < 110

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # heart rate and SBP
  HR_SBP_data_10_65_plus <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_10_col }}, {{ evitals_06_col}}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_10_col }} > {{ evitals_06_col}}

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age 65+
  trauma_triage_1_2_data_65 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_65, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age 10 - 65
  trauma_triage_1_2_data_10_64 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_10_64, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age < 10
  trauma_triage_1_2_data_10 <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_10, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)

  # extremities assessment
  extremities_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = extremities_assessment_values, x = {{ eexam_16_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)

  # neurological assessment

  neurological_assessment_data <- exam_table |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_20_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = neurological_assessment_values, x = {{ eexam_20_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 17, id = progress_bar_population, force = T)

  # tourniquet

  tourniquet_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = tourniquet_values, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 18, id = progress_bar_population, force = T)

  # trauma triage criteria steps 3 and 4

  trauma_triage_3_4_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_04_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_3_4_values, x = {{ einjury_04_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 19, id = progress_bar_population, force = T)

  # fall height

  fall_height_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_09_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ einjury_09_col }} > 10

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 20, id = progress_bar_population, force = T)

  # scene delay

  scene_delay_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_10_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = scene_delay_values, x = {{ eresponse_10_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 21, id = progress_bar_population, force = T)

  # cause of injury

  cause_of_injury_data <- injury_table |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_01_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = cause_of_injury_values, x = {{ einjury_01_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 22, id = progress_bar_population, force = T)

  # respiratory rate for < 10 yrs population

  respiratory_rate_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_14_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_14_col }} < 10 | {{ evitals_14_col }} > 29

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 23, id = progress_bar_population, force = T)

  # SBP check variable for ages < 10 years
  # if using calculated and system ages
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {


  SBP_age_10_data <- final_data |>
    dplyr::select({{ erecord_01_col }}, system_age_10, calc_age_10, {{ epatient_15_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(system_age_10 | calc_age_10) |>
    dplyr::left_join(vitals_table |> dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |> distinct(),

                     by = rlang::as_name(rlang::enquo(erecord_01_col))

                     ) |>
    dplyr::mutate(SBP_10 =

                    ( {{ evitals_06_col }} + ({{ epatient_15_col }} * 2) ) >= 70

                  ) |>
    dplyr::filter(SBP_10) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})


    # SBP check variable for ages < 10 years
    # if using system ages only

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    ))

  {

    SBP_age_10_data <- final_data |>
    dplyr::select({{ erecord_01_col }}, system_age_10, {{ epatient_15_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(system_age_10) |>
    dplyr::left_join(vitals_table |> dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |> distinct(),

                     by = rlang::as_name(rlang::enquo(erecord_01_col))

                     ) |>
    dplyr::mutate(SBP_10 =

                    ( {{ evitals_06_col }} + ({{ epatient_15_col }} * 2) ) >= 70

      ) |>
    dplyr::filter(SBP_10) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  }

  cli::cli_progress_update(set = 24, id = progress_bar_population, force = T)

  # hospital capability
  hospital_capability_data <- disposition_table |>
    dplyr::select({{ erecord_01_col }}, {{ edisposition_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = hospital_capability_values, x = {{ edisposition_23_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 25, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(
      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
      TRANSPORTS = {{ erecord_01_col }} %in% transport_data,
      POSSIBLE_INJURY = {{ erecord_01_col }} %in% possible_injury_data,
      GCS = {{ erecord_01_col }} %in% GCS_data,
      LUNG = {{ erecord_01_col }} %in% lung_assessment_data,
      CHEST = {{ erecord_01_col }} %in% chest_data,
      RESPIRATORY_EFFORT = {{ erecord_01_col }} %in% respiratory_effort_data,
      AIRWAY_MANAGEMENT = {{ erecord_01_col }} %in% airway_management_data,
      EXTREMITIES = {{ erecord_01_col }} %in% extremities_assessment_data,
      NEURO = {{ erecord_01_col }} %in% neurological_assessment_data,
      TOURNIQUET = {{ erecord_01_col }} %in% tourniquet_data,
      TRAUMA_TRIAGE_3_4 = {{ erecord_01_col }} %in% trauma_triage_3_4_data,
      FALL_HEIGHT = {{ erecord_01_col }} %in% fall_height_data,
      SCENE_DELAY = {{ erecord_01_col }} %in% scene_delay_data,
      INJURY_CAUSE = {{ erecord_01_col }} %in% cause_of_injury_data,
      PULSE_OXIMETRY = {{ erecord_01_col }} %in% pulse_oximetry_data,
      SBP = {{ erecord_01_col }} %in% SBP_data,
      SBP_10 = {{ erecord_01_col }} %in% SBP_age_10_data,
      HR_SBP_10_65_PLUS = {{ erecord_01_col }} %in% HR_SBP_data_10_65_plus,
      TRAUMA_TRIAGE_1_2_65 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_65,
      TRAUMA_TRIAGE_1_2_10_64 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_10_64,
      TRAUMA_TRIAGE_1_2_10 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_10,
      RESPIRATORY_RATE_10 = {{ erecord_01_col }} %in% respiratory_rate_data,
      HOSPITAL_CAPABILITY = {{ erecord_01_col }} %in% hospital_capability_data

    )

  cli::cli_progress_update(set = 26, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      POSSIBLE_INJURY,
      CALL_911,
      TRANSPORTS

      )

  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 27, id = progress_bar_population, force = T)

  if(

    # use the system generated and calculated ages

    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {


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
                    TRAUMA_TRIAGE_1_2_65 |
                    EXTREMITIES |
                    NEURO |
                    TOURNIQUET |
                    TRAUMA_TRIAGE_3_4 |
                    FALL_HEIGHT |
                    SCENE_DELAY |
                    INJURY_CAUSE

                  )

  cli::cli_progress_update(set = 28, id = progress_bar_population, force = T)

  # filter ages 10 to 65
  pop_10_64 <- initial_population |>
    dplyr::filter(system_age_10_64 | calc_age_10_64) |>
    dplyr::filter(

                    GCS |
                    LUNG |
                    CHEST |
                    RESPIRATORY_EFFORT |
                    AIRWAY_MANAGEMENT |
                    HR_SBP_10_65_PLUS |
                    TRAUMA_TRIAGE_1_2_10_64 |
                    EXTREMITIES |
                    NEURO |
                    TOURNIQUET |
                    TRAUMA_TRIAGE_3_4 |
                    FALL_HEIGHT |
                    SCENE_DELAY |
                    INJURY_CAUSE

                  )

  cli::cli_progress_update(set = 29, id = progress_bar_population, force = T)

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

  } else if(

    # only use the system generated values

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )

  ) {

    # filter older adult
    pop_65 <- initial_population |>
      dplyr::filter(system_age_65) |>
      dplyr::filter(

        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE

      )

    cli::cli_progress_update(set = 28, id = progress_bar_population, force = T)

    # filter ages 10 to 65
    pop_10_64 <- initial_population |>
      dplyr::filter(system_age_10_64) |>
      dplyr::filter(

        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE

      )

    cli::cli_progress_update(set = 29, id = progress_bar_population, force = T)

    # filter ages < 10
    pop_10 <- initial_population |>
      dplyr::filter(system_age_10) |>
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

  }

  # summarize
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 30, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Situation possible injury",
               "911 calls",
               "Transports",
               "GCS Motor 1-5",
               "Breath sounds absent, decreased, increased respiratory effort",
               "Flail segment, retraction, accessory muscles used in breathing",
               "Respiratory effort apneic, labored, mech. assist, rapid, shallow, weak/agonal",
               "Airway management procedures",
               "Pulse oximetry < 90",
               "SBP < 110",
               "Respiratory rate < 10 or > 29 ages < 10 yrs",
               "(SBP + (Pt. Age * 2)) >= 70 ages < 10 yrs",
               "Heart rate greater than SBP ages >= 10 yrs",
               "Met trauma triage criteria 1-2 ages 65+ yrs",
               "Met trauma triage criteria 1-2 ages 10-64 yrs",
               "Met trauma triage criteria 1-2 ages < 10 yrs",
               "Motor function abnormal/weakness or sensation absent",
               "Hemiplegia left/right, weakness left/right",
               "Tournique procedure",
               "Trauma triage criteria 3-4",
               "Cause of Injury matches V20-V39, V80, V86",
               "Scene delay = extrication",
               "Fall > 10 ft.",
               "Transported to a Trauma Center",
               "Patients 65+ yrs denominator",
               "Patients 10-65 yrs denominator",
               "Patients < 10 yrs denominator",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$POSSIBLE_INJURY, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      sum(computing_population$TRANSPORTS, na.rm = T),
      sum(computing_population$GCS, na.rm = T),
      sum(computing_population$LUNG, na.rm = T),
      sum(computing_population$CHEST, na.rm = T),
      sum(computing_population$RESPIRATORY_EFFORT, na.rm = T),
      sum(computing_population$AIRWAY_MANAGEMENT, na.rm = T),
      sum(computing_population$PULSE_OXIMETRY, na.rm = T),
      sum(computing_population$SBP, na.rm = T),
      sum(computing_population$RESPIRATORY_RATE_10, na.rm = T),
      sum(computing_population$SBP_10, na.rm = T),
      sum(computing_population$HR_SBP_10_65_PLUS, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_65, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10_64, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10, na.rm = T),
      sum(computing_population$EXTREMITIES, na.rm = T),
      sum(computing_population$NEURO, na.rm = T),
      sum(computing_population$TOURNIQUET, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_3_4, na.rm = T),
      sum(computing_population$INJURY_CAUSE, na.rm = T),
      sum(computing_population$SCENE_DELAY, na.rm = T),
      sum(computing_population$FALL_HEIGHT, na.rm = T),
      sum(computing_population$HOSPITAL_CAPABILITY, na.rm = T),
      nrow(pop_65),
      nrow(pop_10_64),
      nrow(pop_10),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  cli::cli_progress_update(set = 31, id = progress_bar_population, force = T)

  # get the population of interest
  trauma.04.population <- list(
    filter_process = filter_counts,
    population_65 = pop_65,
    population_10_64 = pop_10_64,
    population_10 = pop_10,
    initial_population = initial_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(trauma.04.population)

  } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(situation_table),
      is.null(vitals_table),
      is.null(procedures_table),
      is.null(exam_table),
      is.null(injury_table),
      is.null(disposition_table)
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

    # only check the date columns if they are in fact passed
    if(
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    )

    {

      # use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_dob_col)

      if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))) {

        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_dob_col} to class {.cls Date} or similar class."
        )

      }
    }

    progress_bar_population

  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________

  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)

  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  final_data <- df |>
    dplyr::select(-c(

      {{ esituation_02_col }},
      {{ eresponse_05_col }},
      {{ eresponse_10_col }},
      {{ transport_disposition_col }},
      {{ edisposition_23_col }},
      {{ evitals_06_col }},
      {{ evitals_10_col }},
      {{ evitals_12_col }},
      {{ evitals_14_col }},
      {{ evitals_15_col }},
      {{ evitals_21_col }},
      {{ eexam_16_col }},
      {{ eexam_20_col }},
      {{ eexam_23_col }},
      {{ eexam_25_col }},
      {{ eprocedures_03_col }},
      {{ einjury_01_col }},
      {{ einjury_03_col }},
      {{ einjury_04_col }},
      {{ einjury_09_col }}


    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_dob_col }},
      units = "days"
    )) / 365,

    # system age check
    system_age_65 = {{ epatient_15_col }} >= 65 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_64 = ({{ epatient_15_col }} < 65 & {{ epatient_15_col }} >= 10) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_1 = {{ epatient_15_col }} < 10 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_10_2 = {{ epatient_15_col }} < 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
    system_age_10 = system_age_10_1 | system_age_10_2,

    # calculated age check
    calc_age_65 = patient_age_in_years_col >= 65,
    calc_age_10_64 = patient_age_in_years_col < 65 & patient_age_in_years_col >= 10,
    calc_age_10 = patient_age_in_years_col < 10

    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    ))

  {

    final_data <- df |>
      dplyr::select(-c(

        {{ esituation_02_col }},
        {{ eresponse_05_col }},
        {{ eresponse_10_col }},
        {{ transport_disposition_col }},
        {{ edisposition_23_col }},
        {{ evitals_06_col }},
        {{ evitals_10_col }},
        {{ evitals_12_col }},
        {{ evitals_14_col }},
        {{ evitals_15_col }},
        {{ evitals_21_col }},
        {{ eexam_16_col }},
        {{ eexam_20_col }},
        {{ eexam_23_col }},
        {{ eexam_25_col }},
        {{ eprocedures_03_col }},
        {{ einjury_01_col }},
        {{ einjury_03_col }},
        {{ einjury_04_col }},
        {{ einjury_09_col }}


      )) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
      dplyr::mutate(

      # system age check
      system_age_65 = {{ epatient_15_col }} >= 65 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_64 = ({{ epatient_15_col }} < 65 & {{ epatient_15_col }} >= 10) & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_1 = {{ epatient_15_col }} < 10 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_10_2 = {{ epatient_15_col }} < 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
      system_age_10 = system_age_10_1 | system_age_10_2

      )

  }

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # possible injury
  possible_injury_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ esituation_02_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = possible_injury, x = {{ esituation_02_col }}, ignore.case = T)

      ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # transports
  transport_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ transport_disposition_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = transport_responses, x = {{ transport_disposition_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  # GCS
  GCS_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_21_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(grepl(pattern = GCS_motor_values, x = {{ evitals_21_col }}, ignore.case = T)) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # lung assessment
  lung_assessment_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = lung_assessment_values, x = {{ eexam_23_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # chest assessment
  chest_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_25_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = chest_assessment_values, x = {{ eexam_25_col }})

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # respiratory effort
  respiratory_effort_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_15_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = respiratory_effort_values, x = {{ evitals_15_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # airway management
  airway_management_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = airway_management_values, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # pulse oximetry
  pulse_oximetry_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_12_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_12_col }} < 90

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # SBP
  SBP_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_06_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_06_col }} < 110

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # heart rate and SBP
  HR_SBP_data_10_65_plus <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_10_col }}, {{ evitals_06_col}}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_10_col }} > {{ evitals_06_col}}

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age 65+
  trauma_triage_1_2_data_65 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_65, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age 10 - 65
  trauma_triage_1_2_data_10_64 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_10_64, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

  # trauma triage criteria steps 1 and 2 age < 10
  trauma_triage_1_2_data_10 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_1_2_values_10, x = {{ einjury_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)

  # extremities assessment
  extremities_assessment_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = extremities_assessment_values, x = {{ eexam_16_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)

  # neurological assessment

  neurological_assessment_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eexam_20_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = neurological_assessment_values, x = {{ eexam_20_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 17, id = progress_bar_population, force = T)

  # tourniquet

  tourniquet_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = tourniquet_values, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 18, id = progress_bar_population, force = T)

  # trauma triage criteria steps 3 and 4

  trauma_triage_3_4_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_04_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = trauma_triage_3_4_values, x = {{ einjury_04_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 19, id = progress_bar_population, force = T)

  # fall height

  fall_height_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_09_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ einjury_09_col }} > 10

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 20, id = progress_bar_population, force = T)

  # scene delay

  scene_delay_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_10_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = scene_delay_values, x = {{ eresponse_10_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 21, id = progress_bar_population, force = T)

  # cause of injury

  cause_of_injury_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ einjury_01_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = cause_of_injury_values, x = {{ einjury_01_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 22, id = progress_bar_population, force = T)

  # respiratory rate for < 10 yrs population

  respiratory_rate_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_14_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      {{ evitals_14_col }} < 10 | {{ evitals_14_col }} > 29

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 23, id = progress_bar_population, force = T)

  # SBP check variable for ages < 10 years
  # if using calculated and system ages
  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {


  SBP_age_10_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ epatient_15_col }}, {{ evitals_06_col }}) |>
    dplyr::distinct() |>
    dplyr::left_join(final_data |> dplyr::select({{ erecord_01_col }}, system_age_10, calc_age_10) |> distinct(),

                    by = rlang::as_name(rlang::enquo(erecord_01_col))

                    ) |>
    dplyr::filter(system_age_10 | calc_age_10) |>
    dplyr::mutate(SBP_10 =

                    ( {{ evitals_06_col }} + ({{ epatient_15_col }} * 2) ) >= 70

                  ) |>
    dplyr::filter(SBP_10) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})


    # SBP check variable for ages < 10 years
    # if using system ages only

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    ))

  {

  SBP_age_10_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ epatient_15_col }}, {{ evitals_06_col }}) |>
    dplyr::distinct() |>
    dplyr::left_join(final_data |> dplyr::select({{ erecord_01_col }}, system_age_10),

                    by = rlang::as_name(rlang::enquo(erecord_01_col))

                    ) |>
    dplyr::filter(system_age_10) |>
    dplyr::mutate(SBP_10 =

                    ( {{ evitals_06_col }} + ({{ epatient_15_col }} * 2) ) >= 70

                  ) |>
    dplyr::filter(SBP_10) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  }

  cli::cli_progress_update(set = 24, id = progress_bar_population, force = T)

  # hospital capability
  hospital_capability_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ edisposition_23_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = hospital_capability_values, x = {{ edisposition_23_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 25, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(
      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
      TRANSPORTS = {{ erecord_01_col }} %in% transport_data,
      POSSIBLE_INJURY = {{ erecord_01_col }} %in% possible_injury_data,
      GCS = {{ erecord_01_col }} %in% GCS_data,
      LUNG = {{ erecord_01_col }} %in% lung_assessment_data,
      CHEST = {{ erecord_01_col }} %in% chest_data,
      RESPIRATORY_EFFORT = {{ erecord_01_col }} %in% respiratory_effort_data,
      AIRWAY_MANAGEMENT = {{ erecord_01_col }} %in% airway_management_data,
      EXTREMITIES = {{ erecord_01_col }} %in% extremities_assessment_data,
      NEURO = {{ erecord_01_col }} %in% neurological_assessment_data,
      TOURNIQUET = {{ erecord_01_col }} %in% tourniquet_data,
      TRAUMA_TRIAGE_3_4 = {{ erecord_01_col }} %in% trauma_triage_3_4_data,
      FALL_HEIGHT = {{ erecord_01_col }} %in% fall_height_data,
      SCENE_DELAY = {{ erecord_01_col }} %in% scene_delay_data,
      INJURY_CAUSE = {{ erecord_01_col }} %in% cause_of_injury_data,
      PULSE_OXIMETRY = {{ erecord_01_col }} %in% pulse_oximetry_data,
      SBP = {{ erecord_01_col }} %in% SBP_data,
      SBP_10 = {{ erecord_01_col }} %in% SBP_age_10_data,
      HR_SBP_10_65_PLUS = {{ erecord_01_col }} %in% HR_SBP_data_10_65_plus,
      TRAUMA_TRIAGE_1_2_65 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_65,
      TRAUMA_TRIAGE_1_2_10_64 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_10_64,
      TRAUMA_TRIAGE_1_2_10 = {{ erecord_01_col }} %in% trauma_triage_1_2_data_10,
      RESPIRATORY_RATE_10 = {{ erecord_01_col }} %in% respiratory_rate_data,
      HOSPITAL_CAPABILITY = {{ erecord_01_col }} %in% hospital_capability_data

    )

  cli::cli_progress_update(set = 26, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      POSSIBLE_INJURY,
      CALL_911,
      TRANSPORTS

      )

  # Adult and Pediatric Populations

  cli::cli_progress_update(set = 27, id = progress_bar_population, force = T)

  if(

    # use the system generated and calculated ages

    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {


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
                    TRAUMA_TRIAGE_1_2_65 |
                    EXTREMITIES |
                    NEURO |
                    TOURNIQUET |
                    TRAUMA_TRIAGE_3_4 |
                    FALL_HEIGHT |
                    SCENE_DELAY |
                    INJURY_CAUSE

                  )

  cli::cli_progress_update(set = 28, id = progress_bar_population, force = T)

  # filter ages 10 to 65
  pop_10_64 <- initial_population |>
    dplyr::filter(system_age_10_64 | calc_age_10_64) |>
    dplyr::filter(

                    GCS |
                    LUNG |
                    CHEST |
                    RESPIRATORY_EFFORT |
                    AIRWAY_MANAGEMENT |
                    HR_SBP_10_65_PLUS |
                    TRAUMA_TRIAGE_1_2_10_64 |
                    EXTREMITIES |
                    NEURO |
                    TOURNIQUET |
                    TRAUMA_TRIAGE_3_4 |
                    FALL_HEIGHT |
                    SCENE_DELAY |
                    INJURY_CAUSE

                  )

  cli::cli_progress_update(set = 29, id = progress_bar_population, force = T)

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

  } else if(

    # only use the system generated values

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )

  ) {

    # filter older adult
    pop_65 <- initial_population |>
      dplyr::filter(system_age_65) |>
      dplyr::filter(

        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          PULSE_OXIMETRY |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_65 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE

      )

    cli::cli_progress_update(set = 28, id = progress_bar_population, force = T)

    # filter ages 10 to 65
    pop_10_64 <- initial_population |>
      dplyr::filter(system_age_10_64) |>
      dplyr::filter(

        GCS |
          LUNG |
          CHEST |
          RESPIRATORY_EFFORT |
          AIRWAY_MANAGEMENT |
          HR_SBP_10_65_PLUS |
          TRAUMA_TRIAGE_1_2_10_64 |
          EXTREMITIES |
          NEURO |
          TOURNIQUET |
          TRAUMA_TRIAGE_3_4 |
          FALL_HEIGHT |
          SCENE_DELAY |
          INJURY_CAUSE

      )

    cli::cli_progress_update(set = 29, id = progress_bar_population, force = T)

    # filter ages < 10
    pop_10 <- initial_population |>
      dplyr::filter(system_age_10) |>
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

  }

  # summarize
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 30, id = progress_bar_population, force = T)

  # summarize counts for populations filtered
  filter_counts <- tibble::tibble(
    filter = c("Situation possible injury",
               "911 calls",
               "Transports",
               "GCS Motor 1-5",
               "Breath sounds absent, decreased, increased respiratory effort",
               "Flail segment, retraction, accessory muscles used in breathing",
               "Respiratory effort apneic, labored, mech. assist, rapid, shallow, weak/agonal",
               "Airway management procedures",
               "Pulse oximetry < 90",
               "SBP < 110",
               "Respiratory rate < 10 or > 29 ages < 10 yrs",
               "(SBP + (Pt. Age * 2)) >= 70 ages < 10 yrs",
               "Heart rate greater than SBP ages >= 10 yrs",
               "Met trauma triage criteria 1-2 ages 65+ yrs",
               "Met trauma triage criteria 1-2 ages 10-64 yrs",
               "Met trauma triage criteria 1-2 ages < 10 yrs",
               "Motor function abnormal/weakness or sensation absent",
               "Hemiplegia left/right, weakness left/right",
               "Tournique procedure",
               "Trauma triage criteria 3-4",
               "Cause of Injury matches V20-V39, V80, V86",
               "Scene delay = extrication",
               "Fall > 10 ft.",
               "Transported to a Trauma Center",
               "Patients 65+ yrs denominator",
               "Patients 10-65 yrs denominator",
               "Patients < 10 yrs denominator",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$POSSIBLE_INJURY, na.rm = T),
      sum(computing_population$CALL_911, na.rm = T),
      sum(computing_population$TRANSPORTS, na.rm = T),
      sum(computing_population$GCS, na.rm = T),
      sum(computing_population$LUNG, na.rm = T),
      sum(computing_population$CHEST, na.rm = T),
      sum(computing_population$RESPIRATORY_EFFORT, na.rm = T),
      sum(computing_population$AIRWAY_MANAGEMENT, na.rm = T),
      sum(computing_population$PULSE_OXIMETRY, na.rm = T),
      sum(computing_population$SBP, na.rm = T),
      sum(computing_population$RESPIRATORY_RATE_10, na.rm = T),
      sum(computing_population$SBP_10, na.rm = T),
      sum(computing_population$HR_SBP_10_65_PLUS, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_65, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10_64, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_1_2_10, na.rm = T),
      sum(computing_population$EXTREMITIES, na.rm = T),
      sum(computing_population$NEURO, na.rm = T),
      sum(computing_population$TOURNIQUET, na.rm = T),
      sum(computing_population$TRAUMA_TRIAGE_3_4, na.rm = T),
      sum(computing_population$INJURY_CAUSE, na.rm = T),
      sum(computing_population$SCENE_DELAY, na.rm = T),
      sum(computing_population$FALL_HEIGHT, na.rm = T),
      sum(computing_population$HOSPITAL_CAPABILITY, na.rm = T),
      nrow(pop_65),
      nrow(pop_10_64),
      nrow(pop_10),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  cli::cli_progress_update(set = 31, id = progress_bar_population, force = T)

  # get the population of interest
  trauma.04.population <- list(
    filter_process = filter_counts,
    population_65 = pop_65,
    population_10_64 = pop_10_64,
    population_10 = pop_10,
    initial_population = initial_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(trauma.04.population)

  }

}


