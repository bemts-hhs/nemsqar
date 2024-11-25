#' Safety-04
#'
#' The `safety_04` function processes EMS incident data for specific safety and transport criteria, filtering by patient age and incident type to identify cases that meet specified exclusion or inclusion criteria. This function accommodates data with various EMS-specific codes, age descriptors, and procedure identifiers.
#' 
#' @section Data Assumptions:
#' 
#' This function assumes that:
#' 
#' Data are already loaded into a data frame or tibble where each row represents
#' one observation (e.g., patient) and each column is a distinct feature (field).
#' Alternatively, data may consist of separate datasets referenced by unique columns.
#' 
#' Patient age in years will be calculated using `incident_date_col` and `patient_DOB_col`.
#' Both of these fields must be valid `Date` or `POSIXct` data types.
#' 
#' Missing values in rows are represented as `NA`. "Not known" or "not recorded" values,
#' common to ImageTrend or other non-numeric codes, should be pre-cleaned before use.
#' 
#' The `eresponse_05_col` contains NEMSIS response codes and may also include
#' descriptive text for reference.
#' 
#' The `edisposition_14_col` is expected to contain transport mode descriptors, which
#' may be provided as comma-separated text within each cell, or as a list column.
#' 
#' The `earrest_01_col` includes cardiac arrest information as text and code; typically
#' only one response per patient encounter is recorded.
#' 
#' The `eprocedures_03_col` contains all procedures as a list or a text column with 
#' multiple entries separated by commas or other delimiters. Procedure codes may be present
#' but will not affect function output.
#' 
#' The `einjury_03_col` should include all trauma center triage criteria. This function will
#' detect duplicate entries and filter on unique values per patient encounter.
#' 
#' The `edisposition_12` and `edisposition_30` fields should be list columns or text fields
#' with all relevant transport disposition entries provided in a single cell as comma-separated
#' values for each unique incident.
#' 
#' The first argument to this function is the main data frame. No joins are performed within
#' the function; any necessary joins (e.g., to incorporate vitals or additional fields) should
#' be completed prior to calling this function.
#' 
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @section Practical Tips:
#' 
#' Ensure data are pre-processed, with missing values coded as `NA`, before passing
#' into the function.
#' Prepare necessary joins (e.g., for vitals) in advance; this function does not perform joins.
#' 
#' @section Value:
#' 
#' #' A summarized tibble with counts and proportions of pediatric patients ages <8 yrs
#' using a pediatric restraing device among 911 requests or interfacility request.
#'
#' @param df <['tidy-select'][dplyr_tidy_select]> A data frame or tibble containing EMS data where each row represents an individual observation.
#' @param `erecord_01_col` <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Column name containing the incident dates, expected to be of Date or POSIXct class.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Column name containing patient birth dates, expected to be of Date or POSIXct class.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column name indicating the patient age.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column name for the unit of age (e.g., "Years," "Months").
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column containing response transport codes.
#' @param earrest_01_col <['tidy-select'][dplyr_tidy_select]> Column with cardiac arrest status information.
#' @param einjury_03_col <['tidy-select'][dplyr_tidy_select]> Column describing traumatic injuries, expected as a list or text-separated entries.
#' @param eprocedures_03_col <['tidy-select'][dplyr_tidy_select]> Column listing procedures, assumed to contain multiple procedure codes/texts in each cell.
#' @param edisposition_14_col <['tidy-select'][dplyr_tidy_select]> Column for transport dispositions.
#' @param transport_disposition_cols <['tidy-select'][dplyr_tidy_select]> Columns for primary and secondary transport dispositions.
#' @param ... Additional arguments for flexibility in function customization.
#'
#' @section Features:
#' * **Age Calculation**: Calculates patient age in years based on `incident_date_col` and `patient_DOB_col`.
#' * **Transport Identification**: Flags records for EMS transport and interfacility transports based on provided codes.
#' * **Cardiac Arrest & Trauma Triage Filtering**: Identifies cases with severe trauma indicators or cardiac arrest.
#' * **Procedure Exclusion**: Applies exclusion criteria based on specific airway and immobilization procedures.
#' * **Minor Age Check**: Separates pediatric cases (<8 years) by EMS-reported or calculated age.
#'
#' @return A data.frame summarizing results for three population groups (All,
#' Adults, and Peds) with the following columns:
#' `pop`: Population type (All, Adults, or Peds).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
safety_04 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      earrest_01_col,
                      einjury_03_col,
                      eprocedures_03_col,
                      edisposition_14_col,
                      transport_disposition_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn safety_04}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn safety_04}."
      )
    )
    
  }
  
  # Ensure df is a data frame or tibble
  if (!is.data.frame(df) && !is_tibble(df)) {
    cli_abort(
      c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
      )
    )
  }
  
  # use quasiquotation on the date variables to check format
  incident_date <- enquo(incident_date_col)
  patient_DOB <- enquo(patient_DOB_col)
  
  if ((!is.Date(df[[as_name(incident_date)]]) &
       !is.POSIXct(df[[as_name(incident_date)]])) ||
      (!is.Date(df[[as_name(patient_DOB)]]) &
       !is.POSIXct(df[[as_name(patient_DOB)]]))) {
    
    cli_abort(
      "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
    )
    
  }
  
  if (!exists("pretty_percent")) {
    pretty_percent <- function(variable, n_decimal = 0.1) {
      formatted_percent <- percent(variable, accuracy = n_decimal)
      
      # If there are trailing zeros after decimal point, remove them
      formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
      
      # If it ends with ".%", replace it with "%"
      formatted_percent <- sub("\\.%$", "%", formatted_percent)
      
      formatted_percent
      
    }
    
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
  cli::cli_h1("Calculating Safety-04")
  
  # initiate the progress bar process
  progress_bar <- cli::cli_progress_bar(
    "Running `safety_04()`",
    total = 14,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Completed {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {col_blue('Progress')}: {cli::pb_percent} | {col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )
  
  progress_bar
  
  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar, force = T)
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # transport code eresponse.05
  transport_code <- "2205005|Interfacility Transport"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
  # get codes as a regex to find cardiac arrest responses
  cardiac_arrest_responses <- "3001005|3001003|Yes, Prior to Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)|Yes, After Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)"
  
  # get applicable trauma triage codes for steps 1 and 2
  trauma_triage_crit <- "2903001|Amputation proximal to wrist or ankle|2903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity|2903007|Glasgow Coma Score <=13|2903009|Open or depressed skull fracture|2903011|Paralysis|2903013|Pelvic fractures|2903015|All penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1 year\\) or need for ventilatory support|3903019|Systolic Blood Pressure <90 mmHg|2903021|Two or more long-bone fractures"
  
  # procedure exclusion related to long board
  
  long_board <- "450591000124106|Immobilization using long board"
  
  # additional procedures in the exclusion
  
  airway_procedures <- "16883004|Endotracheal intubation, emergency procedure|182682004|Emergency laryngeal intubation|232674004|Orotracheal intubation|232678001|Orotracheal fiberoptic intubation|232682004|Nasotracheal fiberoptic intubation|232685002|Insertion of tracheostomy tube|304341005|Awake intubation|418613003|Tracheal intubation through a laryngeal mask airway|424979004|Laryngeal mask airway insertion|427753009|Insertion of esophageal tracheal double lumen supraglottic airway|429161001|Insertion of endotracheal tube using laryngoscope|450611000124|Insertion of Single Lumen Supraglottic Airway Device"
  
  # car seat code for edisposition.14
  
  car_seat <- "4214001|Car Seat"
  
  # minor age units
  
  minor_age_units <- "days|hours|minutes"
  
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
    dplyr::select(-c({{  eresponse_05_col  }},
                     {{  earrest_01_col  }},
                     {{  einjury_03_col  }},
                     {{  eprocedures_03_col  }},
                     {{  edisposition_14_col  }},
                     {{  transport_disposition_col  }}
    )) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::mutate(patient_age_in_years = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{  patient_DOB_col  }},
      units = "days"
    )) / 365,
    
    # system age check
    system_age_minor1 = {{ epatient_15_col}} <= 8 & {{epatient_16_col }} == "Years",
    system_age_minor2 = {{ epatient_15_col}} < 96 & {{epatient_16_col }} == "Months",
    system_age_minor3 = {{ epatient_15_col}} <= 120 & grepl(pattern = minor_age_units, x = {{epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3,
    
    # calculated age check
    calc_age_minor = patient_age_in_years <= 8
    )
  
  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________
  
  cli::cli_progress_update(set = 4, id = progress_bar, force = T)
  
  # transports
  
  transport_data <- core_data |> 
    dplyr::select(Unique_ID, {{  transport_disposition_col  }}) |> 
    dplyr::filter(
      
      grepl(
        pattern = transport_responses,
        x = {{  transport_disposition_col  }},
        ignore.case = T
      )
      
    ) |> 
    distinct(Unique_ID) |> 
    pull(Unique_ID)
  
  cli::cli_progress_update(set = 5, id = progress_bar, force = T)
  
  # interfacility
  
  interfacility_data <- core_data |> 
    dplyr::select(Unique_ID, {{  eresponse_05_col  }}) |> 
    dplyr::filter(
      
      grepl(
        pattern = transport_code,
        x = {{ eresponse_05_col }},
        ignore.case = T
      )
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 6, id = progress_bar, force = T)
  
  # cardiac arrest
  
  cardiac_arrest_data <- core_data |> 
    dplyr::select(Unique_ID, {{  earrest_01_col  }}) |> 
    dplyr::filter(
      
      grepl(pattern = cardiac_arrest_responses, x = {{ earrest_01_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 7, id = progress_bar, force = T)
  
  # severe injury
  
  severe_injury_data <- core_data |> 
    dplyr::select(Unique_ID, {{  einjury_03_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(
      
      grepl(pattern = trauma_triage_crit, x = {{ einjury_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 8, id = progress_bar, force = T)
  
  # long board
  
  long_board_data <- core_data |> 
    dplyr::select(Unique_ID, {{  eprocedures_03_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(
      
      grepl(pattern = long_board, x = {{  eprocedures_03_col  }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 9, id = progress_bar, force = T)
  
  # airway procedure
  
  airway_proc_data <- core_data |> 
    dplyr::select(Unique_ID, {{  eprocedures_03_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(
      
      grepl(pattern = airway_procedures, x = {{ eprocedures_03_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 10, id = progress_bar, force = T)
  
  # car seat
  
  car_seat_data <- core_data |> 
    dplyr::select(Unique_ID, {{  edisposition_14_col  }}) |> 
    dplyr::distinct(Unique_ID, .keep_all = T) |> 
    dplyr::filter(
      
      grepl(pattern = car_seat, x = {{ edisposition_14_col }}, ignore.case = T)
      
    ) |> 
    dplyr::distinct(Unique_ID) |> 
    dplyr::pull(Unique_ID)
  
  cli::cli_progress_update(set = 11, id = progress_bar, force = T)
  
  # assign variables to final data
  
  initial_population <- final_data |> 
    dplyr::mutate(TRANSPORT = Unique_ID %in% transport_data,
                  INTERFACILITY = Unique_ID %in% interfacility_data,
                  TRANSPORT_OR_INTERFACILITY = TRANSPORT | INTERFACILITY,
                  CARDIAC_ARREST = Unique_ID %in% cardiac_arrest_data,
                  SEVERE_INJURY = Unique_ID %in% severe_injury_data,
                  LONG_BOARD = Unique_ID %in% long_board_data,
                  AIRWAY_PROCEDURE = Unique_ID %in% airway_proc_data,
                  CAR_SEAT = Unique_ID %in% car_seat_data
    ) |> 
    dplyr::filter(
      
      # filter down to age < 8 years
      system_age_minor | calc_age_minor,
      
      # NEMSIS 3.5 transports / interfacility only
      TRANSPORT_OR_INTERFACILITY
      
    )
  
  # Only calculate for pediatric patients < 8 yrs of age
  
  cli::cli_progress_update(set = 12, id = progress_bar, force = T)
  
  # filter peds for the exclusion criteria
  peds_pop <- initial_population |>
    dplyr::filter(!CARDIAC_ARREST &
                    !SEVERE_INJURY &
                    !LONG_BOARD &
                    !AIRWAY_PROCEDURE
    )
  
  # get the summary of results
  
  cli::cli_progress_update(set = 13, id = progress_bar, force = T)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(
      measure_name = "Safety-04",
      population_name = "< 8 yrs",
      numerator_col = sum(CAR_SEAT, na.rm = T),
      ...
    )
  
  cli::cli_progress_update(set = 14, id = progress_bar, force = T)
  
  # summary
  safety.04 <- peds_population
  
  cli::cli_progress_done()
  
  safety.04
  
  
}
