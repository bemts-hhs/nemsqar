#' @title Airway-18 Populations
#'
#' @description
#'
#' This function processes and analyzes the dataset to generate the populations
#' of interest needed to perform calculations to obtain performance data.
#'
#' @param df A data frame or tibble containing the dataset to be processed.
#'   Default is `NULL`.
#' @param patient_scene_table A data frame or tibble containing only ePatient
#'   and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eResponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param procedures_table A data frame or tibble containing only the
#'   eProcedures fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param airway_table A data frame or tibble containing only the eAirway fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data frame or tibble containing only the eVitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col Column name containing the unique patient record
#'   identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_dob_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column name for patient information (exact purpose
#'   unclear).
#' @param epatient_16_col Column name for patient information (exact purpose
#'   unclear).
#' @param eresponse_05_col Column name for emergency response codes.
#' @param eprocedures_01_col Column name for procedure times or other related
#'   data.
#' @param eprocedures_02_col Column name for whether or not the procedure was
#'   performed prior to EMS care being provided.
#' @param eprocedures_03_col Column name for procedure codes.
#' @param eprocedures_05_col Column name for number of procedure attempts.
#' @param eprocedures_06_col Column name for procedure success codes.
#' @param eairway_02_col Column name for airway procedure data (datetime).
#'   Default is `NULL`.
#' @param eairway_04_col Column name for airway procedure data. Default is
#'   `NULL`.
#' @param evitals_01_col Column name for vital signs data (datetime).
#' @param evitals_16_col Column name for additional vital signs data.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#'
#' @examples
#'
#'\dontrun{
#'
#' # If you are sourcing your data from a SQL database connection
#' # or if you have your data in several different tables,
#' # you can pass table inputs versus a single data.frame or tibble
#'
#' # Get the applicable tables from `nemsqar`
#' data("nemsqar_airway_table")
#' data("nemsqar_patient_scene_table")
#' data("nemsqar_response_table")
#' data("nemsqar_vitals_table")
#' data("nemsqar_procedures_table")
#'
#' # Run the function
#'
#' airway_18_population(df = NULL,
#'          patient_scene_table = nemsqar_patient_scene_table,
#'          procedures_table = nemsqar_procedures_table,
#'          vitals_table = nemsqar_vitals_table,
#'          airway_table = nemsqar_airway_table,
#'          response_table = nemsqar_response_table,
#'          erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
#'          incident_date_col = `Incident Date`,
#'          patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
#'          epatient_15_col = `Patient Age (ePatient.15)`,
#'          epatient_16_col = `Patient Age Units (ePatient.16)`,
#'          eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
#'          eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
#'          eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
#'          eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
#'          eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
#'          eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
#'          eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
#'          eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
#'          evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
#'          evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
#'          )
#'}
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
airway_18_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      procedures_table = NULL,
                      vitals_table = NULL,
                      airway_table = NULL,
                      response_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      eprocedures_01_col,
                      eprocedures_02_col,
                      eprocedures_03_col,
                      eprocedures_05_col,
                      eprocedures_06_col,
                      eairway_02_col = NULL,
                      eairway_04_col = NULL,
                      evitals_01_col,
                      evitals_16_col) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(airway_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn airway_18_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(airway_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn airway_18_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure all *_col arguments are fulfilled

  if (
    any(
      missing(erecord_01_col),
      missing(incident_date_col),
      missing(patient_dob_col),
      missing(epatient_15_col),
      missing(epatient_16_col),
      missing(eresponse_05_col),
      missing(eprocedures_01_col),
      missing(eprocedures_02_col),
      missing(eprocedures_03_col),
      missing(eprocedures_05_col),
      missing(eprocedures_06_col),
      missing(eairway_02_col),
      missing(eairway_04_col),
      missing(evitals_01_col),
      missing(evitals_16_col)
    )
  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn airway_18_population}.")
  }

    # 911 codes for eResponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # endotracheal intubation attempts
  endotracheal_intubation <- "673005|Indirect laryngoscopy \\(procedure\\)|49077009|Flexible fiberoptic laryngoscopy \\(procedure\\)|78121007|Direct laryngoscopy \\(procedure\\)|112798008|Insertion of endotracheal tube \\(procedure\\)|16883004|Endotracheal intubation, emergency procedure \\(procedure\\)|182682004|Emergency laryngeal intubation \\(procedure\\)|232674004|Orotracheal intubation \\(procedure\\)|232677006|Tracheal intubation using rigid bronchoscope \\(procedure\\)|232678001|Orotracheal fiberoptic intubation \\(procedure\\)|232679009|Nasotracheal intubation \\(procedure\\)|232682004|Nasotracheal fiberoptic intubation \\(procedure\\)|232680007|Nasal intubation awake \\(procedure\\)|241689008|Rapid sequence induction \\(procedure\\)|304341005|Awake intubation \\(procedure\\)|397892004|Retrograde intubation \\(procedure\\)|418613003|Tracheal intubation through a laryngeal mask airway \\(procedure\\)|429705000|Intubation, combitube \\(procedure\\)|424979004|Laryngeal mask airway insertion \\(procedure\\)|427753009|Insertion of esophageal tracheal double lumen supraglottic airway \\(procedure\\)|429161001|Insertion of endotracheal tube using laryngoscope \\(procedure\\)|450601000124103|Orotracheal intubation using bougie device \\(procedure\\)|450611000124|Insertion of Single Lumen Supraglottic Airway Device \\(procedure\\)|1141752008|Flexible video intubation laryngoscope \\(physical object\\)|285696003|Fiberoptic laryngoscope \\(physical object\\)|420311007|Flexible fiberoptic laryngoscope \\(physical object\\)|421100004|Rigid fiberoptic laryngoscope \\(physical object\\)|44738004|Laryngoscope device \\(physical object\\)|469919007|Flexible video laryngoscope \\(physical object\\)|700640001|Rigid intubation laryngoscope \\(physical object\\)|701054002|Flexible fiberoptic intubation laryngoscope \\(physical object\\)|706013009|Intubation laryngoscope \\(physical object\\)|734928009|Rigid non-bladed video intubation laryngoscope \\(physical object\\)|879788006|Channeled video intubation laryngoscope \\(physical object\\)"

  # waveform ETCO2
  waveform_etco2 <- "4004019|Waveform ETCO2"

  # answer yes!
  yes_code <- "9923003|Yes"

  # minor values
  minor_values <- "days|hours|minutes|months"

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
    complete = cli::col_green("\u25CF"),  # Black Circle
    incomplete = cli::col_br_white("\u2500")  # Light Horizontal Line
  ))

  # Initialize the progress bar
  progress_bar_population <- cli::cli_progress_bar(
    "Running `airway_18_population()`",
    total = 16,
    type = "tasks",
    clear = FALSE,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table),
      !is.null(procedures_table),
      !is.null(vitals_table),
      !is.null(airway_table),
      !is.null(response_table)
    ) && is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
      is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
      is.data.frame(procedures_table) || tibble::is_tibble(procedures_table),
      is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
      is.data.frame(airway_table) || tibble::is_tibble(airway_table),
      is.data.frame(response_table) || tibble::is_tibble(response_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn airway_18_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
      )

    }

    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    ) {
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_dob_col)

      if (
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(patient_scene_table[[rlang::as_name(patient_dob)]]) &
         !lubridate::is.POSIXct(patient_scene_table[[rlang::as_name(patient_dob)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }

    # Use quasiquotation on the vitals, airway, and procedures datetime fields
    airway_datetime <- rlang::enquo(eairway_02_col)
    vitals_datetime <- rlang::enquo(evitals_01_col)
    procedures_datetime <- rlang::enquo(eprocedures_01_col)

    if(!rlang::quo_is_null(airway_datetime)) {

  # Validate the datetime fields in the patient_scene_table
  if ((!lubridate::is.Date(airway_table[[rlang::as_name(airway_datetime)]]) &
       !lubridate::is.POSIXct(airway_table[[rlang::as_name(airway_datetime)]])) ||
      (!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
       !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
      (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
       !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {

    cli::cli_abort(
      "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
    )

  }

  } else if(rlang::quo_is_null(airway_datetime)) {

    # Validate the datetime fields in the patient_scene_table
    if ((!lubridate::is.Date(vitals_table[[rlang::as_name(vitals_datetime)]]) &
         !lubridate::is.POSIXct(vitals_table[[rlang::as_name(vitals_datetime)]])) ||
        (!lubridate::is.Date(procedures_table[[rlang::as_name(procedures_datetime)]]) &
         !lubridate::is.POSIXct(procedures_table[[rlang::as_name(procedures_datetime)]]))) {

      cli::cli_abort(
        "For the variables {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
      )

    }
}

  # Progress update example (repeated throughout the script as necessary)
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  ###_____________________________________________________________________________

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
    patient_age_in_days_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_dob_col }},
      units = "days"
    )),

    # system age check
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,

    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18
    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )) {

    final_data <- patient_scene_table |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
      dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

      )

  }

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  # all invasive airway procedures
  intubation_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}, {{ eprocedures_06_col}}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

    # 911 calls
    call_911_data <- response_table |>
      dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

    cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

    # successful endotracheal intubation
    successful_intubation_data <- procedures_table |>
      dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}, {{ eprocedures_06_col}}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T),

        grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T)

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # Last successful invasive airway procedures in the initial population
  # suppress warnings due to an expected occurrence of some {{ erecord_01_col }} groups
  # returning `NA` values within eprocedures_01_col.  This is expected as
  # in some cases a date/time is not entered, which is a data entry issue but expected
  # we will not get any of the expected warning output flagging these cases

  # last procedures
  last_procedures_data <- suppressWarnings(

    procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_02_col }}, {{ eprocedures_03_col }}, {{ eprocedures_05_col }}, {{ eprocedures_06_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T),

      grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T),

      !is.na({{ eprocedures_01_col }}),

      !grepl(pattern = yes_code, x = {{ eprocedures_02_col }}, ignore.case = T)

      ) |>
    dplyr::filter(

      {{ eprocedures_05_col }} == max({{ eprocedures_05_col }}, na.rm = T),

      .by = {{ erecord_01_col }}

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  )

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # optionally use eairway fields
  if(!rlang::quo_is_null(airway_datetime)) {

  # numerator 1
  numerator_data1 <- suppressWarnings(
    airway_table |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_02_col}}, {{ eairway_04_col }}
                  ) |>
    dplyr::distinct() |>
    dplyr::left_join(procedures_table |> dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}) |> dplyr::distinct(),
                     by = rlang::as_name(rlang::enquo(erecord_01_col))
                     ) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = waveform_etco2, x = {{ eairway_04_col }}, ignore.case = T),

      !is.na({{ eairway_02_col }}),

      {{ eairway_02_col }} > {{ eprocedures_01_col }}

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

    )

  }

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # numerator 2
  numerator_data2 <- suppressWarnings(
    procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}) |>
    dplyr::distinct() |>
    dplyr::left_join(vitals_table |> dplyr::select({{ erecord_01_col }}, {{ evitals_01_col }}, {{ evitals_16_col }}) |> dplyr::distinct(),
                     by = rlang::as_name(rlang::enquo(erecord_01_col))
                     ) |>
    dplyr::distinct() |>
    dplyr::filter(

     ( {{ evitals_01_col }} > {{ eprocedures_01_col }} ) &

       {{ evitals_16_col }} >= 5

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

    )

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # numerator 3
  numerator_data3 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

        {{ evitals_16_col }} >= 5

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # optionally use eairway fields to get counts
  if(!rlang::quo_is_null(airway_datetime)) {

  # numerator calculation part 1
  waveform_ETCO2_data <- airway_table |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_04_col }}
    ) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = waveform_etco2, x = {{ eairway_04_col }}, ignore.case = T)


    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # numerator calculation part 2
  airway_procedure_time_data <- suppressWarnings(

    airway_table |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_02_col }}
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(

      procedures_table |> dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}) |> dplyr::distinct(),

      by = rlang::as_name(rlang::enquo(erecord_01_col))

      ) |>
    dplyr::filter(

      !is.na({{ eairway_02_col }}) &

      ( {{ eairway_02_col }} > {{ eprocedures_01_col }} )


    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

    )

  }

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  if(!rlang::quo_is_null(airway_datetime)) {

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% successful_intubation_data,
                  ENDOTRACHEAL_INTUBATION = {{ erecord_01_col }} %in% intubation_data,
                  LAST_SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% last_procedures_data,
                  WAVEFORM_ETCO2 = {{ erecord_01_col }} %in% waveform_ETCO2_data,
                  AIRWAY_PROCEDURE_TIME = {{ erecord_01_col }} %in% airway_procedure_time_data,
                  NUMERATOR1 = {{ erecord_01_col }} %in% numerator_data1,
                  NUMERATOR2 = {{ erecord_01_col }} %in% numerator_data2,
                  NUMERATOR3 = {{ erecord_01_col }} %in% numerator_data3,
                  NUMERATOR = NUMERATOR1 & NUMERATOR2
                  )

  } else if(rlang::quo_is_null(airway_datetime)) {

    # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% successful_procedure_data,
                  ENDOTRACHEAL_INTUBATION = {{ erecord_01_col }} %in% intubation_data,
                  LAST_SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% last_procedures_data,
                  NUMERATOR2 = {{ erecord_01_col }} %in% numerator_data2,
                  NUMERATOR3 = {{ erecord_01_col }} %in% numerator_data3,
                  NUMERATOR = NUMERATOR2
                  )

  }

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      ENDOTRACHEAL_INTUBATION,
      CALL_911

    )

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # Adult and Pediatric Populations

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult,
                  LAST_SUCCESSFUL_PROCEDURE
                  )

  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor,
                  LAST_SUCCESSFUL_PROCEDURE
                  )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult,
                    LAST_SUCCESSFUL_PROCEDURE
                    )

    cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor,
                    LAST_SUCCESSFUL_PROCEDURE
                    )

  }

  # summarize

  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)


  if(!rlang::quo_is_null(airway_datetime)) {

  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Invasive airway procedures",
                 "Successful invasive airway procedures",
                 "911 calls",
                 "Last successful invasive airway procedures",
                 "Waveform ETCO2 used",
                 "Airway device placement confirmed after airway procedure",
                 "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                 "Waveform ETCO2 >= 5",
                 "Last successful invasive airway procedures with waveform ETCO2",
                 "Adults denominator",
                 "Peds denominator",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$ENDOTRACHEAL_INTUBATION, na.rm = T),
        sum(computing_population$SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$LAST_SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$WAVEFORM_ETCO2, na.rm = T),
        sum(computing_population$AIRWAY_PROCEDURE_TIME, na.rm = T),
        sum(computing_population$NUMERATOR2, na.rm = T),
        sum(computing_population$NUMERATOR3, na.rm = T),
        sum(computing_population$NUMERATOR, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(computing_population)
      )
    )

  } else if(rlang::quo_is_null(airway_datetime)) {

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Invasive airway procedures",
                 "Successful invasive airway procedures",
                 "911 calls",
                 "Last successful invasive airway procedures",
                 "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                 "Waveform ETCO2 >= 5",
                 "Last successful invasive airway procedures with waveform ETCO2",
                 "Adults denominator",
                 "Peds denominator",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$ENDOTRACHEAL_INTUBATION, na.rm = T),
        sum(computing_population$SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$LAST_SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$NUMERATOR2, na.rm = T),
        sum(computing_population$NUMERATOR3, na.rm = T),
        sum(computing_population$NUMERATOR, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(computing_population)
      )
    )

  }

    # get the populations of interest

    cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    airway.18.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop
    )

  cli::cli_progress_done(id = progress_bar_population)

  return(airway.18.population)

  } else if(
    all(
      is.null(patient_scene_table),
      is.null(procedures_table),
      is.null(vitals_table),
      is.null(airway_table),
      is.null(response_table)
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

    # Validate date columns if provided
    if (
      all(
        !rlang::quo_is_null(rlang::enquo(incident_date_col)),
        !rlang::quo_is_null(rlang::enquo(patient_dob_col))
      )
    ) {
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_dob_col)

      if (
        (!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
        (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))
      ) {
        cli::cli_abort(
          "For the variables {.var incident_date_col} and {.var patient_dob_col}, one or both were not of class {.cls Date} or a similar class. Please format these variables to class {.cls Date} or a similar class."
        )
      }
    }

    # Use quasiquotation on the vitals, airway, and procedures datetime fields
    airway_datetime <- rlang::enquo(eairway_02_col)
    vitals_datetime <- rlang::enquo(evitals_01_col)
    procedures_datetime <- rlang::enquo(eprocedures_01_col)

    if(!rlang::quo_is_null(airway_datetime)) {

  # Validate the datetime fields in the df
  if ((!lubridate::is.Date(df[[rlang::as_name(airway_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(airway_datetime)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {

    cli::cli_abort(
      "For the variables {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eairway_02_col}, {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
    )

  }

  } else if(rlang::quo_is_null(airway_datetime)) {

    # Validate the datetime fields in the df
    if ((!lubridate::is.Date(df[[rlang::as_name(vitals_datetime)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(vitals_datetime)]])) ||
        (!lubridate::is.Date(df[[rlang::as_name(procedures_datetime)]]) &
         !lubridate::is.POSIXct(df[[rlang::as_name(procedures_datetime)]]))) {

      cli::cli_abort(
        "For the variables {.var eprocedures_01_col}, and {.var evitals_01_col}, one or a combination of these variables were not of class {.cls Date} or a similar class. Please format your {.var eprocedures_01_col}, and {.var evitals_01_col} to class {.cls Date} or a similar class."
      )

    }
}

  # Progress update example (repeated throughout the script as necessary)
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

  ###_____________________________________________________________________________
  # from the full dataframe with all variables
  # create one fact table and several dimension tables
  # to complete calculations and avoid issues due to row
  # explosion
  ###_____________________________________________________________________________

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  final_data <- df |>
    dplyr::select(-c(
      {{ eresponse_05_col }},
      {{ eprocedures_01_col }},
      {{ eprocedures_02_col }},
      {{ eprocedures_03_col }},
      {{ eprocedures_05_col }},
      {{ eprocedures_06_col }},
      {{ eairway_02_col }},
      {{ eairway_04_col }},
      {{ evitals_01_col }},
      {{ evitals_16_col }}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_dob_col }},
      units = "days"
    )) / 365,
    patient_age_in_days_col = as.numeric(difftime(
      time1 = {{ incident_date_col }},
      time2 = {{ patient_dob_col }},
      units = "days"
    )),

    # system age check
    system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
    system_age_minor = system_age_minor1 | system_age_minor2,

    # calculated age check
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18
    )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )) {

    final_data <- df |>
      dplyr::select(-c(
        {{ eresponse_05_col }},
        {{ eprocedures_01_col }},
        {{ eprocedures_02_col }},
        {{ eprocedures_03_col }},
        {{ eprocedures_05_col }},
        {{ eprocedures_06_col }},
        {{ eairway_02_col }},
        {{ eairway_04_col }},
        {{ evitals_01_col }},
        {{ evitals_16_col }}
      )) |>
      dplyr::distinct({{ erecord_01_col }}, .keep_all = T) |>
      dplyr::mutate(

      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2

      )

  }

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  # all invasive airway procedures
  intubation_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}, {{ eprocedures_06_col}}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T)


    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{ eresponse_05_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = codes_911, x = {{ eresponse_05_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # successful endotracheal intubation
  successful_intubation_data <- procedures_table |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_03_col }}, {{ eprocedures_06_col}}) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T),

      grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # Last successful invasive airway procedures in the initial population
  # suppress warnings due to an expected occurrence of some {{ erecord_01_col }} groups
  # returning `NA` values within eprocedures_01_col.  This is expected as
  # in some cases a date/time is not entered, which is a data entry issue but expected
  # we will not get any of the expected warning output flagging these cases

  # last procedures
  last_procedures_data <- suppressWarnings(

    procedures_table |>
      dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ eprocedures_02_col }}, {{ eprocedures_03_col }}, {{ eprocedures_05_col }}, {{ eprocedures_06_col }}) |>
      dplyr::distinct() |>
      dplyr::filter(

        grepl(pattern = endotracheal_intubation, x = {{ eprocedures_03_col }}, ignore.case = T),

        grepl(pattern = yes_code, x = {{ eprocedures_06_col }}, ignore.case = T),

        !is.na({{ eprocedures_01_col }}),

        !grepl(pattern = yes_code, x = {{ eprocedures_02_col }}, ignore.case = T)

      ) |>
      dplyr::filter(

        {{ eprocedures_05_col }} == max({{ eprocedures_05_col }}, na.rm = T),

        .by = {{ erecord_01_col }}

      ) |>
      dplyr::distinct({{ erecord_01_col }}) |>
      dplyr::pull({{ erecord_01_col }})

  )

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # optionally use eairway fields
  if(!rlang::quo_is_null(airway_datetime)) {

  # numerator 1
  numerator_data1 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_02_col}}, {{ eairway_04_col }}, {{ eprocedures_01_col }}
                  ) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = waveform_etco2, x = {{ eairway_04_col }}, ignore.case = T),

      !is.na({{ eairway_02_col }}),

      {{ eairway_02_col }} > {{ eprocedures_01_col }}

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  }

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # numerator 2
  numerator_data2 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eprocedures_01_col }}, {{ evitals_01_col }}, {{ evitals_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

     ( {{ evitals_01_col }} > {{ eprocedures_01_col }} ) &

       {{ evitals_16_col }} >= 5

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # numerator 3
  numerator_data3 <- df |>
    dplyr::select({{ erecord_01_col }}, {{ evitals_16_col }}) |>
    dplyr::distinct() |>
    dplyr::filter(

        {{ evitals_16_col }} >= 5

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # optionally use eairway fields to get counts
  if(!rlang::quo_is_null(airway_datetime)) {

  # numerator calculation part 1
  waveform_ETCO2_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_04_col }}
    ) |>
    dplyr::distinct() |>
    dplyr::filter(

      grepl(pattern = waveform_etco2, x = {{ eairway_04_col }}, ignore.case = T)


    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # numerator calculation part 2
  airway_procedure_time_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{ eairway_02_col }}, {{ eprocedures_01_col }}
    ) |>
    dplyr::distinct() |>
    dplyr::filter(

      !is.na({{ eairway_02_col }}) &

      ( {{ eairway_02_col }} > {{ eprocedures_01_col }} )


    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  }

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  if(!rlang::quo_is_null(airway_datetime)) {

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% successful_intubation_data,
                  ENDOTRACHEAL_INTUBATION = {{ erecord_01_col }} %in% intubation_data,
                  LAST_SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% last_procedures_data,
                  WAVEFORM_ETCO2 = {{ erecord_01_col }} %in% waveform_ETCO2_data,
                  AIRWAY_PROCEDURE_TIME = {{ erecord_01_col }} %in% airway_procedure_time_data,
                  NUMERATOR1 = {{ erecord_01_col }} %in% numerator_data1,
                  NUMERATOR2 = {{ erecord_01_col }} %in% numerator_data2,
                  NUMERATOR3 = {{ erecord_01_col }} %in% numerator_data3,
                  NUMERATOR = NUMERATOR1 & NUMERATOR2
                  )

  } else if(rlang::quo_is_null(airway_datetime)) {

    # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% successful_procedure_data,
                  ENDOTRACHEAL_INTUBATION = {{ erecord_01_col }} %in% intubation_data,
                  LAST_SUCCESSFUL_PROCEDURE = {{ erecord_01_col }} %in% last_procedures_data,
                  NUMERATOR2 = {{ erecord_01_col }} %in% numerator_data2,
                  NUMERATOR3 = {{ erecord_01_col }} %in% numerator_data3,
                  NUMERATOR = NUMERATOR2
                  )

  }

  cli::cli_progress_update(set = 12, id = progress_bar_population, force = T)

  # get the initial population
  initial_population <- computing_population |>
    dplyr::filter(

      ENDOTRACHEAL_INTUBATION,
      CALL_911

    )

  cli::cli_progress_update(set = 13, id = progress_bar_population, force = T)

  # Adult and Pediatric Populations

  if (
    all(
      !rlang::quo_is_null(rlang::enquo(incident_date_col)),
      !rlang::quo_is_null(rlang::enquo(patient_dob_col))
    )
  ) {

  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult,
                  LAST_SUCCESSFUL_PROCEDURE
                  )

  cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor,
                  LAST_SUCCESSFUL_PROCEDURE
                  )

  } else if(

    all(
      is.null(incident_date_col),
      is.null(patient_dob_col)
    )) {

    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult,
                    LAST_SUCCESSFUL_PROCEDURE
                    )

    cli::cli_progress_update(set = 14, id = progress_bar_population, force = T)

    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor,
                    LAST_SUCCESSFUL_PROCEDURE
                    )

  }

  # summarize

  cli::cli_progress_update(set = 15, id = progress_bar_population, force = T)


  if(!rlang::quo_is_null(airway_datetime)) {

  # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Invasive airway procedures",
                 "Successful invasive airway procedures",
                 "911 calls",
                 "Last successful invasive airway procedures",
                 "Waveform ETCO2 used",
                 "Airway device placement confirmed after airway procedure",
                 "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                 "Waveform ETCO2 >= 5",
                 "Last successful invasive airway procedures with waveform ETCO2",
                 "Adults denominator",
                 "Peds denominator",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$ENDOTRACHEAL_INTUBATION, na.rm = T),
        sum(computing_population$SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$LAST_SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$WAVEFORM_ETCO2, na.rm = T),
        sum(computing_population$AIRWAY_PROCEDURE_TIME, na.rm = T),
        sum(computing_population$NUMERATOR2, na.rm = T),
        sum(computing_population$NUMERATOR3, na.rm = T),
        sum(computing_population$NUMERATOR, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(computing_population)
      )
    )

  } else if(rlang::quo_is_null(airway_datetime)) {

    # summarize counts for populations filtered
    filter_counts <- tibble::tibble(
      filter = c("Invasive airway procedures",
                 "Successful invasive airway procedures",
                 "911 calls",
                 "Last successful invasive airway procedures",
                 "Vitals taken after airway procedure where waveform ETCO2 >= 5",
                 "Waveform ETCO2 >= 5",
                 "Last successful invasive airway procedures with waveform ETCO2",
                 "Adults denominator",
                 "Peds denominator",
                 "Total dataset"
      ),
      count = c(
        sum(computing_population$ENDOTRACHEAL_INTUBATION, na.rm = T),
        sum(computing_population$SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$CALL_911, na.rm = T),
        sum(computing_population$LAST_SUCCESSFUL_PROCEDURE, na.rm = T),
        sum(computing_population$NUMERATOR2, na.rm = T),
        sum(computing_population$NUMERATOR3, na.rm = T),
        sum(computing_population$NUMERATOR, na.rm = T),
        nrow(adult_pop),
        nrow(peds_pop),
        nrow(computing_population)
      )
    )

  }

    # get the populations of interest

    cli::cli_progress_update(set = 16, id = progress_bar_population, force = T)

    # gather data into a list for multi-use output
    airway.18.population <- list(
      filter_process = filter_counts,
      adults = adult_pop,
      peds = peds_pop
    )

  cli::cli_progress_done(id = progress_bar_population)

  return(airway.18.population)

  }

}
