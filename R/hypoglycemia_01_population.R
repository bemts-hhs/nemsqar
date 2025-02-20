#' @title Hypoglycemia-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Hypoglycemia-01, and
#' categorizes records to identify needed information for the calculations.
#'
#' Identifies key categories related to diabetes/hypoglycemia incidents in an
#' EMS dataset, specifically focusing on cases where 911 was called for
#' diabetes/hypoglycemia distress, certain medications were administered, and a
#' weight is taken. This function segments the data into pediatric populations,
#' computing the proportion of cases that have a documented weight.
#'
#' @param df A data frame or tibble containing emergency response records.
#'   Default is `NULL`.
#' @param patient_scene_table A data.frame or tibble containing at least
#'   epatient and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing at least the
#'   eresponse fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing at least the
#'   esituation fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param vitals_table A data.frame or tibble containing at least the evitals
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing at least the
#'   emedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param procedures_table A data.frame or tibble containing at least the
#'   eprocedures fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col Column representing the unique record identifier.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column representing the patient's numeric age agnostic
#'   of unit.
#' @param epatient_16_col Column representing the patient's age unit ("Years",
#'   "Months", "Days", "Hours", or "Minute").
#' @param eresponse_05_col Column containing response type codes.
#' @param esituation_11_col Column for primary impression fields, containing
#'   ICD-10 codes.
#' @param esituation_12_col Column for secondary impression fields, containing
#'   ICD-10 codes.
#' @param evitals_18_col Column for blood glucose levels.
#' @param evitals_23_col Column for Glasgow Coma Scale (GCS) scores.
#' @param evitals_26_col Column for AVPU alertness levels.
#' @param emedications_03_col Column for administered medications.
#' @param eprocedures_03_col Column for procedures performed.
#'
#' @return A list that contains the following:
#' * a tibble with counts for each filtering step,
#' * a tibble for each population of interest
#' * a tibble for the initial population
#' * a tibble for the total dataset with computations
#'
#' @examples
#'
#' # create tables to test correct functioning
#'
#' # patient table
#' patient_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
#'   "2025-01-01", "2025-06-01")),
#'   patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
#'   "2023-01-01", "1970-06-01")),
#'   epatient_15 = c(25, 5, 2, 2, 55),  # Ages
#'   epatient_16 = c("Years", "Years", "Years", "Years", "Years")
#'
#' )
#'
#' # response table
#' response_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eresponse_05 = rep(2205001, 5)
#'
#' )
#'
#' # situation table
#' situation_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   esituation_11 = c(rep("E13.64", 3), rep("E16.2", 2)),
#'   esituation_12 = c(rep("E13.64", 2), rep("E16.2", 3))
#' )
#'
#' # medications table
#' medications_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   emedications_03 = c(372326, 376937, 377980, 4850, 4832),
#'
#' )
#'
#' # vitals table
#' vitals_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   evitals_18 = c(60, 59, 58, 57, 56),
#'   evitals_23 = c(16, 15, 14, 13, 12),
#'   evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert")
#'
#' )
#'
#' # procedures table
#' procedures_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eprocedures_03 = rep("710925007", 5)
#'
#' )
#'
#' # test the success of the function
#' result <- hypoglycemia_01_population(patient_scene_table = patient_table,
#'                             response_table = response_table,
#'                             situation_table = situation_table,
#'                             medications_table = medications_table,
#'                             vitals_table = vitals_table,
#'                             procedures_table = procedures_table,
#'                             erecord_01_col = erecord_01,
#'                             incident_date_col = incident_date,
#'                             patient_DOB_col = patient_dob,
#'                             epatient_15_col = epatient_15,
#'                             epatient_16_col = epatient_16,
#'                             eresponse_05_col = eresponse_05,
#'                             esituation_11_col = esituation_11,
#'                             esituation_12_col = esituation_12,
#'                             emedications_03_col = emedications_03,
#'                             evitals_18_col = evitals_18,
#'                             evitals_23_col = evitals_23,
#'                             evitals_26_col = evitals_26,
#'                             eprocedures_03_col = eprocedures_03
#'                             )
#'
#' # show the results of filtering at each step
#' result$filter_process
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
                            evitals_23_col,
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

    cli::cli_abort("{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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

    cli::cli_abort("{.fn hypoglycemia_01_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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
      missing(evitals_23_col),
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
  altered_mental_status <- "R41.82|Altered Mental Status, unspecified"

  # codes for diabetes via primary and secondary impression

  diabetes_codes <- "(?:E13.64|E16.2)|Other specified diabetes mellitus with hypoglycemia|Hypoglycemia, unspecified"

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
    complete = cli::col_green("\u25CF"),  # Black Circle
    incomplete = cli::col_br_white("\u2500")  # Light Horizontal Line
  ))

  # initiate the progress bar process
  progress_bar_population <- cli::cli_progress_bar(
    "Running `hypoglycemia_01_population()`",
    total = 17,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
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

    # Ensure df is a data frame or tibble
    if (

      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||

          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) ||

          !(is.data.frame(situation_table) && tibble::is_tibble(situation_table)) ||

          !(is.data.frame(vitals_table) && tibble::is_tibble(vitals_table)) ||

          !(is.data.frame(medications_table) && tibble::is_tibble(medications_table)) ||

          !(is.data.frame(procedures_table) && tibble::is_tibble(procedures_table))

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
        !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
      )
    ) {
      # Use quasiquotation on the date variables to check format
      incident_date <- rlang::enquo(incident_date_col)
      patient_dob <- rlang::enquo(patient_DOB_col)

      # Convert quosures to names and check the column classes
      incident_date_name <- rlang::as_name(incident_date)
      patient_dob_name <- rlang::as_name(patient_dob)

      if ((!lubridate::is.Date(patient_scene_table[[incident_date_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[incident_date_name]])) ||
          (!lubridate::is.Date(patient_scene_table[[patient_dob_name]]) &
           !lubridate::is.POSIXct(patient_scene_table[[patient_dob_name]]))) {

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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_col  }}) |>
    dplyr::distinct() |>
    dplyr::filter({{ evitals_23_col }} < 15) |>
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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    initial_population = initial_population,
    computing_population = computing_population
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
      patient_dob <- rlang::enquo(patient_DOB_col)

      if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
          (!lubridate::is.Date(df[[rlang::as_name(patient_dob)]]) &
           !lubridate::is.POSIXct(df[[rlang::as_name(patient_dob)]]))) {

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
                     {{  evitals_23_col  }},
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
                     {{  evitals_23_col  }},
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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_col  }}) |>
    dplyr::distinct() |>
    dplyr::filter({{ evitals_23_col }} < 15) |>
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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

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
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T)

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
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T)

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
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(hypoglycemia.01.population)

  }

}

