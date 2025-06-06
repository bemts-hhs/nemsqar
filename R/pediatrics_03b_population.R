#' @title Pediatrics-03B Populations
#'
#' @description
#'
#' Filters data down to the target populations for Pediatrics-03B, and
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
#' @param patient_scene_table A data.frame or tibble containing only ePatient
#'   and eScene fields as a fact table. Default is `NULL`.
#' @param response_table A data.frame or tibble containing only the eResponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param exam_table A data.frame or tibble containing only the eExam fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param medications_table A data.frame or tibble containing only the
#'   eMedications fields needed for this measure's calculations. Default is
#'   `NULL`.
#' @param erecord_01_col Column for unique EMS record identifiers.
#' @param incident_date_col Column that contains the incident date. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param patient_DOB_col Column that contains the patient's date of birth. This
#'   defaults to `NULL` as it is optional in case not available due to PII
#'   restrictions.
#' @param epatient_15_col Column giving the calculated age value.
#' @param epatient_16_col Column giving the provided age unit value.
#' @param eresponse_05_col Column containing the EMS response codes.
#' @param eexam_01_col Column containing documented weight information.
#' @param eexam_02_col Another column for weight documentation, if applicable.
#' @param emedications_03_col Column indicating medication administration.
#' @param emedications_04_col Column listing medications administered.
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
#' patient_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     incident_date = as.Date(c("2025-01-01", "2025-01-05",
#'                               "2025-02-01", "2025-01-01",
#'                               "2025-06-01")
#'                               ),
#'     patient_dob = as.Date(c("2000-01-01", "2020-01-01",
#'                             "2023-02-01", "2023-01-01",
#'                             "1970-06-01")
#'                             ),
#'     epatient_15 = c(25, 5, 2, 2, 55),  # Ages
#'     epatient_16 = c("Years", "Years", "Years", "Years", "Years")
#'
#' )
#'
#' response_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eresponse_05 = rep(2205001, 5)
#'
#' )
#'
#' exam_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   eexam_01 = c(60, 59, 58, 57, 56),
#'   eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
#' )
#'
#' medications_table <- tibble::tibble(
#'
#'   erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'   emedications_03 = rep("stuff", 5),
#'   emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
#'
#' )
#'
#' # test the success of the function
#'
#' result <- pediatrics_03b_population(patient_scene_table = patient_table,
#'                            response_table = response_table,
#'                            exam_table = exam_table,
#'                            medications_table = medications_table,
#'                            erecord_01_col = erecord_01,
#'                            incident_date_col = incident_date,
#'                            patient_DOB_col = patient_dob,
#'                            epatient_15_col = epatient_15,
#'                            epatient_16_col = epatient_16,
#'                            eresponse_05_col = eresponse_05,
#'                            emedications_03_col = emedications_03,
#'                            emedications_04_col = emedications_04,
#'                            eexam_01_col = eexam_01,
#'                            eexam_02_col = eexam_02
#'                            )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
pediatrics_03b_population <- function(df = NULL,
                           patient_scene_table = NULL,
                           response_table = NULL,
                           exam_table = NULL,
                           medications_table = NULL,
                           erecord_01_col,
                           incident_date_col = NULL,
                           patient_DOB_col = NULL,
                           epatient_15_col,
                           epatient_16_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col
                           ) {

  if(

    any(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(exam_table),
      !is.null(medications_table)
    )

    &&

    !is.null(df)

  ) {

    cli::cli_abort("{.fn pediatrics_03b_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

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
      missing(eexam_01_col),
      missing(eexam_02_col),
      missing(emedications_03_col),
      missing(emedications_04_col)
    )

  ) {

    cli::cli_abort("One or more of the *_col arguments is missing.  Please make sure you pass an unquoted column to each of the *_col arguments to run {.fn pediatrics_03b_population}.")

  }

  if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(exam_table),
      is.null(medications_table)
    )

    && is.null(df)

  ) {

    cli::cli_abort("{.fn pediatrics_03b_population} will only work by passing a {.cls data.frame} or {.cls tibble} to the {.var df} argument, or by fulfilling all table arguments.  Please choose to either pass an object of class {.cls data.frame} or {.cls tibble} to the {.var df} argument, or fulfill all table arguments.")

  }

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # non-weight-based medications
  non_weight_based_meds <- "inhalation|topical|9927049|9927009"

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
    "Running `pediatrics_03b_population()`",
    total = 9,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )


  # utilize applicable tables to analyze the data for the measure
  if(
    all(
      !is.null(patient_scene_table),
      !is.null(response_table),
      !is.null(exam_table),
      !is.null(medications_table)
    )

    && is.null(df)

  ) {

    # Ensure df is a data frame or tibble
    if (

      any(!(is.data.frame(patient_scene_table) && tibble::is_tibble(patient_scene_table)) ||

          !(is.data.frame(response_table) && tibble::is_tibble(response_table)) ||

          !(is.data.frame(exam_table) && tibble::is_tibble(exam_table)) ||

          !(is.data.frame(medications_table) && tibble::is_tibble(medications_table))

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

      cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

      ###_____________________________________________________________________________
      # from the full dataframe with all variables
      # create one fact table and several dimension tables
      # to complete calculations and avoid issues due to row
      # explosion
      ###_____________________________________________________________________________

      # fact table
      # the user should ensure that variables beyond those supplied for calculations
      # are distinct (i.e. one value or cell per patient)

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
        )
      ) {

      final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{  incident_date_col  }},
          time2 = {{  patient_DOB_col  }},
          units = "days"
        )) / 365,

        # system age check
        system_age_check1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check = system_age_check1 | system_age_check2,

        # calculated age check
        calc_age_check = patient_age_in_years_col < 18
        )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_DOB_col)
        )) {

        final_data <- patient_scene_table |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
        dplyr::mutate(

        # system age check
        system_age_check1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check = system_age_check1 | system_age_check2

        )

      }

      ###_____________________________________________________________________________
      ### dimension tables
      ### each dimension table is turned into a vector of unique IDs
      ### that are then utilized on the fact table to create distinct variables
      ### that tell if the patient had the characteristic or not for final
      ### calculations of the numerator and filtering
      ###_____________________________________________________________________________

      cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

      # non-weight based medications

      non_weight_based_meds_data <- medications_table |>
        dplyr::select({{ erecord_01_col }}, {{  emedications_04_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(grepl(
          pattern = non_weight_based_meds,
          x = {{ emedications_04_col }},
          ignore.case = TRUE
        )) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

      # meds not missing

      meds_not_missing_data <- medications_table |>
        dplyr::select({{ erecord_01_col }}, {{  emedications_03_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na({{ emedications_03_col }})
        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

      # 911 calls

      call_911_data <- response_table |>
        dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = TRUE)) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

      # documented weight 1

      documented_weight_data1 <- exam_table |>
        dplyr::select({{ erecord_01_col }}, {{  eexam_01_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          !is.na({{ eexam_01_col }})

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

      # documented weight 2
      documented_weight_data2 <- exam_table |>
        dplyr::select({{ erecord_01_col }}, {{  eexam_02_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          !is.na({{ eexam_02_col }})

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

      # assign variables to the final data
        computing_population <- final_data |>
        dplyr::mutate(NON_WEIGHT_BASED = {{ erecord_01_col }} %in% non_weight_based_meds_data,
                      MEDS_NOT_MISSING = {{ erecord_01_col }} %in% meds_not_missing_data,
                      CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                      DOCUMENTED_WEIGHT1 = {{ erecord_01_col }} %in% documented_weight_data1,
                      DOCUMENTED_WEIGHT2 = {{ erecord_01_col }} %in% documented_weight_data2,
                      DOCUMENTED_WEIGHT = DOCUMENTED_WEIGHT1 | DOCUMENTED_WEIGHT2
                      )

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
        )
      ) {

        # get the initial population
        initial_population <- computing_population |>
        dplyr::filter(
          # age filter
          system_age_check | calc_age_check,

          # only rows where meds are passed
          MEDS_NOT_MISSING,

          # only 911 calls
          CALL_911,

          # exclude non-weight based meds
          !NON_WEIGHT_BASED

        )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_DOB_col)
        )

      ) {

        initial_population <- computing_population |>
          dplyr::filter(

            # age filter
            system_age_check,

            # only rows where meds are passed
            MEDS_NOT_MISSING,

            # only 911 calls
            CALL_911,

            # exclude non-weight based meds
            !NON_WEIGHT_BASED

          )

      }

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

      # summarize counts for populations filtered
      filter_counts <- tibble::tibble(
        filter = c("Meds not missing",
                   "Non-Weight Based Meds",
                   "Documented Weight",
                   "911 calls",
                   "Peds denominator",
                   "Total dataset"
        ),
        count = c(
          sum(computing_population$MEDS_NOT_MISSING, na.rm = TRUE),
          sum(computing_population$NON_WEIGHT_BASED, na.rm = TRUE),
          sum(computing_population$DOCUMENTED_WEIGHT, na.rm = TRUE),
          sum(computing_population$CALL_911, na.rm = TRUE),
          nrow(initial_population),
          nrow(computing_population)
        )
      )

      cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

      # get the population of interest
      pediatrics.03b.population <- list(
        filter_process = filter_counts,
        initial_population = initial_population,
        computing_population = computing_population
      )

      # get the summary of results, already filtered down to the target age group for the measure

      cli::cli_progress_done(id = progress_bar_population)

      # summary
      return(pediatrics.03b.population)

    } else if(

    all(
      is.null(patient_scene_table),
      is.null(response_table),
      is.null(exam_table),
      is.null(medications_table)
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


  # initiate the progress bar process
      progress_bar_population <- cli::cli_progress_bar(
      "Running `pediatrics_03b_population()`",
      total = 9,
      type = "tasks",
      clear = F,
      format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
    )

      progress_bar_population

      cli::cli_progress_update(set = 1, id = progress_bar_population, force = TRUE)

      ###_____________________________________________________________________________
      # from the full dataframe with all variables
      # create one fact table and several dimension tables
      # to complete calculations and avoid issues due to row
      # explosion
      ###_____________________________________________________________________________

      # fact table
      # the user should ensure that variables beyond those supplied for calculations
      # are distinct (i.e. one value or cell per patient)

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
        )
      ) {

      final_data <- df |>
        dplyr::select(-c({{ eresponse_05_col }},
                         {{ eexam_01_col }},
                         {{ eexam_02_col }},
                         {{ emedications_03_col }},
                         {{ emedications_04_col }}

        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
        dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
          time1 = {{  incident_date_col  }},
          time2 = {{  patient_DOB_col  }},
          units = "days"
        )) / 365,

        # system age check
        system_age_check1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check = system_age_check1 | system_age_check2,

        # calculated age check
        calc_age_check = patient_age_in_years_col < 18
        )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_DOB_col)
        )) {

        final_data <- df |>
        dplyr::select(-c({{ eresponse_05_col }},
                         {{ eexam_01_col }},
                         {{ eexam_02_col }},
                         {{ emedications_03_col }},
                         {{ emedications_04_col }}

        )) |>
        dplyr::distinct({{ erecord_01_col }}, .keep_all = TRUE) |>
        dplyr::mutate(

        # system age check
        system_age_check1 = {{ epatient_15_col }} < 18 & grepl(pattern = year_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check2 = !is.na({{ epatient_15_col }}) & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = TRUE),
        system_age_check = system_age_check1 | system_age_check2

        )

      }

      ###_____________________________________________________________________________
      ### dimension tables
      ### each dimension table is turned into a vector of unique IDs
      ### that are then utilized on the fact table to create distinct variables
      ### that tell if the patient had the characteristic or not for final
      ### calculations of the numerator and filtering
      ###_____________________________________________________________________________

      cli::cli_progress_update(set = 2, id = progress_bar_population, force = TRUE)

      # non-weight based medications

      non_weight_based_meds_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{  emedications_04_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(grepl(
          pattern = non_weight_based_meds,
          x = {{ emedications_04_col }},
          ignore.case = TRUE
        )) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 3, id = progress_bar_population, force = TRUE)

      # meds not missing

      meds_not_missing_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{  emedications_03_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na({{ emedications_03_col }})
                      ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 4, id = progress_bar_population, force = TRUE)

      # 911 calls

      call_911_data <- df |>
        dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(grepl(pattern = codes_911, x = {{  eresponse_05_col  }}, ignore.case = TRUE)) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 5, id = progress_bar_population, force = TRUE)

      # documented weight 1

      documented_weight_data1 <- df |>
        dplyr::select({{ erecord_01_col }}, {{  eexam_01_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          !is.na({{ eexam_01_col }})

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 6, id = progress_bar_population, force = TRUE)

      # documented weight 2
      documented_weight_data2 <- df |>
        dplyr::select({{ erecord_01_col }}, {{  eexam_02_col  }}) |>
        dplyr::distinct() |>
        dplyr::filter(

          !is.na({{ eexam_02_col }})

        ) |>
        dplyr::distinct({{ erecord_01_col }}) |>
        dplyr::pull({{ erecord_01_col }})

      cli::cli_progress_update(set = 7, id = progress_bar_population, force = TRUE)

      # assign variables to the final data
      computing_population <- final_data |>
      dplyr::mutate(NON_WEIGHT_BASED = {{ erecord_01_col }} %in% non_weight_based_meds_data,
                    MEDS_NOT_MISSING = {{ erecord_01_col }} %in% meds_not_missing_data,
                    CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                    DOCUMENTED_WEIGHT1 = {{ erecord_01_col }} %in% documented_weight_data1,
                    DOCUMENTED_WEIGHT2 = {{ erecord_01_col }} %in% documented_weight_data2,
                    DOCUMENTED_WEIGHT = DOCUMENTED_WEIGHT1 | DOCUMENTED_WEIGHT2
                    )

      if (
        all(
          !rlang::quo_is_null(rlang::enquo(incident_date_col)),
          !rlang::quo_is_null(rlang::enquo(patient_DOB_col))
        )
      ) {


        # get the initial population
        initial_population <- computing_population |>
        dplyr::filter(
          # age filter
          system_age_check | calc_age_check,

          # only rows where meds are passed
          MEDS_NOT_MISSING,

          # only 911 calls
          CALL_911,

          # exclude non-weight based meds
          !NON_WEIGHT_BASED

        )

      } else if(

        all(
          is.null(incident_date_col),
          is.null(patient_DOB_col)
        )

      ) {

        initial_population <- computing_population |>
          dplyr::filter(

            # age filter
            system_age_check,

            # only rows where meds are passed
            MEDS_NOT_MISSING,

            # only 911 calls
            CALL_911,

            # exclude non-weight based meds
            !NON_WEIGHT_BASED

          )

      }

      cli::cli_progress_update(set = 8, id = progress_bar_population, force = TRUE)

      # summarize counts for populations filtered
      filter_counts <- tibble::tibble(
        filter = c("Meds not missing",
                   "Non-Weight Based Meds",
                   "Documented Weight",
                   "911 calls",
                   "Peds denominator",
                   "Total dataset"
        ),
        count = c(
          sum(computing_population$MEDS_NOT_MISSING, na.rm = TRUE),
          sum(computing_population$NON_WEIGHT_BASED, na.rm = TRUE),
          sum(computing_population$DOCUMENTED_WEIGHT, na.rm = TRUE),
          sum(computing_population$CALL_911, na.rm = TRUE),
          nrow(initial_population),
          nrow(computing_population)
        )
      )

      cli::cli_progress_update(set = 9, id = progress_bar_population, force = TRUE)

      # get the population of interest
      pediatrics.03b.population <- list(
        filter_process = filter_counts,
        initial_population = initial_population,
        computing_population = computing_population
      )

      # get the summary of results, already filtered down to the target age group for the measure

      cli::cli_progress_done(id = progress_bar_population)

      # summary
      return(pediatrics.03b.population)

    }
}
