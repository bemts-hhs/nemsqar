#' @title Stroke-01 Populations
#'
#' @description
#'
#' Filters data down to the target populations for Stroke-01, and categorizes
#' records to identify needed information for the calculations.
#'
#' Identifies key categories related to stroke-related incidents in an EMS
#' dataset, specifically focusing on cases where 911 was called for stroke, and
#' a stroke scale was administered. .
#'
#' @param df A data frame or tibble containing the dataset. Each row should
#'   represent a unique patient encounter.
#' @param patient_scene_table A data frame or tibble containing only epatient
#'   and escene fields as a fact table. Default is `NULL`.
#' @param response_table A data frame or tibble containing only the eresponse
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param situation_table A data.frame or tibble containing only the esituation
#'   fields needed for this measure's calculations. Default is `NULL`.
#' @param vitals_table A data.frame or tibble containing only the evitals fields
#'   needed for this measure's calculations. Default is `NULL`.
#' @param erecord_01_col The column containing unique record identifiers for
#'   each encounter.
#' @param eresponse_05_col The column containing EMS response codes, which
#'   should include 911 response codes.
#' @param esituation_11_col The column containing the primary impression codes
#'   or descriptions related to the situation.
#' @param esituation_12_col The column containing secondary impression codes or
#'   descriptions related to the situation.
#' @param evitals_23_col The column containing the Glasgow Coma Scale (GCS)
#'   score.
#' @param evitals_26_col The column containing the AVPU (alert, verbal, pain,
#'   unresponsive) scale value.
#' @param evitals_29_col The column containing the stroke scale score achieved
#'   during assessment.
#' @param evitals_30_col The column containing stroke scale type descriptors
#'   (e.g., FAST, NIH, etc.).
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
#'   # patient table
#'   patient_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
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
#'   )
#'
#'   # response table
#'   response_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     eresponse_05 = rep(2205001, 5)
#'
#'   )
#'
#'   # situation table
#'   situation_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     esituation_11 = c(rep("I60", 3), rep("I61", 2)),
#'     esituation_12 = c(rep("I63", 2), rep("I64", 3)),
#'   )
#'
#'   # vitals table
#'   vitals_table <- tibble::tibble(
#'
#'     erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
#'     evitals_23 = c(16, 15, 14, 13, 12),
#'     evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
#'     evitals_29 = rep("positive", 5),
#'     evitals_30 = rep("a pain scale", 5)
#'   )
#'
#'   # test the success of the function
#'   result <- stroke_01_population(patient_scene_table = patient_table,
#'                               response_table = response_table,
#'                               situation_table = situation_table,
#'                               vitals_table = vitals_table,
#'                               erecord_01_col = erecord_01,
#'                               eresponse_05_col = eresponse_05,
#'                               esituation_11_col = esituation_11,
#'                               esituation_12_col = esituation_12,
#'                               evitals_29_col = evitals_29,
#'                               evitals_23_col = evitals_23,
#'                               evitals_26_col = evitals_26,
#'                               evitals_30_col = evitals_30
#'                               )
#'
#' # show the results of filtering at each step
#' result$filter_process
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @export
#'
stroke_01_population <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      situation_table = NULL,
                      vitals_table = NULL,
                      erecord_01_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      evitals_23_col,
                      evitals_26_col,
                      evitals_29_col,
                      evitals_30_col
                      ) {

  # Ensure that not all table arguments AND the df argument are fulfilled
  # User must pass either `df` or all table arguments, but not both

  if (
    any(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&
    !is.null(df)
  ) {
    cli::cli_abort("{.fn stroke_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure that df or all table arguments are fulfilled

  if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)
    ) &&
    is.null(df)
  ) {
    cli::cli_abort("{.fn stroke_01_population} requires either a {.cls data.frame} or {.cls tibble} passed to the {.var df} argument, or all table arguments to be fulfilled. Please choose one approach.")
  }

  # Ensure all *_col arguments are fulfilled

  if (
    any(
      missing(erecord_01_col),
      missing(eresponse_05_col),
      missing(esituation_11_col),
      missing(esituation_12_col),
      missing(evitals_23_col),
      missing(evitals_26_col),
      missing(evitals_29_col),
      missing(evitals_30_col)
    )
  ) {
    cli::cli_abort("One or more of the *_col arguments is missing. Please ensure you pass an unquoted column to each of the *_col arguments to run {.fn stroke_01_population}.")
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
    "Running `stroke_01_population()`",
    total = 11,
    type = "tasks",
    clear = F,
    format = "{cli::pb_name} [Working on {cli::pb_current} of {cli::pb_total} tasks] {cli::pb_bar} | {cli::col_blue('Progress')}: {cli::pb_percent} | {cli::col_blue('Runtime')}: [{cli::pb_elapsed}]"
  )

  progress_bar_population

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions

  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009|Emergency Response \\(Primary Response Area\\)|Emergency Response \\(Intercept\\)|Emergency Response \\(Mutual Aid\\)"

  # primary and secondary provider impression values
  stroke_pattern <- "(?:I6[013]|G4[56])|Nontraumatic subarachnoid hemorrhage|Nontraumatic intracerebral hemorrhage|Cerebral infarction|Transient cerebral ischemic attacks|Vascular syndromes of brain in cerebrovascular diseases"

  # AVPU exclusion
  avpu_pattern <- "3326007|Unresponsive"

  # stroke score not values
  stroke_values <- "positive|negative|non-conclusive"

  # scale_values
  scale_values <- "F\\.A\\.S\\.T\\. Exam|Miami Emergency Neurologic Deficit \\(MEND\\)|Cincinnati|Other Stroke Scale Type|NIH|Los Angeles|RACE \\(Rapid Arterial Occlusion Evaluation\\)|Los Angeles Motor Score \\(LAMS\\)|Massachusetts"

  # utilize applicable tables to analyze the data for the measure

  if (
    all(
      !is.null(patient_scene_table),
      !is.null(vitals_table),
      !is.null(situation_table),
      !is.null(response_table)
    ) &&

      is.null(df)

  ) {

    # Ensure all tables are of class `data.frame` or `tibble`
    if (

      !all(
        is.data.frame(patient_scene_table) || tibble::is_tibble(patient_scene_table),
        is.data.frame(vitals_table) || tibble::is_tibble(vitals_table),
        is.data.frame(situation_table) || tibble::is_tibble(situation_table),
        is.data.frame(response_table) || tibble::is_tibble(response_table)
      )

    ) {

      cli::cli_abort(
        "One or more of the tables passed to {.fn stroke_01_population} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables, all tables must be of class {.cls data.frame} or {.cls tibble}."
      )

    }

  ###_____________________________________________________________________________
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  ###_____________________________________________________________________________

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

  final_data <- patient_scene_table |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # stroke 1
  stroke_data1 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_11_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # stroke 2
  stroke_data2 <- situation_table |>
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_12_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- response_table |>
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # GCS
  GCS_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_col  }}) |>
    dplyr::filter(

      {{evitals_23_col}} <= 9

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # AVPU
  AVPU_data <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_26_col  }}) |>
    dplyr::filter(

      grepl(pattern = avpu_pattern, x = {{ evitals_26_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # stroke scale 1
  stroke_scale_data1 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_29_col  }}) |>
    dplyr::filter(

      !is.na({{evitals_29_col}}) & grepl(pattern = stroke_values, x = {{evitals_29_col}}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # stroke scale 2
  stroke_scale_data2 <- vitals_table |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_30_col  }}) |>
    dplyr::filter(

      !is.na({{evitals_30_col}}) & grepl(pattern = scale_values, x = {{evitals_30_col}}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(STROKE1 = {{ erecord_01_col }} %in% stroke_data1,
                  STROKE2 = {{ erecord_01_col }} %in% stroke_data2,
                  STROKE = STROKE1 | STROKE2,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  GCS = {{ erecord_01_col }} %in% GCS_data,
                  AVPU = {{ erecord_01_col }} %in% AVPU_data,
                  NOT_GCS_AVPU = !GCS | !AVPU,
                  STROKE_SCALE1 = {{ erecord_01_col }} %in% stroke_scale_data1,
                  STROKE_SCALE2 = {{ erecord_01_col }} %in% stroke_scale_data2,
                  STROKE_SCALE = STROKE_SCALE1 | STROKE_SCALE2
                  )


    initial_population <- computing_population |>
    dplyr::filter(

      # Identify Records that have seizure documentation defined above
      STROKE,

      # filter down to 911 calls
      CALL_911,

      # no GCS < 9 or AVPU not equal to Unresponsive
      NOT_GCS_AVPU

    )

  # Initial population only

  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # get the summary of results
  filter_counts <- tibble::tibble(
    filter = c("911 calls",
               "Stroke cases",
               "GCUS <= 9",
               "AVPU = Unresponsive",
               "Non-Null Stroke Scale Score or Type",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$CALL_911, na.rm = T),
      sum(computing_population$STROKE, na.rm = T),
      sum(computing_population$GCS, na.rm = T),
      sum(computing_population$AVPU, na.rm = T),
      sum(computing_population$STROKE_SCALE, na.rm = T),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # gather data into a list for multi-use output
  stroke.01.population <- list(
    filter_process = filter_counts,
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(stroke.01.population)

  } else if (
    all(
      is.null(patient_scene_table),
      is.null(vitals_table),
      is.null(situation_table),
      is.null(response_table)

    ) &&

      !is.null(df)

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

    ###_____________________________________________________________________________
  # fact table
  # the user should ensure that variables beyond those supplied for calculations
  # are distinct (i.e. one value or cell per patient)
  ###_____________________________________________________________________________

  # progress update, these will be repeated throughout the script
  cli::cli_progress_update(set = 1, id = progress_bar_population, force = T)

  final_data <- df |>
    dplyr::select(-c({{  eresponse_05_col  }},
                     {{ esituation_11_col }},
                     {{ esituation_12_col }},
                     {{ evitals_23_col }},
                     {{ evitals_26_col }},
                     {{ evitals_29_col }},
                     {{ evitals_30_col }}
    )) |>
    dplyr::distinct({{ erecord_01_col }}, .keep_all = T)

  ###_____________________________________________________________________________
  ### dimension tables
  ### each dimension table is turned into a vector of unique IDs
  ### that are then utilized on the fact table to create distinct variables
  ### that tell if the patient had the characteristic or not for final
  ### calculations of the numerator and filtering
  ###_____________________________________________________________________________

  cli::cli_progress_update(set = 2, id = progress_bar_population, force = T)

  # stroke 1
  stroke_data1 <- df |>
    dplyr::select({{ erecord_01_col }}, {{  esituation_11_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_11_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 3, id = progress_bar_population, force = T)

  # stroke 2
  stroke_data2 <- df |>
    dplyr::select({{ erecord_01_col }}, {{  esituation_12_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = stroke_pattern,
        x = {{ esituation_12_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 4, id = progress_bar_population, force = T)

  # 911 calls
  call_911_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  eresponse_05_col  }}) |>
    dplyr::filter(

      grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      )

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 5, id = progress_bar_population, force = T)

  # GCS
  GCS_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_23_col  }}) |>
    dplyr::filter(

      {{evitals_23_col}} <= 9

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 6, id = progress_bar_population, force = T)

  # AVPU
  AVPU_data <- df |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_26_col  }}) |>
    dplyr::filter(

      grepl(pattern = avpu_pattern, x = {{ evitals_26_col }}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 7, id = progress_bar_population, force = T)

  # stroke scale 1
  stroke_scale_data1 <- df |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_29_col  }}) |>
    dplyr::filter(

      !is.na({{evitals_29_col}}) & grepl(pattern = stroke_values, x = {{evitals_29_col}}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 8, id = progress_bar_population, force = T)

  # stroke scale 2
  stroke_scale_data2 <- df |>
    dplyr::select({{ erecord_01_col }}, {{  evitals_30_col  }}) |>
    dplyr::filter(

      !is.na({{evitals_30_col}}) & grepl(pattern = scale_values, x = {{evitals_30_col}}, ignore.case = T)

    ) |>
    dplyr::distinct({{ erecord_01_col }}) |>
    dplyr::pull({{ erecord_01_col }})

  cli::cli_progress_update(set = 9, id = progress_bar_population, force = T)

  # assign variables to final data
  computing_population <- final_data |>
    dplyr::mutate(STROKE1 = {{ erecord_01_col }} %in% stroke_data1,
                  STROKE2 = {{ erecord_01_col }} %in% stroke_data2,
                  STROKE = STROKE1 | STROKE2,
                  CALL_911 = {{ erecord_01_col }} %in% call_911_data,
                  GCS = {{ erecord_01_col }} %in% GCS_data,
                  AVPU = {{ erecord_01_col }} %in% AVPU_data,
                  NOT_GCS_AVPU = !GCS | !AVPU,
                  STROKE_SCALE1 = {{ erecord_01_col }} %in% stroke_scale_data1,
                  STROKE_SCALE2 = {{ erecord_01_col }} %in% stroke_scale_data2,
                  STROKE_SCALE = STROKE_SCALE1 | STROKE_SCALE2
                  )

    # get the initial population
    initial_population <- computing_population |>
    dplyr::filter(

      # Identify Records that have seizure documentation defined above
      STROKE,

      # filter down to 911 calls
      CALL_911,

      # no GCS < 9 or AVPU not equal to Unresponsive
      NOT_GCS_AVPU

    )

  # Initial population only
  cli::cli_progress_update(set = 10, id = progress_bar_population, force = T)

  # get the summary of results
  filter_counts <- tibble::tibble(
    filter = c("911 calls",
               "Stroke cases",
               "GCUS <= 9",
               "AVPU = Unresponsive",
               "Non-Null Stroke Scale Score or Type",
               "Initial population",
               "Total dataset"
    ),
    count = c(
      sum(computing_population$CALL_911, na.rm = T),
      sum(computing_population$STROKE, na.rm = T),
      sum(computing_population$GCS, na.rm = T),
      sum(computing_population$AVPU, na.rm = T),
      sum(computing_population$STROKE_SCALE, na.rm = T),
      nrow(initial_population),
      nrow(computing_population)
    )
  )

  # get the populations of interest

  cli::cli_progress_update(set = 11, id = progress_bar_population, force = T)

  # gather data into a list for multi-use output
  stroke.01.population <- list(
    filter_process = filter_counts,
    initial_population = initial_population,
    computing_population = computing_population
  )

  cli::cli_progress_done(id = progress_bar_population)

  return(stroke.01.population)

  }

}
