#' @title Stroke-01:
#' 
#' The `stroke_01` function processes EMS dataset to identify potential stroke cases based on specific criteria
#' and calculates the stroke scale measures. It filters the data for 911 response calls, identifies stroke-related
#' impressions and scales, and aggregates results by unique patient encounters.
#' 
#' @section Data Assumptions:
#' 
#' This function assumes that:
#' 
#' Data are already loaded into a data frame or tibble where each row represents
#' one observation (e.g., patient) and each column is a distinct feature (field).
#' Alternatively, data may consist of separate datasets referenced by unique columns.
#'  
#' Missing values in rows are represented as `NA`. "Not known" or "not recorded" values,
#' common to ImageTrend or other non-numeric codes, should be pre-cleaned before use.
#' 
#' The `eresponse_05_col` contains NEMSIS response codes and may also include
#' descriptive text for reference.
#' 
#' The vital signs columns `evitals.23`, `evitals.26`, `evitals.29`, and `evitals.30` can contain
#' all the unique values entered per incident, which may cause row explosion.  The function will
#' handle this issue elegantly under the hood.
#'  
#' The `esituation.11` contains single responses per response, but `esituation.12` should be a list 
#' column or text field with all relevant secondary provider impression entries provided in a single cell 
#' as comma-separated values for each unique incident.
#' 
#' The first argument to this function is the main data frame. No joins are performed within
#' the function; any necessary joins (e.g., to incorporate vitals or additional fields) should
#' be completed prior to calling this function.
#' 
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#' 
#' #' @section Practical Tips:
#' 
#' Ensure data are pre-processed, with missing values coded as `NA`, before passing
#' into the function.
#' Prepare necessary joins (e.g., for vitals) in advance; this function does not perform joins.
#' 
#' @section Value:
#' 
#' A summarized tibble with counts and proportions of patients of all ages
#' who had a 911 response, were suffering from stroke, and had a stroke assessment completed.
#' 
#' @param df <['tidy-select'][dplyr_tidy_select]> A data frame or tibble containing the dataset. Each row should represent a unique patient encounter.
#' 
#' @param erecord_01_col <['tidy-select'][dplyr_tidy_select]> The column containing unique record identifiers for each encounter.
#' 
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> The column containing the date and time of the incident. This must be a `Date` or `POSIXct` type.
#' 
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> The column containing the patient's date of birth, formatted as `Date` or `POSIXct`.
#' 
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> The column containing EMS response codes, which should include 911 response codes.
#' 
#' @param esituation_11_col <['tidy-select'][dplyr_tidy_select]> The column containing the primary impression codes or descriptions related to the situation.
#' 
#' @param esituation_12_col <['tidy-select'][dplyr_tidy_select]> The column containing secondary impression codes or descriptions related to the situation.
#' 
#' @param evitals_23_col <['tidy-select'][dplyr_tidy_select]> The column containing the Glasgow Coma Scale (GCS) score.
#' 
#' @param evitals_26_col <['tidy-select'][dplyr_tidy_select]> The column containing the AVPU (alert, verbal, pain, unresponsive) scale value.
#' 
#' @param evitals_29_col <['tidy-select'][dplyr_tidy_select]> The column containing the stroke scale score achieved during assessment.
#' 
#' @param evitals_30_col <['tidy-select'][dplyr_tidy_select]> The column containing stroke scale type descriptors (e.g., FAST, NIH, etc.).
#' 
#' @param ... Additional arguments passed to `dplyr::summarize()` function for further customization of results.
#' 
#' @section Features:
#' * **Stroke Case Identification**: Identifies potential stroke cases based on primary and secondary impression codes.
#' * **911 Response Filtering**: Filters records to include only those flagged with 911 response codes in `eresponse_05_col`.
#' * **Glasgow Coma Scale and AVPU Processing**: Analyzes and includes Glasgow Coma Scale (GCS) scores and AVPU levels from vital signs.
#' * **Stroke Scale Type and Values Assessment**: Extracts and processes stroke scale information (e.g., FAST, NIH) from the specified columns.
#' * **Encounter Aggregation**: Aggregates data by unique patient encounters to provide a summary view of stroke case proportions.
#'         
#' @return A tibble summarizing results for the total population with the following columns:
#' `pop`: Population type (All).
#' `numerator`: Count of incidents where beta-agonist medications were administered.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents involving beta-agonist medications.
#' `prop_label`: Proportion formatted as a percentage with a specified number of
#' decimal places.
#' 
#' @note
#' This function filters and processes EMS data to:
#' - Identify stroke cases based on primary and secondary impression codes.
#' - Filter for 911 response codes and relevant vital signs (e.g., GCS, AVPU).
#' - Aggregate results by patient encounter and calculate stroke scale outcomes.
#' - Return a summary of stroke cases by unique patient identifier, including stroke scale measurements.
#' 
#' @export
#' 
stroke_01 <- function(df,
                      erecord_01_col,
                       incident_date_col,
                       patient_DOB_col,
                       eresponse_05_col,
                       esituation_11_col,
                       esituation_12_col,
                       evitals_23_col,
                       evitals_26_col,
                       evitals_29_col,
                       evitals_30_col,
                       ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn stroke_02}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn stroke_02}."
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
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # primary and secondary provider impression values
  stroke_pattern <- "I6[013]|G4[56]"
  
  # AVPU exclusion
  avpu_pattern <- "3326007|Unresponsive"
  
  # stroke score not values
  
  stroke_values <- "positive|negative|conclusive"
  
  # scale_values
  
  scale_values <- "F\\.A\\.S\\.T\\. Exam|Miami Emergency Neurologic Deficit \\(MEND\\)|Cincinnati|Other Stroke Scale Type|NIH|Los Angeles|RACE \\(Rapid Arterial Occlusion Evaluation\\)|Los Angeles Motor Score \\(LAMS\\)|Massachusetts"
  
  # filter the table to get the initial population regardless of age
  initial_population_0 <- df |>
    
    # create the age in years variable
    
    mutate(
      
    # create the respiratory distress variable
    stroke = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
      pattern = stroke_pattern,
      x = .,
      ignore.case = T
    )),
    
    # create the 911 variable
    call_911 = grepl(
      pattern = codes_911,
      x = {{eresponse_05_col}},
      ignore.case = T
    ),
    
    # GCS > 9
    gcs_greater_9 = {{evitals_23_col}} <= 9,
    
    # AVPU not equal to Unresponsive
    avpu_not_unresponsive = grepl(pattern = avpu_pattern, x = {{evitals_26_col}}, ignore.case = T)
    ) |>
    
    dplyr::filter(
      
      # Identify Records that have seizure documentation defined above
      stroke,
      
      # filter down to 911 calls
      call_911,
      
     # no GCS < 9 or AVPU not equal to Unresponsive 
     !gcs_greater_9 | !avpu_not_unresponsive
      
    )
  
  # continue manipulations with a separate mutate() process
  
  initial_population <- initial_population_0 |> 
    mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) |> 
    
    # tidy stroke scale data 
    mutate({{evitals_29_col}} := str_c({{evitals_29_col}}, collapse = ", "),
           {{evitals_30_col}} := str_c({{evitals_30_col}}, collapse = ", "),
           .by = Unique_ID
           ) |> 
    
    # remove columns that introduce row explosion
    select(-c({{evitals_23_col}}, {{evitals_26_col}})) |> 
    distinct(Unique_ID, .keep_all = T) |> 
    
    # create the numerator variable for stroke scales
    mutate(stroke_scale1 = !is.na({{evitals_29_col}}) & grepl(pattern = stroke_values, x = {{evitals_29_col}}, ignore.case = T),
           stroke_scale2 = !is.na({{evitals_30_col}}) & grepl(pattern = scale_values, x = {{evitals_30_col}}, ignore.case = T),
           stroke_scale = if_else(stroke_scale1 | stroke_scale2, 1, 0)
           )
  
  # Initial population only
  
  # get the summary of results
  
  # all
  total_population <- initial_population |>
    summarize(
      measure = "Stroke-02",
      pop = "All",
      numerator = sum(stroke_scale, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  stroke.01 <- total_population
  
  stroke.01
  
}
