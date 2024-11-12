

syncope_01 <- function(df,
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
  syncope_pattern <- "R(55|40.4)"
  
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
  syncope.01 <- total_population
  
  syncope.01
  
}

syncope_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/syncope01_Export.csv") |> 
  clean_names(case = "screaming_snake", sep_out = "_")

stroke_01_clean <- stroke_01_data |> 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(string = ., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))
