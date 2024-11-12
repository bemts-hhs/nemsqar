

stroke_02 <- function(df,
                      erecord_01_col,
                       incident_date_col,
                       patient_DOB_col,
                       epatient_15_col,
                       epatient_16_col,
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

  # GCS exclusion
  
  gcs_exclude <- "\\b[0-9]\\b"
  
  # filter the table to get the initial population regardless of age
  initial_population <- df |>
    
    # create the age in years variable
    
    mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365,
    
    # create the respiratory distress variable
    seizure = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
      pattern = epilepsy_pattern,
      x = .,
      ignore.case = T
    )),
    
    # create the 911 variable
    call_911 = grepl(
      pattern = codes_911,
      x = {{eresponse_05_col}},
      ignore.case = T
    ),
    
    # system age checks
    system_age_adult = {{epatient_15_col}} >= 18 & {{epatient_16_col}} == "Years",
    system_age_minor1 = {{epatient_15_col}} < 18 & {{epatient_15_col}} >= 2 & {{epatient_16_col}} == "Years",
    system_age_minor2 = {{epatient_15_col}} >= 24 & {{epatient_16_col}} == "Months",
    system_age_minor = system_age_minor1 | system_age_minor2,
    
    # calculated age checks
    calc_age_adult = patient_age_in_years_col >= 18,
    calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    ) |>
    
    dplyr::filter(
      
      # Identify Records that have seizure documentation defined above
      seizure,
      
      # filter down to 911 calls
      call_911
      
    ) |>
    
    # check to see if target meds were passed
    mutate(meds_check = if_else(grepl(pattern = medication_pattern, x = {{emedications_03_col}}, ignore.case = T), 1, 0)
    )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # get the summary of results
  
  # all
  total_population <- initial_population |>
    summarize(
      measure = "Seizure-02",
      pop = "All",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # adults
  adult_population <- adult_pop |>
    summarize(
      measure = "Seizure-02",
      pop = "Adults",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # peds
  peds_population <- peds_pop |>
    summarize(
      measure = "Seizure-02",
      pop = "Peds",
      numerator = sum(meds_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  seizure.02 <- bind_rows(adult_population, peds_population, total_population)
  
  seizure.02
  
  
}
