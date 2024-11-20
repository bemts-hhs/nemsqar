airway_01 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      earrest_01_col,
                      evitals_01_col,
                      evitals_06_col,
                      evitals_12_col,
                      ,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      emedications_03_col,
                      ...) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn asthma_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn asthma_01}."
      )
    )
    
  }
  
  # Ensure df is a data frame or tibble
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    cli::cli_abort(
      c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
      )
    )
  }
  
  # use quasiquotation on the date variables to check format
  incident_date <- rlang::enquo(incident_date_col)
  patient_DOB <- rlang::enquo(patient_DOB_col)
  
  if ((!lubridate::is.Date(df[[rlang::as_name(incident_date)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(incident_date)]])) ||
      (!lubridate::is.Date(df[[rlang::as_name(patient_DOB)]]) &
       !lubridate::is.POSIXct(df[[rlang::as_name(patient_DOB)]]))) {
    
    cli::cli_abort(
      "For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class."
    )
    
  }
  
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary/secondary impression fields
  beta_agonist <- "albuterol|ipratropium|levalbuterol|metaproterenol"
  
  # codes for asthma or acute bronchospasm
  asthma_codes <- "\\b(?:J45|J98.01)\\b"
  
  # filter the table to get the initial population ages >= 2 years
  initial_population <- df |>
    
    # create the age in years variable
    
    dplyr::mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{  incident_date_col  }},
      time2 = {{ patient_DOB_col }},
      units = "days"
    )) / 365) |>
    
    # filter down to 911 calls
    
    dplyr::filter(grepl(
      pattern = codes_911,
      x = {{ eresponse_05_col }},
      ignore.case = T
    ),
    
    # Identify Records that have specified asthma
    
    dplyr::if_any(c({{ esituation_11_col}}, {{esituation_12_col }}), ~ grepl(
      pattern = asthma_codes,
      x = .,
      ignore.case = T
    ))) |>
    
    # check to ensure beta agonist was used
    dplyr::mutate(beta_agonist_check = dplyr::if_else(grepl(
      pattern = beta_agonist,
      x = {{ emedications_03_col }},
      ignore.case = TRUE
    ), 1, 0))
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(patient_age_in_years_col >= 18)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(patient_age_in_years_col < 18, patient_age_in_years_col >= 2)
  
  # get the summary of results
  # summary
  asthma.01 <- results_summarize(total_population = initial_population,
                                 adult_population = adult_pop,
                                 peds_population = peds_pop,
                                 measure_name = "Asthma-01",
                                 numerator_col = beta_agonist_check,
                                 ...)
  
  
  asthma.01
  
  
}
