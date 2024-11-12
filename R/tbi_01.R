tbi_01 <- function(df,
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
                   transport_disposition_cols,
                   evitals_12_col,
                   evitals_16_col,
                   evitals_06_col,
                   ...
) {
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
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
  
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # patient evaluation care
  patient_care <- "4228001|Patient Evaluated and Care Provided"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
  # get codes as a regex to find lights and siren responses
  no_lights_and_sirens <- "4218015|No Lights or Sirens"
  
  # filter the table to get the initial population regardless of age
  initial_population_0 <- df |>
    # create the age in years variable
    
    dplyr::mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{ incident_date_col }},
        time2 = {{ patient_DOB_col }},
        units = "days"
      )) / 365,
      
      # transport variable
      transport = dplyr::if_else(dplyr::if_any(
        c({{ transport_disposition_cols }}),
        ~ grepl(
          pattern = transport_responses,
          x = .,
          ignore.case = T
        )
      ), TRUE, FALSE),
      
      # 911 variable
      call_911 = grepl(
        pattern = codes_911,
        x = {{ eresponse_05_col }},
        ignore.case = T
      ),
      
      # no lights and sirens check
      no_ls_check = dplyr::if_else(grepl(pattern = no_lights_and_sirens, x = {{ edisposition_18_col }}), 1, 0),
      
      # patient evaluation care provided
      patient_care_provided = grepl(pattern = patient_care, x = {{ edisposition_28_col }}, ignore.case = T),
      
      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = ({{ epatient_15_col }} >= 2 & {{ epatient_15_col }} < 18) & {{ epatient_16_col }} == "Years",
      system_age_minor2 = {{ epatient_15_col }} >= 24 & {{ epatient_16_col }} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2,
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
    )
  
  # get the initial population
  initial_population <- initial_population_0 |>
    dplyr::filter(
      
      # filter down to 911 calls
      call_911,
      
      # patient evaluated and care provided only
      patient_care_provided,
      
      # NEMSIS 3.5 transports only
      transport
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
    summarize_measure(measure_name = "Safety-02",
                      population_name = "All",
                      no_ls_check,
                      ...)
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "Safety-02",
                      population_name = "Adults",
                      no_ls_check,
                      ...)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "Safety-02",
                      population_name = "Peds",
                      no_ls_check,
                      ...)
  
  # summary
  tbi.01 <- dplyr::bind_rows(adult_population, peds_population, total_population)
  
  tbi.01
}

  
}