#' Title
#'
#' @param df 
#' @param erecord_01_col 
#' @param incident_date_col 
#' @param patient_DOB_col 
#' @param epatient_15_col 
#' @param epatient_16_col 
#' @param eresponse_05_col 
#' @param esituation_11_col 
#' @param esituation_12_col 
#' @param evitals_23_col 
#' @param evitals_26_col 
#' @param transport_disposition_cols 
#' @param evitals_12_col 
#' @param evitals_16_col 
#' @param evitals_06_col 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
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
                   ) 
{
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli::cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn tbi_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn tbi_01}."
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
  
  # avpu not values
  avpu_values <- "3326003|3326005|3326007|Verbal|Painful|Unresponsive"
  
  # TBI injuries
  tbi_injuries <- "S02|S04.4|S06|S06.X9|S06.0|S07.1|S09.90|T74.4"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
  # minor values
  
  minor_values <- "days|hours|minutes|months"
  
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
      
      # TBI variable
      TBI = if_else(if_any(c({{ esituation_11_col }}, {{ esituation_12_col }}), ~ grepl(
        pattern = tbi_injuries,
        x = .,
        ignore.case = T
      )), TRUE, FALSE), 
      
      # GCS check
      GCS_check = {{ evitals_23_col }} < 15,
      
      # AVPU check
      AVPU_check = grepl(pattern = avpu_values, x = {{ evitals_26_col }}, ignore.case = T),
      
      # AVPU or GCS check,
      AVPU_or_GCS = (GCS_check | AVPU_check),
      
      # system age check
      system_age_adult = {{ epatient_15_col }} >= 18 & {{ epatient_16_col }} == "Years",
      system_age_minor1 = {{ epatient_15_col }} < 18 & {{ epatient_16_col }} == "Years",
      system_age_minor2 = {{ epatient_15_col }} <= 120 & grepl(pattern = minor_values, x = {{ epatient_16_col }}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2,
      
      # calculated age check
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18
    )
  
  # get the initial population
  initial_population <- initial_population_0 |>
    dplyr::filter(
      
      # filter down GCS < 15 or AVPU < Alert
      AVPU_or_GCS & 
      
      # TBI Dx only
      TBI & 
      
      # 911 calls only
      # NEMSIS 3.4/3.5 transports only
      (call_911 & transport)
      
    ) |> 
    
    dplyr::select(-c({{ evitals_23_col }}, {{ evitals_26_col }}, {{ transport_disposition_cols }})) |> 
    mutate(Unique_ID = str_c({{ erecord_01_col }}, {{ incident_date_col }}, {{ patient_DOB_col }}, sep = "-")) |> 
    mutate(
           across(c({{ evitals_12_col }}, {{ evitals_16_col }}, {{ evitals_06_col }}), ~ str_c(., collapse = ", ")),
           .by = Unique_ID) |> 
    distinct({{ erecord_01_col }}, .keep_all = T) |> 
    mutate(
           vitals_check = if_all(c({{ evitals_12_col }}, {{ evitals_16_col }}, {{ evitals_06_col }}), ~ !is.na(.)),
      )

  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population |>
    dplyr::filter(system_age_adult | calc_age_adult)
  
  # filter peds
  peds_pop <- initial_population |>
    dplyr::filter(system_age_minor | calc_age_minor)
  
  # get the summary of results
  
  # adults
  adult_population <- adult_pop |>
    summarize_measure(measure_name = "TBI-01",
                      population_name = "Adults",
                      vitals_check,
                      ...)
  
  # peds
  peds_population <- peds_pop |>
    summarize_measure(measure_name = "TBI-01",
                      population_name = "Peds",
                      vitals_check,
                      ...)
  
  # summary
  tbi.01 <- dplyr::bind_rows(adult_population, peds_population)
  
  tbi.01
  
}
