################################################################################
### Safety-01 Function #########################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### Need to be a table where each row is 1 observation and each column is a feature
### or distinct datasets that can be referenced as unique columns
### this function will calculate an age in years
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the function assumes that the eresponse.05 column has the codes in it, text
### can be present, too, for reference
### the function assumes that erepsonse.24 is a list column or a column that has all
### text descriptors for additional response mode descriptors.  These can be separated
### by commas or other characters as long as all eresponse.24 values are present
### in one cell for each unique erecord.01 value.  Codes can be present
### but will be ignored by the function.
### the first argument is a dataframe, no joining is done.
### any joins to get vitals etc. will need to be done outside the function
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________


safety_01 <- function(df,
                      incident_date_col,
                      patient_DOB_col,
                      eresponse_05_col,
                      eresponse_24_col,
                      ...) {
  # Load necessary packages
  for (pkg in c("tidyverse", "scales", "rlang")) {
    if (!pkg %in% installed.packages())
      install.packages(pkg, quiet = TRUE)
    if (!paste0("package:", pkg) %in% search())
      library(pkg, quietly = TRUE)
  }
  
  # provide better error messaging if df is missing
  if (missing(df)) {
    cli_abort(
      c(
        "No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
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
  
  if (!exists("pretty_percent")) {
    pretty_percent <- function(variable, n_decimal = 0.1) {
      formatted_percent <- percent(variable, accuracy = n_decimal)
      
      # If there are trailing zeros after decimal point, remove them
      formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
      
      # If it ends with ".%", replace it with "%"
      formatted_percent <- sub("\\.%$", "%", formatted_percent)
      
      formatted_percent
      
    }
    
  }
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to find lights and siren responses
  lights_and_sirens <- "Initial Lights and Sirens, Downgraded to No Lights or Sirens|Initial No Lights or Sirens, Upgraded to Lights and Sirens|Lights and Sirens"
  
  # filter the table to get the initial population regardless of age
  initial_population <- df %>%
    
    # create the age in years variable
    
    mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365) %>%
    
    # filter down to 911 calls
    
    dplyr::filter(grepl(
      pattern = codes_911,
      x = {{eresponse_05_col}},
      ignore.case = T
    )) %>%
    
    # if lights and sirens ARE NOT present, 1, else 0
    mutate(l_s_check = if_else(!grepl(pattern = lights_and_sirens, x = {{eresponse_24_col}}), 1, 0))
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population %>%
    dplyr::filter(patient_age_in_years_col >= 18)
  
  # filter peds
  peds_pop <- initial_population %>%
    dplyr::filter(patient_age_in_years_col < 18)
  
  # get the summary of results
  
  # all
  total_population <- initial_population %>%
    summarize(
      measure = "Safety-01",
      pop = "All",
      numerator = sum(l_s_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # adults
  adult_population <- adult_pop %>%
    summarize(
      measure = "Safety-01",
      pop = "Adults",
      numerator = sum(l_s_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # peds
  peds_population <- peds_pop %>%
    summarize(
      measure = "Safety-01",
      pop = "Peds",
      numerator = sum(l_s_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  safety.01 <- bind_rows(adult_population, peds_population, total_population)
  
  safety.01
  
  
}
