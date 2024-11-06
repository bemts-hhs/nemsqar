################################################################################
### Pediatrics-03b Function ####################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### The data must be a dataframe or tibble that contains emedications.03
### as a column where each cell contains all values entered for each respective incident.
### This is not a list column but can be comma separated values in each cell, and must contain
### all medications for each incident.
### emedications.04 is the full list of medication administration routes present in the data
### can the function will roll up these values so that the table is distinct with each row being
### 1 observation, and each column a feature.
### this function will calculate an age in years
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the function assumes that the eresponse.05 column has the codes in it, text
### can be present, too, for reference
### the function assumes that edisposition.18 is a list column or a column that has all
### text descriptors for additional transport mode descriptors.  These can be separated
### by commas or other characters as long as all eresponse.18 values are present
### in one cell for each unique erecord.01 value.  Codes can be present
### but will be ignored by the function.
### for the argument transport_disposition_cols, this argument can receive the unquoted
### column names of edisposition.12 and edisposition.30.  One or both can be entered and
### the function will evaluate them.  These columns are used to create a `transport`
### variable that is used to filter the table down furhter.  AS such, these columns
### edisposition.12 and edisposition.30 must be list columns that and/or contain all values
### from each unique incident entered for each field.  These can be comma separated values
### all in one cell to make the table tidy.
### the first argument is a dataframe, no joining is done.
### any joins to get vitals etc. will need to be done outside the function
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________

pediatrics_03b <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           eresponse_05_col,
                           eexam_01_col,
                           eexam_02_col,
                           emedications_03_col,
                           emedications_04_col,
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
  
  # non-weight-based medications
  non_weight_based_meds <- "Inhalation|Topical"
  
  # not responses for medication route
  not_values <- "Not Recorded|Not Applicable|Not Reporting"
  
  # filter the table to get the initial population regardless of age, only 911 responses
  initial_population_0 <- df %>%
    dplyr::filter(
      # only 911 calls
      grepl(
        pattern = codes_911,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),
      # only patients that had meds passed
      !is.na({{emedications_03_col}}),
      # only weight-based meds
      !grepl(
        pattern = non_weight_based_meds,
        x = {{emedications_04_col}},
        ignore.case = TRUE
      )
    )
  
  # create the age in years variable
  initial_population_1 <- initial_population_0 %>%
    mutate(patient_age_in_years_col = as.numeric(difftime(
      time1 = {{incident_date_col}},
      time2 = {{patient_DOB_col}},
      units = "days"
    )) / 365
    ) %>%
    filter(patient_age_in_years_col < 18) %>%
    
    # check if weight was documented
    mutate(documented_weight = if_else(!is.na({{eexam_01_col}}) |
                                         (
                                           !is.na({{eexam_02_col}}) &
                                             !grepl(
                                               pattern = not_values,
                                               x = {{eexam_02_col}},
                                               ignore.case = T
                                             )
                                         ), 1, 0))
  
  # second filtering process, make the table distinct by rolling up emedications.04
  # based on a unique identifier
  initial_population <- initial_population_1 %>%
    rowwise() %>% # use rowwise() as we do not have a reliable grouping variable yet if the table is not distinct
    mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>%
    ungroup() %>%
    mutate({{emedications_04_col}} := str_c({{emedications_04_col}}, collapse = ", "), .by = Unique_ID) %>%
    distinct(Unique_ID, .keep_all = T)
  
  # get the summary of results, already filtered down to the target age group for the measure
  
  # peds
  pediatrics.03b <- initial_population %>%
    summarize(
      measure = "Pediatrics-03b",
      pop = "Peds",
      numerator = sum(documented_weight, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  pediatrics.03b
  
}
