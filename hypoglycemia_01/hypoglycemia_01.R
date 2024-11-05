################################################################################
### Hypoglycemia-01 ##################################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### Assume data provided is a dataframe / tibble
### this function will calculate an age in years
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the function assumes that the primary/secondary impression fields have the
### ICD-10 code in them.  The text description can be present, too, for reference.
### the function assumes that the eresponse.05 column has the codes in it, text
### can be present, too, for reference
### the function assumes that emedications_03 contains all medications administered,
### and that it contains the text description of the medication.  The code can be
### included for reference, but will not be checked. All medications administered are
### in one cell per record.  This can be a list column or comma separated.
### the esituation_12 is best as a list column of the secondary impressions entered
### the first argument is a dataframe, no joining is done.
### any joins to get vitals etc. will need to be done outside the function
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________

hypoglycemia_01 <- function(df,
                      erecord_01_col,
                      incident_date_col,
                      patient_DOB_col,
                      eresponse_05_col,
                      esituation_11_col,
                      esituation_12_col,
                      evitals_18_col,
                      evitals_23_cl,
                      evitals_26_col,
                      emedications_03_col,
                      ...) {
  
  # Load necessary packages
  for (pkg in c("tidyverse", "scales", "rlang")) {
    if (!pkg %in% installed.packages()) install.packages(pkg, quiet = TRUE)
    if (!paste0("package:", pkg) %in% search()) library(pkg, quietly = TRUE)
  }
  
  # provide better error messaging if df is missing
  if(missing(df)) {
    
    cli_abort(c("No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
                "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
                ))
    
  }
  
  # Ensure df is a data frame or tibble
  if (!is.data.frame(df) && !is_tibble(df)) {
    cli_abort(c(
      "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
      "i" = "The passed object is of class {.val {class(df)}}."
    ))
  }
  
  # use quasiquotation on the date variables to check format
  incident_date <- enquo(incident_date_col)
  patient_DOB <- enquo(patient_DOB_col)
  
  if(!is.Date(df[[as_name(incident_date)]]) & !is.POSIXct(df[[as_name(incident_date)]]) & !is.Date(df[[as_name(patient_DOB)]]) & !is.POSIXct(df[[as_name(patient_DOB)]])) {
    
    cli_abort("For the variables {.var incident_date_col} and {.var patient_DOB_col}, one or both of these variables were not of class {.cls Date} or a similar class.  Please format your {.var incident_date_col} and {.var patient_DOB_col} to class {.cls Date} or similar class.")
    
  }
  
  if(!exists("pretty_percent")) {
    
    pretty_percent <- function(variable, n_decimal = 0.1) {
      
      formatted_percent <- percent(variable, accuracy = n_decimal)
      
      # If there are trailing zeros after decimal point, remove them
      formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
      
      # If it ends with ".%", replace it with "%"
      formatted_percent <- sub("\\.%$", "%", formatted_percent)
      
      formatted_percent
      
    }
    
  }
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary/secondary impression fields
  hypoglycemia_treatment_codes <- "4850|237653|309778|260258|237648|807169|4832"
  
  # code(s) for altered mental status
  altered_mental_status <- "R41.82"
  
  # some manipulations to prepare the table
  # create the patient age in years and the patient age in days variables for filters
  # create the unique ID variable
  initial_population_0 <- df %>% 
    
    # create the age in years variable
    
    mutate(patient_age_in_years = as.numeric(
      difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days")) / 365,
      patient_age_in_days = as.numeric(
        difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days"))
    )
  
  # filter the table to get the initial population regardless of age
  initial_population_1 <- initial_population_0 %>% 
    
    # filter down to 911 calls
    
    dplyr::filter(grepl(pattern = codes_911, x = {{eresponse_05_col}}, ignore.case = T)) %>% 
    
    # Identify Records that have GCUS < 15, or AVPU not equal to Alert, or
    # primary/secondary impression of altered mental status
    
    mutate(altered = grepl(pattern =  altered_mental_status, x = {{esituation_11_col}}, ignore.case = T) | 
                    grepl(pattern =  altered_mental_status, x = {{esituation_12_col}}, ignore.case = T),
           AVPU = {{evitals_26_col}} %in% c("Unresponsive", "Verbal", "Painful"),
           GCS = {{evitals_23_cl}} < 15
                  ) %>%
    
    # filter down to the target population with any of the three logical conditions, and 
    # blood glucose < 60
    
    filter(altered == TRUE | AVPU == TRUE | GCS == TRUE,
           {{evitals_18_col}} < 60
           ) %>% 
    
    # create variable that documents if any of target treatments were used
    mutate(correct_treatment = if_else(grepl(pattern = hypoglycemia_treatment_codes, x = {{emedications_03_col}}, ignore.case = TRUE), 1, 0
                                  )
           )
  
  # final pass with manipulations to get a tidy table
  # 1 row per observation, 1 column per feature
  
  initial_population <- initial_population_1 %>% 
    rowwise() %>% # use rowwise() as we do not have a reliable grouping variable yet if the table is not distinct
    mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>% 
    ungroup() %>%
    mutate({{evitals_18_col}} := str_c({{evitals_18_col}}, collapse = ", "), 
           {{evitals_23_cl}} := str_c({{evitals_23_cl}}, collapse = ", "), 
           {{evitals_26_col}} := str_c({{evitals_26_col}}, collapse = ", "),
           .by = Unique_ID
           ) %>% 
    distinct(Unique_ID, .keep_all = T)
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population %>% 
    dplyr::filter(patient_age_in_years >= 18)
  
  # filter peds
  peds_pop <- initial_population %>% 
    dplyr::filter(patient_age_in_years < 18,
                  patient_age_in_days >= 1
                  )
  
  # get the summary of results
  
  # all
  total_population <- initial_population %>% 
    summarize(pop = "All",
              numerator = sum(correct_treatment, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
              ...
    )
  
  # adults
  adult_population <- adult_pop %>% 
    summarize(pop = "Adults",
              numerator = sum(correct_treatment, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
              ...
    )
  
  # peds
  peds_population <- peds_pop %>% 
    summarize(pop = "Peds",
              numerator = sum(correct_treatment, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
              ...
    )
  
  # summary
  hypoglycemia.01 <- bind_rows(adult_population, peds_population, total_population)
  
  hypoglycemia.01
  
  
}
