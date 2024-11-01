################################################################################
### Respiratory-02 Function  ###################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### The data must be a dataframe or tibble that contains emedications.03 and eprocedures.03
### as columns where each cell contains all values entered for each respective incident.
### These are not list columns but can be comma separated values in each cell, and must contain
### all medications for each incident.  
### The table must also have the erecord.01 values to attempt a best estimate of a unique ID by concatenating
### the erecord.01, incident date, and patient DOB.
### The table can include all values entered for evitals.12
### in the table.  This will cause row explosion, but the function will find if the patient had
### sp02 < 90 among any values entered.
### This function assumes you have the following fields:
### eRecord.01, eresponse.05 (must contain code), evitals.12 (all values), emedications.03 (all with code),
### eprocedures.03 (with code)
### this function assumes that there is an age in years calculated for epatient.15
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the first argument is a dataframe, no joining is done in the function.
### any needed table joins must be done prior to running the function to ensure
### required columns are present.
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________

respiratory_02 <- function(df,
                           erecord_01_col,
                           incident_date_col,
                           patient_DOB_col,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col) {
  
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
  
  
# some manipulations to prepare the table
# create the patient age in years and the patient age in days variables for filters
# create the unique ID variable
initial_population <- df %>% 
  mutate(patient_age_in_years = as.numeric(
    difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days")) / 365,
    patient_age_in_days = as.numeric(
      difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days"))
    ) %>% 
  rowwise() %>% # use rowwise() as we do not have a reliable grouping variable yet if the table is not distinct
  mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>% 
  ungroup()

# finish filters and make the table distinct
initial_population <- initial_population %>% 
  filter(
    grepl(
      pattern = "2205001|2205003|2205009",
      x = {{eresponse_05_col}},
      ignore.case = T
    ),
    {{evitals_12_col}} < 90
  ) %>%
  mutate(
    {{evitals_12_col}} := str_c({{evitals_12_col}}, collapse = ", "), # take all vitals for each pt and roll them into one cell per pt
    .by = Unique_ID
  ) %>% 
  distinct(Unique_ID, .keep_all = T) # this will ensure each row is an observation, and each column is a feature

# get population 1 for respiratory-02, peds
respiratory_02_peds <- initial_population %>% 
  filter(patient_age_in_years < 18,
         patient_age_in_days >= 24 
         )

# get population 2 for respiratory-02, adults
respiratory_02_adults <- initial_population %>% 
  filter(patient_age_in_years >= 18)

# calculations for peds
peds_calculation <- respiratory_02_peds %>% 
  summarize(pop = "Peds",
            numerator = sum(
              grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# calculations for adults

adults_calculation <- respiratory_02_adults %>% 
  summarize(pop = "Adults",
            numerator = sum(
              grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# overall calculation

total_population <- initial_population %>% 
  summarize(pop = "All",
            numerator = sum(
              grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
            ),
            denominator = n(),
            prop = numerator / denominator,
            prop_label = pretty_percent(prop, n_decimal = 0.01)
            )

# bind rows of calculations for final table

resp_02 <- bind_rows(total_population, peds_calculation, adults_calculation)

resp_02
  
}