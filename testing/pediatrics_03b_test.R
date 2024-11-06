reprex::reprex({

################################################################################
### Test for Pediatrics-03b ####################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# function
  
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
  non_weight_based_meds <- "Blow-by|Bag Valve Mask (BVM)|Non-Rebreather Mask|Nasal Cannula|Inhalation|Re-breather mask|Ventimask|Topical|OxyMask|CPAP"
  
  # filter the table to get the initial population regardless of age, only 911 responses
  initial_population_0 <- df %>%
    dplyr::filter(grepl(
      pattern = codes_911,
      x = {{eresponse_05_col}},
      ignore.case = T
    ))
  
  # create the age in years variable
  initial_population_1 <- initial_population_0 %>%
    mutate(
      patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365,
      
      # check if weight was documented
      documented_weight = if_else(!is.na({{eexam_01_col}}) |
                                    !is.na({{eexam_02_col}}), 1, 0),
      # confirm meds passed
      meds_given = if_else(!is.na(emedications_03_col), TRUE, FALSE),
      
      # check to see if non-weight-based meds
      non_weight_based = grepl(
        pattern = non_weight_based_meds,
        x = {{emedications_04_col}},
        ignore.case = TRUE
      )
    ) %>%
    
    # filter down to 911 calls where weight-based meds were passed
    filter(non_weight_based == FALSE)
  
  # second filtering process, make the table distinct by rolling up emedications.04
  # based on a unique identifier
  initial_population <- initial_population_0 %>%
    rowwise() %>% # use rowwise() as we do not have a reliable grouping variable yet if the table is not distinct
    mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>%
    ungroup() %>%
    mutate({{emedications_04_col}} := str_c({{emedications_04_col}}, collapse = ", ")) %>%
    distinct(Unique_ID, .keep_all = T)
  
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
      measure = "Pediatrics-03b",
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
      measure = "Pediatrics-03b",
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
      measure = "Pediatrics-03b",
      pop = "Peds",
      numerator = sum(l_s_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  pediatrics.03b <- bind_rows(adult_population, peds_population, total_population)
  
  pediatrics.03b
  
  
}
  
# load data

pediatrics_03b_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/pediatrics03b_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean

pediatrics_03b_clean <- pediatrics_03b_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
  )))

# run function

pediatrics_03b_data %>% 
  pediatrics_03b(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
            incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            DISPOSITION_ADDITIONAL_TRANSPORT_MODE_DESCRIPTOR_LIST_E_DISPOSITION_18,
            transport_disposition_cols = c(DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112, 
                                           TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30)
            )

}, venue = "gh", advertise = TRUE)
