reprex::reprex({

################################################################################
### Respiratory-01 Test  #######################################################
################################################################################

# packages
library(tidyverse)
library(janitor)
library(rlang)
library(scales)
  
# data
  
  respiratory_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory01_Export_2023.csv") %>% 
    clean_names(case = "screaming_snake", sep_out = "_")

# clean

  respiratory_01_clean <- respiratory_01_data %>%
    mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
      str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
    )))
  
# the function will load necessary packages in the background
  
  respiratory_01 <- function(df,
                             erecord_01_col,
                             incident_date_col,
                             patient_DOB_col,
                             epatient_15_col,
                             epatient_16_col,
                             eresponse_05_col,
                             esituation_11_col,
                             esituation_12_col,
                             evitals_12_col,
                             evitals_14_col,
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
    
    # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
    
    # 911 codes for eresponse.05
    codes_911 <- "2205001|2205003|2205009"
    
    # get codes as a regex to filter primary impression fields
    resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
    
    # minor values
    minor_values <- "days|hours|minutes|months"
    
    # filter the table to get the initial population regardless of age
    initial_population <- df %>%
      
      # create the age in years variable
      
      mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365,
      
      # create the respiratory distress variable
      respiratory_distress = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
        pattern = resp_codes,
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
      system_age_minor = ({{epatient_15_col}} < 18 & {{epatient_16_col}} == "Years") | 
        (!is.na({{epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col}}, ignore.case = T)),
      # calculated age checks
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18
      ) %>%
      
      dplyr::filter(
        
        # Identify Records that have Respiratory Distress Codes defined above
        respiratory_distress,
        
        # filter down to 911 calls
        call_911
        
      ) %>%
      
      # check to see if target vitals were captured
      mutate(vitals_check = if_else(!is.na({{evitals_12_col}}) &
                                      !is.na({{evitals_14_col}}), 1, 0)) %>%
      mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>%
      distinct(Unique_ID, .keep_all = T)
    
    # Adult and Pediatric Populations
    
    # filter adult
    adult_pop <- initial_population %>%
      dplyr::filter(system_age_adult | calc_age_adult)
    
    # filter peds
    peds_pop <- initial_population %>%
      dplyr::filter(system_age_minor | calc_age_minor)
    
    # get the summary of results
    
    # all
    total_population <- initial_population %>%
      summarize(
        measure = "Respiratory-01",
        pop = "All",
        numerator = sum(vitals_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # adults
    adult_population <- adult_pop %>%
      summarize(
        measure = "Respiratory-01",
        pop = "Adults",
        numerator = sum(vitals_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # peds
    peds_population <- peds_pop %>%
      summarize(
        measure = "Respiratory-01",
        pop = "Peds",
        numerator = sum(vitals_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # summary
    resp_01 <- bind_rows(adult_population, peds_population, total_population)
    
    resp_01
    
    
  }
  
  
  # test the function
  
  respiratory_01_clean %>% 
    respiratory_01(
      erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
      incident_date_col = INCIDENT_DATE,
      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
      esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
      esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
      evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
      evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14
    )
  
  
}, venue = "gh", advertise = T)





