reprex::reprex({
  
################################################################################
### Test for Hypoglycemia-01 ###################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
  hypoglycemia_01 <- function(df,
                              erecord_01_col,
                              incident_date_col,
                              patient_DOB_col,
                              epatient_15_col,
                              epatient_16_col,
                              eresponse_05_col,
                              esituation_11_col,
                              esituation_12_col,
                              evitals_18_col,
                              evitals_23_cl,
                              evitals_26_col,
                              emedications_03_col,
                              eprocedures_03_col,
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
          "No object of class {.cls data.frame} was passed to {.fn hypoglycemia_01}.",
          "i" = "Please supply a {.cls data.frame} to the first argument in {.fn hypoglycemia_01}."
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
    
    # get codes as a regex to filter primary/secondary impression fields
    hypoglycemia_treatment_codes <- "4832|4850|377980|376937|372326|237653|260258|309778|1795610|1795477|1794567|1165823|1165822|1165819"
    
    # hypoglycemia procedures
    
    hypoglycemia_procedure_codes <- "225285007|710925007"
    
    # code(s) for altered mental status
    altered_mental_status <- "R41.82|Altered Mental Status, unspecified"
    
    # codes for diabetes via primary and secondary impression
    
    diabetes_codes <- "E13.64|E16.2|Other specified diabetes mellitus with hypoglycemia|Hypoglycemia, unspecified"
    
    # AVPU responses
    
    avpu_responses <- "Unresponsive|Verbal|Painful|3326003|3326005|3326007"
    
    # days, hours, minutes, months
    
    minor_values <- "days|hours|minutes|months"
    
    # some manipulations to prepare the table
    # create the patient age in years and the patient age in days variables for filters
    # create the unique ID variable
    initial_population_0 <- df %>%
      
      # create the age in years variable
      
      mutate(
        patient_age_in_years = as.numeric(difftime(
          time1 = {{incident_date_col}},
          time2 = {{patient_DOB_col}},
          units = "days"
        )) / 365,
        patient_age_in_days = as.numeric(difftime(
          time1 = {{incident_date_col}},
          time2 = {{patient_DOB_col}},
          units = "days"
        ))
      )
    
    # filter the table to get the initial population regardless of age
    initial_population_1 <- initial_population_0 %>%
      
      # Identify Records that have GCUS < 15, or AVPU not equal to Alert, or
      # primary/secondary impression of altered mental status
      
      mutate(
        altered = grepl(
          pattern =  altered_mental_status,
          x = {{esituation_11_col}},
          ignore.case = T
        ) |
          grepl(
            pattern =  altered_mental_status,
            x = {{esituation_12_col}},
            ignore.case = T
          ),
        AVPU = grepl(pattern = avpu_responses, x = {{evitals_26_col}}, ignore.case = T),
        GCS = {{evitals_23_cl}} < 15,
        diabetes_dx = grepl(
          pattern =  diabetes_codes,
          x = {{esituation_11_col}},
          ignore.case = T
        ) |
          grepl(
            pattern =  diabetes_codes,
            x = {{esituation_12_col}},
            ignore.case = T
          ),
        blood_glucose_flag = {{evitals_18_col}} < 60,
        call_911 = grepl(
          pattern = codes_911,
          x = {{eresponse_05_col}},
          ignore.case = T
        )
        
      ) %>% 
      
      dplyr::filter(
        # diabetic patients 
        # patients with a GCS < 15, or AVPU < Alert,
        # or altered mental status, and blood glucose less than 60
        
        (diabetes_dx & (GCS | AVPU)) | 
          (altered & blood_glucose_flag),
        call_911
        
      ) %>%
      
      # create variable that documents if any of target treatments were used
      mutate(correct_treatment = if_else(
        grepl(
          pattern = hypoglycemia_treatment_codes,
          x = {{emedications_03_col}},
          ignore.case = TRUE
        ) |
          grepl(
            pattern = hypoglycemia_procedure_codes,
            x = {{eprocedures_03_col}},
            ignore.case = T
          ),
        1,
        0
      ))
    
    # final pass with manipulations to get a tidy table
    # 1 row per observation, 1 column per feature
    
    initial_population <- initial_population_1 %>%
      mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>% 
      distinct(Unique_ID, .keep_all = T)
    
    # Adult and Pediatric Populations
    
    # filter adult
    adult_pop <- initial_population %>%
      dplyr::filter(patient_age_in_years >= 18 | 
                      ({{epatient_15_col}} >= 18 & {{epatient_16_col}} == "Years")
                    
      )
    
    # filter peds
    peds_pop <- initial_population %>%
      dplyr::filter(patient_age_in_years < 18 | 
                      ({{epatient_15_col}} < 18 & {{epatient_16_col}} == "Years") | 
                      !is.na({{epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col}}, ignore.case = T)
      ) %>% 
      filter(patient_age_in_days >= 1 | 
               !({{epatient_15_col}} < 1 & {{epatient_16_col}} == "Days") & 
               !({{epatient_15_col}} < 24 & {{epatient_16_col}} == "Hours") & 
               !({{epatient_15_col}} < 120 & {{epatient_16_col}} == "Minutes")
             
      )
    
    # get the summary of results
    
    # all
    total_population <- initial_population %>% 
      filter(!({{epatient_15_col}} < 1 & {{epatient_16_col}} == "Days") & 
               !({{epatient_15_col}} < 24 & {{epatient_16_col}} == "Hours") & 
               !({{epatient_15_col}} < 120 & {{epatient_16_col}} == "Minutes")
      ) %>% 
      summarize(
        measure = "Hypoglycemia-01",
        pop = "All",
        numerator = sum(correct_treatment, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # adults
    adult_population <- adult_pop %>%
      summarize(
        measure = "Hypoglycemia-01",
        pop = "Adults",
        numerator = sum(correct_treatment, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # peds
    peds_population <- peds_pop %>%
      summarize(
        measure = "Hypoglycemia-01",
        pop = "Peds",
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
  
  
# load data

hypoglycemia_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/hypoglycemia01_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean data

hypoglycemia_01_clean <- hypoglycemia_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~  mdy(str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+\\s(AM|PM))?"))
                )
         )

# run the function

hypoglycemia_01_clean %>% 
  hypoglycemia_01(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                  incident_date_col = INCIDENT_DATE,
                  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                  evitals_18_col = PATIENT_LOW_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                  evitals_23_cl = PATIENT_LOW_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                  evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                  emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
                  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03
                  )

}, venue = "gh", advertise = TRUE)
