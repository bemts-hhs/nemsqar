reprex::reprex({

########################################### <- <- <- <- <- <- <- <- <- %>% <- <- %>% #####################################
### Respiratory-01 Test  #######################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
  
# functions
  
  resp_01 <- function(df, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col, epatient_15_col) {
    
    # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
    
    # 911 codes for eresponse.05
    codes_911 <- "2205001|2205003|2205009"
    
    # get codes as a regex to filter primary impression fields
    resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
    
    # get codes that indicate a missing data element
    
    missing_codes <- "7701003|7701001"
    
    # filter the table to get the initial population regardless of age
    initial_population <- df %>% 
      
      # filter down to 911 calls
      
      dplyr::filter(grepl(pattern = codes_911, x = {{eresponse_05_col}}, ignore.case = T),
                    
                    # Identify Records that have Respiratory Distress Codes defined above
                    
                    if_any(
                      c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(pattern = resp_codes, x = ., ignore.case = T)
                    )
      ) %>% 
      
      # make sure that 
      mutate(vitals_check = if_else(!is.na({{evitals_12_col}}) & !is.na({{evitals_14_col}}), 1, 0
      )
      )
    
    # Adult and Pediatric Populations
    
    # filter adult
    adult_pop <- initial_population %>% 
      dplyr::filter({{epatient_15_col}} >= 18)
    
    # filter peds
    peds_pop <- initial_population %>% 
      dplyr::filter({{epatient_15_col}} < 18)
    
    # get the summary of results
    
    # all
    total_population <- initial_population %>% 
      summarize(pop = "All",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # adults
    adult_population <- adult_pop %>% 
      summarize(pop = "Adults",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # peds
    peds_population <- peds_pop %>% 
      summarize(pop = "Peds",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # summary
    respiratory_01 <- bind_rows(adult_population, peds_population, total_population)
    
    respiratory_01
    
    
  }
  
  pretty_percent <- function(variable, n_decimal = 0.1) {
    
    formatted_percent <- percent(variable, accuracy = n_decimal)
    
    # If there are trailing zeros after decimal point, remove them
    formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
    
    # If it ends with ".%", replace it with "%"
    formatted_percent <- sub("\\.%$", "%", formatted_percent)
    
    formatted_percent
    
  }
  
# load data

resp_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory01_Export.csv")

# clean up names

resp_01_clean <- resp_01_data %>% 
  clean_names(case = "screaming_snake", sep_out = "_") %>% 
  mutate(region = sample(c("1A", "1C", "2", "3", "4", "5", "6", "7"), size = nrow(resp_01_data), replace = T))

# test the function process to debug

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary impression fields
  resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
  
  # get codes that indicate a missing data element
  
  missing_codes <- "7701003|7701001"
  
  # filter the table to get the initial population regardless of age
  initial_population <- resp_01_clean %>% 
    
    # filter down to 911 calls
    
    dplyr::filter(grepl(pattern = codes_911, x = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05, ignore.case = T),
                  
                  # Identify Records that have Respiratory Distress Codes defined above
                  
                  if_any(
                    c(SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11, SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12), ~ grepl(pattern = resp_codes, x = ., ignore.case = T)
                  )
    ) %>% 
    
    # make sure that 
    mutate(vitals_check = if_else(!is.na(PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12) & !is.na(PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14), 1, 0
    )
    )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population %>% 
    dplyr::filter(PATIENT_AGE_IN_YEARS_E_PATIENT_15 >= 18)
  
  # filter peds
  peds_pop <- initial_population %>% 
    dplyr::filter(PATIENT_AGE_IN_YEARS_E_PATIENT_15 < 18)
  
  # get the summary of results
  
  # all
  total_population <- initial_population %>% 
    summarize(pop = "All",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # adults
  adult_population <- adult_pop %>% 
    summarize(pop = "Adults",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # peds
  peds_population <- peds_pop %>% 
    summarize(pop = "Peds",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # summary
  respiratory_01 <- bind_rows(adult_population, peds_population, total_population)
  
  respiratory_01


# test the function

resp_01_clean %>% 
  group_by(region) %>% 
  resp_01(eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
          esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
          esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
          evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
          evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14,
          epatient_15_col = PATIENT_AGE_IN_YEARS_E_PATIENT_15
          )

}, venue = "gh", advertise = T)


epatient.15 <- unique(resp_01_data["Patient Age In Years (ePatient.15)"])
esituation.11 <- unique(resp_01_data["Situation Provider Primary Impression Code (eSituation.11)"])
esituation.12 <- unique(resp_01_data["Situation Provider Secondary Impression Code List (eSituation.12)"])
eresponse.05 <- unique(resp_01_data["Response Type Of Service Requested With Code (eResponse.05)"])
evitals.12 <- unique(resp_01_data["Patient Initial Pulse Oximetry (eVitals.12)"])
evitals.14 <- unique(resp_01_data["Patient Initial Respiratory Rate (eVitals.14)"])


write_csv(x = epatient.15, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_patient_ages.csv")

write_csv(x = esituation.11, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_first_impressions.csv")

write_csv(x = esituation.12, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_second_impressions.csv")

write_csv(x = eresponse.05, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_response_types.csv")

write_csv(x = evitals.12, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_sp02.csv")

write_csv(x = evitals.14, file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Desktop/Analytics/Analytics Builds/GitHub/nemsqar-package/ems_respiratory_rate.csv")

respiratory_01_test <- data.frame(`Patient Age In Years (ePatient.15)` = sample(0:120, size = 1000, replace = T),
                                  `Situation Provider Primary Impression Code (eSituation.11)` = 
                                  )
