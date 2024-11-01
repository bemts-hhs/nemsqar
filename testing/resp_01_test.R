respiratory_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory01_Export.csv")

respiratory_01_clean <- respiratory_01_data %>% 
  mutate(across(c(`Incident Date`, `Patient Date Of Birth (ePatient.17)`), ~ mdy(
    str_remove_all(string = ., pattern = "\\s12:00:00\\sAM")
  ))) %>% 
  mutate(Patient_age_in_days = as.numeric(
    difftime(`Incident Date`, `Patient Date Of Birth (ePatient.17)`, units = "days")),
         Patient_age_in_mins = as.numeric(
           difftime(`Incident Date`, `Patient Date Of Birth (ePatient.17)`, units = "mins")),
        Patient_age_in_years = Patient_age_in_days / 365,
         .before = `Patient Age In Years (ePatient.15)`
         )


reprex::reprex({

################################################################################
### Respiratory-01 Test  #######################################################
################################################################################

  library(tidyverse)
  library(janitor)
  library(rlang)
  library(scales)
  
# the function will load necessary packages in the background
  
  respiratory_01 <- function(df, incident_date_col, patient_DOB_col, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col) {
    
    # Load necessary packages
    for (pkg in c("tidyverse", "janitor", "scales", "rlang")) {
      if (!pkg %in% installed.packages()) install.packages(pkg, quiet = TRUE)
      if (!paste0("package:", pkg) %in% search()) library(pkg, quietly = TRUE)
    }
    
    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !is_tibble(df)) {
      cli_abort(c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
      ))
    }
    
    # provide better error messaging if df is missing
    if(missing(df)) {
      
      cli_abort(c("No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
                  "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
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
    
    # get codes as a regex to filter primary impression fields
    resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
    
    # get codes that indicate a missing data element
    
    missing_codes <- "7701003|7701001"
    
    # filter the table to get the initial population regardless of age
    initial_population <- df %>% 
      
      # create the age in years variable
      
      mutate(patient_age_in_years_col = as.numeric(
        difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days")) / 365
        
      ) %>% 
      
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
      dplyr::filter(patient_age_in_years_col >= 18)
    
    # filter peds
    peds_pop <- initial_population %>% 
      dplyr::filter(patient_age_in_years_col < 18)
    
    # get the summary of results
    
    # all
    total_population <- initial_population %>% 
      summarize(pop = "All",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = numerator / denominator,
                prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # adults
    adult_population <- adult_pop %>% 
      summarize(pop = "Adults",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = numerator / denominator,
                prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # peds
    peds_population <- peds_pop %>% 
      summarize(pop = "Peds",
                numerator = sum(vitals_check, na.rm = T),
                denominator = n(),
                prop = numerator / denominator,
                prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
      )
    
    # summary
    resp_01 <- bind_rows(adult_population, peds_population, total_population)
    
    resp_01
    
    
  }  
  
# load data
  
  set.seed(50319)

  respiratory_01_test <- tibble(
    `Incident Date` = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                               size = 1000, replace = TRUE),
    `Patient Date Of Birth (ePatient.17)` = sample(seq(as.Date("1980-01-01"), as.Date("2022-12-31"), by = "day"), 
                             size = 1000, replace = TRUE),
    `Situation Provider Primary Impression Code (eSituation.11)` = sample(
      c(
        "I50.9",
        "J00",
        "J05",
        "J18.9",
        "J20.9",
        "J44.1",
        "J45.901",
        "J80",
        "J81",
        "J93.9",
        "J96",
        "J98.01",
        "J98.9",
        "R05",
        "R06",
        "R09.2",
        "T17.9",
        "A08.4",
        "A09",
        "A37.90",
        "A41",
        "A41.9",
        "A48.8",
        "B30.9",
        "B34.2",
        "B95.62",
        "B96.7",
        "B96.89",
        "B97.2",
        "B97.21",
        "B97.29",
        "B97.4",
        "B99.9",
        "C18.9",
        "C25.9",
        "C34.90",
        "C71.9"
      ),
      size = 1000,
      replace = T
    ),
    `Situation Provider Secondary Impression Code List (eSituation.12)` = sample(
      c(
        "I50.9",
        "J00",
        "J05",
        "J18.9",
        "J20.9",
        "J44.1",
        "J45.901",
        "J80",
        "J81",
        "J93.9",
        "J96",
        "J98.01",
        "J98.9",
        "R05",
        "R06",
        "R09.2",
        "T17.9",
        "A08.4",
        "A09",
        "A37.90",
        "A41",
        "A41.9",
        "A48.8",
        "B30.9",
        "B34.2",
        "B95.62",
        "B96.7",
        "B96.89",
        "B97.2",
        "B97.21",
        "B97.29",
        "B97.4",
        "B99.9",
        "C18.9",
        "C25.9",
        "C34.90",
        "C71.9"
      ),
      size = 1000,
      replace = T
    ),
    `Response Type Of Service Requested With Code (eResponse.05)` = sample(
      c(
        "911 Response (Scene) (2205001)",
        "Interfacility Transport (2205005)",
        "Emergency Response (Primary Response Area) (2205001)",
        "Medical Transport (2205007)",
        "Public Assistance (2205011)",
        "Standby (2205013)",
        "Public Assistance/Other Not Listed (2205011)",
        "Emergency Response (Intercept) (2205003)",
        "Intercept (2205003)",
        "Hospital-to-Hospital Transfer (2205005)",
        "Other Routine Medical Transport (2205007)",
        "Hospital to Non-Hospital Facility Transfer (2205015)",
        "Support Services (2205021)",
        "Mutual Aid (2205009)",
        "Emergency Response (Mutual Aid) (2205009)",
        "Assist Unit (it2205.145)",
        "Mortuary Services (2205029)",
        "Non-Hospital Facility to Non-Hospital Facility Transfer (2205017)",
        NA,
        "Non-Patient Care Rescue/Extrication (2205023)",
        "Non-Hospital Facility to Hospital Transfer (2205019)",
        "Administrative Operations (2205035)",
        "Community Paramedicine (it2205.121)",
        "Crew Transport Only (2205025)",
        "MIHC/Community Paramedicine (2205031)",
        "Organ Transport (2205027)",
        "Evaluation for Special Referral/Intake Programs (2205033)"
      ),
      size = 1000,
      replace = T
    ),
    `Patient Initial Pulse Oximetry (eVitals.12)` = sample(0:100, size = 1000, replace = T),
    sp02 = sample(0:100, size = 1000, replace = T),
    `Patient Initial Respiratory Rate (eVitals.14)` = sample(0:272, size = 1000, replace = T),
    RR = sample(0:272, size = 1000, replace = T)
  ) %>%
    mutate(
      `Patient Initial Pulse Oximetry (eVitals.12)` = if_else(
        `Patient Initial Pulse Oximetry (eVitals.12)` == sp02,
        NA_real_,
        `Patient Initial Pulse Oximetry (eVitals.12)`
      ),
      `Patient Initial Respiratory Rate (eVitals.14)` = if_else(
        `Patient Initial Respiratory Rate (eVitals.14)` == RR,
        NA_real_,
        `Patient Initial Respiratory Rate (eVitals.14)`
      )
    ) %>%
    dplyr::select(-sp02, -RR)
  
# clean up names

respiratory_01_test_clean <- respiratory_01_test %>% 
  clean_names(case = "screaming_snake", sep_out = "_") %>% 
  mutate(region = sample(c("1A", "1C", "2", "3", "4", "5", "6", "7"), size = nrow(respiratory_01_test), replace = T))

# test the function

respiratory_01_test_clean %>% 
  respiratory_01(
    incident_date_col = INCIDENT_DATE,
    patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
    eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
    esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
    esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
    evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
    evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14
  )

# by region

respiratory_01_test_clean %>% 
  group_by(region) %>% 
  respiratory_01(
    incident_date_col = INCIDENT_DATE,
    patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
    eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
    esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
    esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
    evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
    evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14
  ) %>% 
  print(n = Inf)
  

}, venue = "gh", advertise = T)





