reprex::reprex({

################################################################################
### Test for Respiratory-02 ####################################################
################################################################################

# load needed packages

library(tidyverse)
library(janitor)
library(scales)
library(rlang)

# load needed functions
  
  respiratory_02 <- function(df,
                             erecord_01_col,
                             incident_date_col,
                             patient_DOB_col,
                             epatient_15_col,
                             epatient_16_col,
                             eresponse_05_col,
                             evitals_12_col,
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
    
    # minor values
    minor_values <- "days|hours|minutes|months"
    
    # not values for meds
    not_med <- "8801001|8801003|8801009|8801019|8801027"
    
    # not values for procedures
    not_proc <- "8801001|8801023|8801003|8801019|8801027"
    # some manipulations to prepare the table
    # create the patient age in years and the patient age in days variables for filters
    # create the unique ID variable
    initial_population_0 <- df %>%
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
        )),
        # get 911 variable
        call_911 = grepl(
          pattern = codes_911,
          x = {{eresponse_05_col}},
          ignore.case = T
        ),
        
        # get sp02 variable
        pulse_oximetry_check = {{evitals_12_col}} < 90,
        
        # system age checks
        system_age_adult = {{epatient_15_col}} >= 18 & {{epatient_16_col}} == "Years",
        system_age_minor1 = {{epatient_15_col}} < 18 & {{epatient_16_col}} == "Years",
        system_age_minor2 = {{epatient_15_col}} <= 120 & grepl(pattern = minor_values, x = {{epatient_16_col}}, ignore.case = T),
        system_age_minor = system_age_minor1 | system_age_minor2,
        system_age_minor_exclusion1 = {{epatient_15_col}} < 24 & {{epatient_16_col}} == "Hours", 
        system_age_minor_exclusion2 = {{epatient_15_col}} < 120 & {{epatient_16_col}} == "Minutes",
        system_age_minor_exclusion = system_age_minor_exclusion1 | system_age_minor_exclusion2,
        
        # calculated age checks
        calc_age_adult = patient_age_in_years >= 18,
        calc_age_minor = patient_age_in_years < 18 & patient_age_in_days >= 1,
        
        # oxygen variable for numerator
        oxygen_med = grepl(pattern = "7806", x = {{emedications_03_col}}) & 
          !grepl(pattern = not_med, x = {{emedications_03_col}}, ignore.case = T),
        oxygen_proc = grepl(pattern = "57485005", x = {{eprocedures_03_col}}, ignore.case = T) & 
          !grepl(pattern = not_proc, x = {{eprocedures_03_col}}, ignore.case = T)
      )
    
    # finish filters and make the table distinct
    initial_population <- initial_population_0 %>%
      filter(
        
        # pulse oximetry < 90
        pulse_oximetry_check,
        
        # 911 calls
        call_911
        
      ) %>%
      mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>%
      distinct(Unique_ID, .keep_all = T) # this will ensure each row is an observation, and each column is a feature
    
    # get population 1 for respiratory-02, peds
    respiratory_02_peds <- initial_population %>%
      filter((system_age_minor & !system_age_minor_exclusion) | 
               calc_age_minor
      )
    
    # get population 2 for respiratory-02, adults
    respiratory_02_adults <- initial_population %>%
      filter(system_age_adult | calc_age_adult)
    
    # calculations for peds
    peds_calculation <- respiratory_02_peds %>%
      summarize(
        measure = "Respiratory-02",
        pop = "Peds",
        numerator = sum(
          oxygen_med | oxygen_proc,
          na.rm = T
        ),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(prop, n_decimal = 0.01),
        ...
      )
    
    # calculations for adults
    adults_calculation <- respiratory_02_adults %>%
      summarize(
        measure = "Respiratory-02",
        pop = "Adults",
        numerator = sum(
          oxygen_med | oxygen_proc,
          na.rm = T
        ),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(prop, n_decimal = 0.01),
        ...
      )
    
    # bind rows of calculations for final table
    respiratory.02 <- bind_rows(peds_calculation, adults_calculation)
    
    respiratory.02
    
  }

# load data
  
respiratory_02_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory02_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean

respiratory_02_clean <- respiratory_02_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# test the function

respiratory_02_clean %>% 
  respiratory_02(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                 incident_date_col = INCIDENT_DATE,
                 patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                 epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                 epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                 eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                 evitals_12_col = PATIENT_LOW_PULSE_OXIMETRY_E_VITALS_12,
                 emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
                 eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03
                 )


}, venue = "gh", advertise = T)
