reprex::reprex({

################################################################################
### Test for Seizure-02 ########################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# function
  
  seizure_02 <- function(df,
                         incident_date_col,
                         patient_DOB_col,
                         epatient_15_col,
                         epatient_16_col,
                         eresponse_05_col,
                         esituation_11_col,
                         esituation_12_col,
                         emedications_03_col,
                         ...) {
    
    # provide better error messaging if df is missing
    if (missing(df)) {
      cli_abort(
        c(
          "No object of class {.cls data.frame} was passed to {.fn seizure_02}.",
          "i" = "Please supply a {.cls data.frame} to the first argument in {.fn seizure_02}."
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
    
    # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
    
    # 911 codes for eresponse.05
    codes_911 <- "2205001|2205003|2205009"
    
    # get codes as a regex to filter primary/secondary impression fields
    epilepsy_pattern <- "epilepsy.*?with status epilepticus|G40\\.\\d{1,3}"
    
    # medication values for seizure_02
    
    medication_pattern = "3322|6960|203128|6470|diazepam|midazolam|midazolam hydrochloride|lorazepam"
    
    # minor values
    minor_values <- "days|hours|minutes|months"
    
    # filter the table to get the initial population regardless of age
    initial_population <- df |>
      
      # create the age in years variable
      
      mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365,
      
      # create the respiratory distress variable
      seizure = if_any(c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(
        pattern = epilepsy_pattern,
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
      system_age_minor1 = {{epatient_15_col}} < 18 & {{epatient_15_col}} >= 2 & {{epatient_16_col}} == "Years",
      system_age_minor2 = {{epatient_15_col}} >= 24 & {{epatient_16_col}} == "Months",
      system_age_minor = system_age_minor1 | system_age_minor2,
      
      # calculated age checks
      calc_age_adult = patient_age_in_years_col >= 18,
      calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
      ) |>
      
      dplyr::filter(
        
        # Identify Records that have seizure documentation defined above
        seizure,
        
        # filter down to 911 calls
        call_911
        
      ) |>
      
      # check to see if target meds were passed
      mutate(meds_check = if_else(grepl(pattern = medication_pattern, x = {{emedications_03_col}}, ignore.case = T), 1, 0)
      )
    
    # Adult and Pediatric Populations
    
    # filter adult
    adult_pop <- initial_population |>
      dplyr::filter(system_age_adult | calc_age_adult)
    
    # filter peds
    peds_pop <- initial_population |>
      dplyr::filter(system_age_minor | calc_age_minor)
    
    # get the summary of results
    
    # all
    total_population <- initial_population |>
      summarize(
        measure = "Seizure-02",
        pop = "All",
        numerator = sum(meds_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # adults
    adult_population <- adult_pop |>
      summarize(
        measure = "Seizure-02",
        pop = "Adults",
        numerator = sum(meds_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # peds
    peds_population <- peds_pop |>
      summarize(
        measure = "Seizure-02",
        pop = "Peds",
        numerator = sum(meds_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # summary
    seizure.02 <- bind_rows(adult_population, peds_population, total_population)
    
    seizure.02
    
    
  }
  
# load data

seizure_02_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/seizure02_Export_2023.csv") |> 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean

seizure_02_clean <- seizure_02_data |> 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# run function

seizure_02_clean |> 
  seizure_02(incident_date_col = INCIDENT_DATE,
             patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
             epatient_15_col = PATIENT_AGE_E_PATIENT_15,
             epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
             eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
             esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
             esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
             emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03
            )

}, venue = "gh", advertise = TRUE)
