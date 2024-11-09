reprex::reprex({

################################################################################
### Test for Safety-02 #########################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# function
  
  safety_02 <- function(df,
                        incident_date_col,
                        patient_DOB_col,
                        epatient_15_col,
                        epatient_16_col,
                        eresponse_05_col,
                        edisposition_18_col,
                        edisposition_28_col,
                        transport_disposition_cols,
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
    
    # patient evaluation care 
    patient_care <- "4228001|Patient Evaluated and Care Provided"
    
    # define transports
    transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
    
    # get codes as a regex to find lights and siren responses
    no_lights_and_sirens <- "4218015|No Lights or Sirens"
    
    # filter the table to get the initial population regardless of age
    initial_population_0 <- df %>%
      
      # create the age in years variable
      
      mutate(
        patient_age_in_years_col = as.numeric(difftime(
          time1 = {{incident_date_col}},
          time2 = {{patient_DOB_col}},
          units = "days"
        )) / 365,
        
        # transport variable
        transport = if_else(if_any(
          c({{transport_disposition_cols}}),
          ~ grepl(
            pattern = transport_responses,
            x = .,
            ignore.case = T
          )
        ), TRUE, FALSE),
        
        # 911 variable
        call_911 = grepl(
          pattern = codes_911,
          x = {{eresponse_05_col}},
          ignore.case = T
        ),
        
        # no lights and sirens check
        no_ls_check = if_else(grepl(pattern = no_lights_and_sirens, x = {{edisposition_18_col}}), 1, 0),
        
        # patient evaluation care provided
        patient_care_provided = grepl(pattern = patient_care, x = {{edisposition_28_col}}, ignore.case = T),
        
        # system age check
        system_age_adult = {{epatient_15_col}} >= 18 & {{epatient_16_col}} == "Years",
        system_age_minor1 = ({{epatient_15_col}} >= 2 & {{epatient_15_col}} < 18) & {{epatient_16_col}} == "Years",
        system_age_minor2 = {{epatient_15_col}} >= 24 & {{epatient_16_col}} == "Months",
        system_age_minor = system_age_minor1 | system_age_minor2,
        
        # calculated age check
        calc_age_adult = patient_age_in_years_col >= 18,
        calc_age_minor = patient_age_in_years_col < 18 & patient_age_in_years_col >= 2
      )
    
    # get the initial population
    initial_population <- initial_population_0 %>% 
      dplyr::filter(
        
        # filter down to 911 calls
        call_911,
        
        # patient evaluated and care provided only
        patient_care_provided,
        
        # NEMSIS 3.5 transports only
        transport
        
      )
    
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
        measure = "Safety-02",
        pop = "All",
        numerator = sum(no_ls_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # adults
    adult_population <- adult_pop %>%
      summarize(
        measure = "Safety-02",
        pop = "Adults",
        numerator = sum(no_ls_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # peds
    peds_population <- peds_pop %>%
      summarize(
        measure = "Safety-02",
        pop = "Peds",
        numerator = sum(no_ls_check, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # summary
    safety.02 <- bind_rows(adult_population, peds_population, total_population)
    
    safety.02
    
    
  }
  
# load data

safety_02_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/safety01_02_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean

safety_02_clean <- safety_02_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# run function

safety_02_clean %>% 
  safety_02(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            epatient_15_col = PATIENT_AGE_E_PATIENT_15,
            epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            edisposition_18_col = DISPOSITION_ADDITIONAL_TRANSPORT_MODE_DESCRIPTOR_LIST_E_DISPOSITION_18,
            edisposition_28_col = PATIENT_EVALUATION_CARE_3_4_IT_DISPOSITION_100_3_5_E_DISPOSITION_28,
            transport_disposition_cols = c(DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112, TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30)
            )

}, venue = "gh", advertise = TRUE)
