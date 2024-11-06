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
                        eresponse_05_col,
                        edisposition_18_col,
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
    
    if (!is.Date(df[[as_name(incident_date)]]) &
        !is.POSIXct(df[[as_name(incident_date)]]) &
        !is.Date(df[[as_name(patient_DOB)]]) &
        !is.POSIXct(df[[as_name(patient_DOB)]])) {
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
    
    # define transports
    transport_responses <-
      "Patient Refused Evaluation/Care (With Transport)|Patient Treated, Transported by this EMS Unit|TX W/Mutual Aid Transported|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Transport by This EMS Unit (This Crew Only)|Transport by This EMS Unit, with a Member of Another Crew|Mutual Aid Tx & Transport|4212033|itDisposition.112.116|4212023|itDisposition.112.141|itDisposition.112.142|itDisposition.112.111"
    
    # get codes as a regex to find lights and siren responses
    lights_and_sirens <- "Initial Lights and Sirens, Downgraded to No Lights or Sirens|Initial No Lights or Sirens, Upgraded to Lights and Sirens|Lights and Sirens"
    
    # filter the table to get the initial population regardless of age
    initial_population <- df %>%
      
      # create the age in years variable
      
      mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365,
      transport = if_else(if_any(c({{transport_disposition_cols}}), ~ grepl(pattern = transport_responses, x = ., ignore.case = T)), TRUE, FALSE
      )) %>%
      
      # filter down to 911 calls that involved transport
      
      dplyr::filter(grepl(
        pattern = codes_911,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),
      transport == TRUE
      ) %>%
      
      # if lights and sirens ARE NOT present, 1, else 0
      mutate(l_s_check = if_else(!grepl(pattern = lights_and_sirens, x = {{edisposition_18_col}}), 1, 0))
    
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
        measure = "Safety-02",
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
        measure = "Safety-02",
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
        measure = "Safety-02",
        pop = "Peds",
        numerator = sum(l_s_check, na.rm = T),
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
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
  )))

# run function

safety_02_clean %>% 
  safety_02(incident_date_col = INCIDENT_DATE,
            PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            DISPOSITION_ADDITIONAL_TRANSPORT_MODE_DESCRIPTOR_LIST_E_DISPOSITION_18,
            transport_disposition_cols = c(DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112, 
                                           TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30)
            )

}, venue = "gh", advertise = TRUE)
