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
                             epatient_15_col,
                             epatient_16_col,
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
          "No object of class {.cls data.frame} was passed to {.fn pediatrics_03b}.",
          "i" = "Please supply a {.cls data.frame} to the first argument in {.fn pediatrics_03b}."
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
    non_weight_based_meds <- "Inhalation|Topical|9927049|9927009"
    
    # minor values
    minor_values <- "days|hours|minutes|months"
    
    # filter the table to get the initial population regardless of age, only 911 responses
    initial_population_0 <- df %>%
      mutate(patient_age_in_years_col = as.numeric(difftime(
        time1 = {{incident_date_col}},
        time2 = {{patient_DOB_col}},
        units = "days"
      )) / 365,
      
      # check to see if non-weight-based meds
      non_weight_based = grepl(
        pattern = non_weight_based_meds,
        x = {{emedications_04_col}},
        ignore.case = TRUE
      ),
      call_911 = grepl(
        pattern = codes_911,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),
      meds_not_missing = !is.na({{emedications_03_col}}),
      system_age_check = {{epatient_15_col}} < 18 & {{epatient_16_col}} == "Years" | 
        !is.na({{epatient_15_col}}) & grepl(pattern = minor_values, x = {{epatient_16_col}}, ignore.case = T),
      calc_age_check = patient_age_in_years_col < 18
      ) %>%
      dplyr::filter(
        # only 911 calls
        call_911,
        
        # only rows where meds are passed
        meds_not_missing,
        
        # age filter
        system_age_check | calc_age_check,
        
        # exclude non-weight based meds
        !non_weight_based
      )
    
    initial_population_1 <- initial_population_0 %>%
      
      mutate(
        # check if weight was documented
        documented_weight = if_else(!is.na({{eexam_01_col}}) |
                                      !is.na({{eexam_02_col}}), 1, 0),
      )
    
    # second filtering process, make the table distinct by rolling up emedications.04
    # based on a unique identifier
    initial_population <- initial_population_1 %>%
      mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>% 
      distinct(Unique_ID, .keep_all = T)
    
    # get the summary of results, already filtered down to the target age group for the measure
    
    # peds
    pediatrics.03b <- initial_population %>%
      summarize(
        measure = "Pediatrics-03b",
        pop = "Peds",
        numerator = sum(documented_weight, na.rm = T),
        denominator = n(),
        prop = numerator / denominator,
        prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
        ...
      )
    
    # summary
    pediatrics.03b
    
  }
  
  # load data
  
  pediatrics_03b_data <- read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/pediatrics03b_Export_2023.csv"
  ) %>%
    clean_names(case = "screaming_snake", sep_out = "_")
  
  # clean
  
  pediatrics_03b_clean <- pediatrics_03b_data %>%
    mutate(across(
      c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17),
      ~ mdy(str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?"))
    ))
  
  # run function
  
  pediatrics_03b_clean %>%
    pediatrics_03b(
      erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
      incident_date_col = INCIDENT_DATE,
      patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
      epatient_15_col = PATIENT_AGE_E_PATIENT_15,
      epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
      eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
      eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
      eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
      emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
      emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
    )
  
}, venue = "gh", advertise = TRUE)
