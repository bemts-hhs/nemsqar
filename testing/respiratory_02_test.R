reprex::reprex({

# load needed packages

library(tidyverse)
library(scales)
library(rlang)

# load needed functions

  respiratory_02 <- function(df,
                             erecord_01_col,
                             incident_date_col,
                             patient_DOB_col,
                             eresponse_05_col,
                             evitals_12_col,
                             emedications_03_col,
                             eprocedures_03_col,
                             ...) {
    
    # Load necessary packages
    for (pkg in c("tidyverse", "scales", "rlang")) {
      if (!pkg %in% installed.packages()) install.packages(pkg, quiet = TRUE)
      if (!paste0("package:", pkg) %in% search()) library(pkg, quietly = TRUE)
    }
    
    # provide better error messaging if df is missing
    if(missing(df)) {
      
      cli_abort(c("No object of class {.cls data.frame} was passed to {.fn respiratory_01}.",
                  "i" = "Please supply a {.cls data.frame} to the first argument in {.fn respiratory_01}."
      ))
      
    }
    
    # Ensure df is a data frame or tibble
    if (!is.data.frame(df) && !is_tibble(df)) {
      cli_abort(c(
        "An object of class {.cls data.frame} or {.cls tibble} is required as the first argument.",
        "i" = "The passed object is of class {.val {class(df)}}."
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
    
    
    # some manipulations to prepare the table
    # create the patient age in years and the patient age in days variables for filters
    # create the unique ID variable
    initial_population <- df %>% 
      mutate(patient_age_in_years = as.numeric(
        difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days")) / 365,
        patient_age_in_days = as.numeric(
          difftime(time1 = {{incident_date_col}}, time2 = {{patient_DOB_col}}, units = "days"))
      ) %>% 
      rowwise() %>% # use rowwise() as we do not have a reliable grouping variable yet if the table is not distinct
      mutate(Unique_ID = str_c({{erecord_01_col}}, {{incident_date_col}}, {{patient_DOB_col}}, sep = "-")) %>% 
      ungroup()
    
    # finish filters and make the table distinct
    initial_population <- initial_population %>% 
      filter(
        grepl(
          pattern = "2205001|2205003|2205009",
          x = {{eresponse_05_col}},
          ignore.case = T
        ),
        {{evitals_12_col}} < 90
      ) %>%
      mutate(
        {{evitals_12_col}} := str_c({{evitals_12_col}}, collapse = ", "), # take all vitals for each pt and roll them into one cell per pt
        .by = Unique_ID
      ) %>% 
      distinct(Unique_ID, .keep_all = T) # this will ensure each row is an observation, and each column is a feature
    
    # get population 1 for respiratory-02, peds
    respiratory_02_peds <- initial_population %>% 
      filter(patient_age_in_years < 18,
             patient_age_in_days >= 24 
      )
    
    # get population 2 for respiratory-02, adults
    respiratory_02_adults <- initial_population %>% 
      filter(patient_age_in_years >= 18)
    
    if(is.null(by)) {
      
      # calculations for peds
      peds_calculation <- respiratory_02_peds %>% 
        summarize(pop = "Peds",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01)
        )
      
      # calculations for adults
      adults_calculation <- respiratory_02_adults %>% 
        summarize(pop = "Adults",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01)
        )
      
      # overall calculation
      total_population <- initial_population %>% 
        summarize(pop = "All",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01)
        )
      
      # bind rows of calculations for final table
      resp_02 <- bind_rows(total_population, peds_calculation, adults_calculation)
      
      resp_02
      
    } else if(!is.null(by)) {
      
      # calculations for peds
      peds_calculation <- respiratory_02_peds %>% 
        summarize(pop = "Peds",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01),
                  ...
        )
      
      # calculations for adults
      adults_calculation <- respiratory_02_adults %>% 
        summarize(pop = "Adults",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01),
                  ...
        )
      
      # overall calculation
      total_population <- initial_population %>% 
        summarize(pop = "All",
                  numerator = sum(
                    grepl(pattern = "7806", x = {{emedications_03_col}}) | 
                      grepl(pattern = "57485005", x = {{eprocedures_03_col}}), na.rm = T
                  ),
                  denominator = n(),
                  prop = numerator / denominator,
                  prop_label = pretty_percent(prop, n_decimal = 0.01),
                  ...
        )
      
      # bind rows of calculations for final table
      resp_02 <- bind_rows(total_population, peds_calculation, adults_calculation)
      
      resp_02
      
    }
    
  }

# a function to generate random numbers
generate_random_ID <- function(n, set_seed = 12345) {
  # choose whether or not to set the seed
  
  if (is.numeric(set_seed)) {
    set.seed(set_seed)
    
  } else if (is.null(set_seed)) {
    message(
      "Set.seed was not called internally, and so your results will not be consistent across applications.  Make [set_seed] any number to set the seed and make reproducible results!"
    )
    
  }
  
  random_strings <- vector(mode = "character", length = n)
  for (i in 1:n) {
    random_strings[i] <- paste0(paste0(sample(c(
      LETTERS, letters, LETTERS
    ), size = 10), collapse = ""),
    "-",
    sample(1000000000:9999999999, size = 1))
  }
  return(random_strings)
}

# load data
  
  set.seed(50319)

  respiratory_02_test <- tibble(`Incident Patient Care Report Number - PCR (eRecord.01)` = generate_random_ID(1000, set_seed = 50319),
    `Incident Date` = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                               size = 1000, replace = TRUE),
    `Patient Date Of Birth (ePatient.17)` = sample(seq(as.Date("1980-01-01"), as.Date("2022-12-31"), by = "day"), 
                             size = 1000, replace = TRUE),
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
    `Vitals Pulse Oximetry (eVitals.12)` = sample(0:100, size = 1000, replace = T),
    `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)` = sample(
      c("7806", NA_character_, "125464","214199, 7806", "4917, 1191, 7806", "285059", "214199", "7806, 313002", "214199, 6902", "6387, 7806","7806, 318272","7242, 7242, 26225, 7806, 7806", "435, 7806","4337, 26225","125464, 727374, 727374, 727374", "7806, 313002, 317361, 317361", "7806, 7806","125464, 7806, 237653", "92821, 7806", "7052, 7806, 1191, 26225", "435, 1191, 7806, 7806", "563812", "214199, 435", "125464, 6130", "7806, 1008501", "435", "4917, 285059", "202908, 285059", "435, 6585, 6902, 435", "7806, 435"), size = 1000, replace = T
    ),
    `Patient Attempted Procedure Codes List (eProcedures.03)` = sample(c("46825001, 46825001, 46825001, 46825001, 268400002, 392230005",
"422618004, 422618004, 252465000, 56342008, 33747003, 425543005",
"255560000, 255560000, 23852006, 268400002, 33747003",
NA_character_,
"392230005, 425447009, 7443007, 79821001",
"46825001",
"47545007, 284029005, 47545007, 386053000, 268400002, 428803005",
"392230005, 268400002, 392230005",
"392230005, 392230005, 47545007",
"386053000, 392230005, 428803005, 268400002",
"392230005",
"268400002, 392230005, 392230005, 284029005",
"268400002, 392230005, 392230005",
"392230005, 392230005, 268400002",
"392230005, 392230005, 392230005, 430824005, 392230005, 46825001, 47545007",
"89666000, 89666000, 182692007, 429283006, 7443007, 425447009, 243140006, 233169004",
"46825001, 46825001, 268400002",
"182692007, 392230005, 392230005",
"23852006",
"392230005, 392230005, 386053000, 268400002, 268400002, 268400002, 428803005, 77248004, 284029005, 47545007",
"268400002, 268400002, 268400002, 392230005, 392230005",
"398041008, 392230005, 182555002",
"268400002, 392230005, 428803005, 284029005",
"268400002, 268400002, 268400002",
"23852006, 33747003",
"61746007, 422618004, 404996007, 56469005",
"89666000, 430824005, 427569000, 425447009, 429283006, 232674004, 425543005",
"429283006, 232674004, 386053000, 89666000, 430824005",
"268400002, 392230005",
"386053000, 428803005, 392230005" ), size = 1000, replace = T)
  )
  
# add the region variable for grouping later

respiratory_02_test_prep <- respiratory_02_test %>% 
  mutate(region = sample(c("1A", "1C", "2", "3", "4", "5", "6", "7"), size = nrow(respiratory_02_test), replace = T))

# test the function

respiratory_02_test_prep %>% 
  respiratory_02(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                 incident_date_col = `Incident Date`,
                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                 emedications_03_col = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`,
                 eprocedures_03_col = `Patient Attempted Procedure Codes List (eProcedures.03)`
                 )

# test with grouping

respiratory_02_test_prep %>% 
  respiratory_02(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                 incident_date_col = `Incident Date`,
                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                 emedications_03_col = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`,
                 eprocedures_03_col = `Patient Attempted Procedure Codes List (eProcedures.03)`,
                 .by = region
                 ) %>% 
  print(n = Inf)

}, venue = "gh", advertise = T)
