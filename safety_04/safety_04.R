################################################################################
### Safety-04 Function #########################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### Need to be a table where each row is 1 observation and each column is a feature
### or distinct datasets that can be referenced as unique columns
### this function will calculate an age in years
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the function assumes that the eresponse.05 column has the codes in it, text
### can be present, too, for reference
### the function assumes that edisposition.14 is a list column or a column that has all
### text descriptors for additional transport mode descriptors.  These can be separated
### by commas or other characters as long as all eresponse.14 values are present
### in one cell for each unique erecord.01 value.  Codes can be present
### but will be ignored by the function.
### the function assumes that earrest.01 values contain the text and code, and are typically
### one response per patient encounter per responding service.
### eprocedures.03 is a list column or a column that has all text descriptors for additional transport mode descriptors.  
### These can be separated by commas or other characters as long as all eprocedures.03 values are present
### in one cell for each unique erecord.01 value.  Codes can be present.
### all values for einjury.03 can be passed to the function.  The function will detect any duplicate values
### and will filter on the unique values per each unique patient encounter.
### edisposition.12 and edisposition.30 must be list columns that and/or contain all values
### from each unique incident entered for each field.  These can be comma separated values
### all in one cell to make the table tidy.
### the first argument is a dataframe, no joining is done.
### any joins to get vitals etc. will need to be done outside the function
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________

safety_04 <- function(df,
                      incident_date_col,
                      patient_DOB_col,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      earrest_01_col,
                      einjury_03_col,
                      eprocedures_03_col,
                      edisposition_14_col,
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
        "No object of class {.cls data.frame} was passed to {.fn safety_04}.",
        "i" = "Please supply a {.cls data.frame} to the first argument in {.fn safety_04}."
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
  
  # transport code eresponse.05
  transport_code <- "2205005|Interfacility Transport"
  
  # define transports
  transport_responses <- "Transport by This EMS Unit \\(This Crew Only\\)|Transport by This EMS Unit, with a Member of Another Crew|Transport by Another EMS Unit, with a Member of This Crew|Patient Treated, Transported by this EMS Unit|Patient Treated, Transported with this EMS Crew in Another Vehicle|Treat / Transport ALS by this unit|Treat / Transport BLS by this unit|Mutual Aid Tx & Transport|4212033|4230001|4230003|4230007|itDisposition\\.112\\.116|it4212\\.142|itDisposition\\.112\\.165|itDisposition\\.112\\.141|Treat / Transport BLS by this unit|itDisposition\\.112\\.142"
  
  # get codes as a regex to find cardiac arrest responses
  cardiac_arrest_responses <- "3001005|3001003|Yes, Prior to Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)|Yes, After Any EMS Arrival \\(includes Transport EMS & Medical First Responders\\)"
  
  # get applicable trauma triage codes for steps 1 and 2
  trauma_triage_crit <- "2903001|Amputation proximal to wrist or ankle|2903003|Crushed, degloved, mangled, or pulseless extremity|2903005|Chest wall instability or deformity|2903007|Glasgow Coma Score <=13|2903009|Open or depressed skull fracture|2903011|Paralysis|2903013|Pelvic fractures|2903015|All penetrating injuries to head, neck, torso, and extremities proximal to elbow or knee|2903017|Respiratory Rate <10 or >29 breaths per minute \\(<20 in infants aged <1 year\\) or need for ventilatory support|3903019|Systolic Blood Pressure <90 mmHg|2903021|Two or more long-bone fractures"
  
  # procedure exclusion related to long board
  
  long_board <- "450591000124106|Immobilization using long board"
  
  # additional procedures in the exclusion
  
  airway_procedures <- "16883004|Endotracheal intubation, emergency procedure|182682004|Emergency laryngeal intubation|232674004|Orotracheal intubation|232678001|Orotracheal fiberoptic intubation|232682004|Nasotracheal fiberoptic intubation|232685002|Insertion of tracheostomy tube|304341005|Awake intubation|418613003|Tracheal intubation through a laryngeal mask airway|424979004|Laryngeal mask airway insertion|427753009|Insertion of esophageal tracheal double lumen supraglottic airway|429161001|Insertion of endotracheal tube using laryngoscope|450611000124|Insertion of Single Lumen Supraglottic Airway Device"
  
  # car seat code for edisposition.14
  
  car_seat <- "4214001|Car Seat"
  
  # minor age units
  
  minor_age_units <- "days|hours|minutes"
  
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
      
      # interfacility variable
      interfacility = grepl(
        pattern = transport_code,
        x = {{eresponse_05_col}},
        ignore.case = T
      ),
      
      # interfacility transport or otherwise transported variable
      interfacility_or_transport = transport | interfacility,
      
      # cardiac arrest check
      cardiac_arrest_check = grepl(pattern = cardiac_arrest_responses, x = {{earrest_01_col}}, ignore.case = T),
      
      # severe injury flag
      trauma_triage_flag = grepl(pattern = trauma_triage_crit, x = {{einjury_03_col}}, ignore.case = T),
      
      # long board flag
      long_board_flag = grepl(pattern = long_board, x = {{eprocedures_03_col}}, ignore.case = T),
      
      # airway procedures flag
      airway_proc_flag = grepl(pattern = airway_procedures, x = {{eprocedures_03_col}}, ignore.case = T),
      
      # numerator condition - car seat
      car_seat_check = if_else(grepl(pattern = car_seat, x = {{edisposition_14_col}}, ignore.case = T), 1, 0),
      
      # system age check
      system_age_minor1 = {{epatient_15_col}} <= 8 & {{epatient_16_col}} == "Years",
      system_age_minor2 = {{epatient_15_col}} < 96 & {{epatient_16_col}} == "Months",
      system_age_minor3 = {{epatient_15_col}} <= 120 & grepl(pattern = minor_age_units, x = {{epatient_16_col}}, ignore.case = T),
      system_age_minor = system_age_minor1 | system_age_minor2 | system_age_minor3,
      
      # calculated age check
      calc_age_minor = patient_age_in_years_col <= 8
    )
    
  # get the initial population
    initial_population <- initial_population_0 %>% 
      dplyr::filter(
      
      # filter down to age < 8 years
      system_age_minor | calc_age_minor,
      
      # NEMSIS 3.5 transports / interfacility only
      interfacility_or_transport
      
      )
    
  # Only calculate for pediatric patients < 8 yrs of age
  
  # filter peds for the exclusion criteria
  peds_pop <- initial_population %>%
    dplyr::filter(!cardiac_arrest_check &
                    !trauma_triage_flag &
                    !long_board_flag &
                    !airway_proc_flag
                  )
  
  # get the summary of results
  
  # peds
  peds_population <- peds_pop %>%
    summarize(
      measure = "Safety-04",
      pop = "Peds",
      numerator = sum(car_seat_check, na.rm = T),
      denominator = n(),
      prop = numerator / denominator,
      prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01),
      ...
    )
  
  # summary
  safety.04 <- peds_population
  
  safety.04
  
  
}
