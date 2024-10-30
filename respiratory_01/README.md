# `respiratory_01`

## Description
The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.

## Usage
```R
respiratory_01(df, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col, epatient_15_col)
```

# Arguments
* `df`: A data frame containing incident data with each row representing an observation.
* `eresponse_05_col`: Column name for 911 response codes (e.g., 2205001, 2205003, 2205009).
* `esituation_11_col`: Column name for primary impression codes related to respiratory distress.
* `esituation_12_col`: Column name for secondary impression codes related to respiratory distress.
* `evitals_12_col`: Column name for the first vital sign measurement.
* `evitals_14_col`: Column name for the second vital sign measurement.
* `epatient_15_col`: Column name for patient age, in years.

# Output
Returns a data frame summarizing the proportion of cases with complete vital sign data, divided into:

* Adults: Patients aged 18 or older.
* Peds: Patients under 18.
* All: Total population across all ages.

# Features
* Filters data based on 911 response and specific respiratory distress codes.
* Identifies complete vital sign recordings.
* Computes summary statistics for adult, pediatric, and total populations.

# Value
A summary data frame containing the following columns:
* `pop`: Population group (e.g., "Adults," "Peds," "All").
* `numerator`: Count of cases with complete vital sign data.
* `denominator`: Total number of cases in the group.
* `prop`: Proportion of cases with complete vital signs formatted as a decimal number.
* `prop_label`: Proportion of cases with complete vital signs, formatted as a percentage.

# Example

``` r
# packages
  
library(tidyverse)
library(janitor)
#> 
#> Attaching package: 'janitor'
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
  
# functions
  
  pretty_percent <- function(variable, n_decimal = 0.1) {
    
    formatted_percent <- percent(variable, accuracy = n_decimal)
    
    # If there are trailing zeros after decimal point, remove them
    formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
    
    # If it ends with ".%", replace it with "%"
    formatted_percent <- sub("\\.%$", "%", formatted_percent)
    
    formatted_percent
    
  }

  respiratory_01 <- function(df, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col, epatient_15_col) {
    
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
  
# load data
  
  set.seed(50319)

  respiratory_01_test <- tibble(
    `Patient Age In Years (ePatient.15)` = sample(0:120, size = 1000, replace = T),
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

# test the function process

  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary impression fields
  resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
  
  # get codes that indicate a missing data element
  
  missing_codes <- "7701003|7701001"
  
  # filter the table to get the initial population regardless of age
  initial_population <- respiratory_01_test_clean %>% 
    
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
#> # A tibble: 3 × 5
#>   pop    numerator denominator  prop prop_label
#>   <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Adults       131         133 0.985 98.5%     
#> 2 Peds          39          39 1     100%      
#> 3 All          170         172 0.988 98.84%


# test the function

respiratory_01_test_clean %>% 
  respiratory_01(eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
          esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
          esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
          evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
          evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14,
          epatient_15_col = PATIENT_AGE_IN_YEARS_E_PATIENT_15
          )
#> # A tibble: 3 × 4
#>   pop    numerator denominator prop  
#>   <chr>      <dbl>       <int> <chr> 
#> 1 Adults       131         133 98.5% 
#> 2 Peds          39          39 100%  
#> 3 All          170         172 98.84%

# by region

respiratory_01_test_clean %>% 
  group_by(region) %>% 
  respiratory_01(eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
          esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
          esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
          evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
          evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14,
          epatient_15_col = PATIENT_AGE_IN_YEARS_E_PATIENT_15
          ) %>% 
  print(n = Inf)
#> # A tibble: 24 × 5
#>    region pop    numerator denominator prop  
#>    <chr>  <chr>      <dbl>       <int> <chr> 
#>  1 1A     Adults        18          18 100%  
#>  2 1C     Adults        15          15 100%  
#>  3 2      Adults        18          18 100%  
#>  4 3      Adults        12          13 92.31%
#>  5 4      Adults        15          15 100%  
#>  6 5      Adults        15          15 100%  
#>  7 6      Adults        15          15 100%  
#>  8 7      Adults        23          24 95.83%
#>  9 1A     Peds           8           8 100%  
#> 10 1C     Peds           3           3 100%  
#> 11 2      Peds           5           5 100%  
#> 12 3      Peds           8           8 100%  
#> 13 4      Peds           4           4 100%  
#> 14 5      Peds           3           3 100%  
#> 15 6      Peds           4           4 100%  
#> 16 7      Peds           4           4 100%  
#> 17 1A     All           26          26 100%  
#> 18 1C     All           18          18 100%  
#> 19 2      All           23          23 100%  
#> 20 3      All           20          21 95.24%
#> 21 4      All           19          19 100%  
#> 22 5      All           18          18 100%  
#> 23 6      All           19          19 100%  
#> 24 7      All           27          28 96.43%
```

<sup>Created on 2024-10-30 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

Original code credit goes to the illustrious Alyssa Whim.
