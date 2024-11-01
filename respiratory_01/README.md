# `respiratory_01`

## Description
The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.

## Usage
```R
respiratory_01(df, incident_date_col, patient_DOB_col, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col)
```

# Arguments
* `df`: A data frame containing incident data with each row representing an observation.
* `incident_date_col`: Column name for the Incident Date field.
* `patient_DOB_col`: Column name for epatient.17.
* `eresponse_05_col`: Column name for 911 response codes (e.g., 2205001, 2205003, 2205009).
* `esituation_11_col`: Column name for primary impression codes related to respiratory distress.
* `esituation_12_col`: Column name for secondary impression codes related to respiratory distress.
* `evitals_12_col`: Column name for the first vital sign measurement.
* `evitals_14_col`: Column name for the second vital sign measurement.
* `...`: arguments passed to `dplyr::summarize()`.

# Output
Returns a data frame summarizing the proportion of cases with complete vital sign data, divided into:

* `Adults`: Patients aged 18 or older.
* `Peds`: Patients under 18.
* `All`: Total population across all ages.

# Features
* Filters data based on 911 response and specific respiratory distress codes.
* Identifies complete vital sign recordings.
* Computes summary statistics for adult, pediatric, and total populations.
* Allows grouping via `dplyr::summarize()` using tidy dots `...`.

# Value
A summary data frame containing the following columns:
* `pop`: Population group (e.g., "Adults," "Peds," "All").
* `numerator`: Count of cases with complete vital sign data.
* `denominator`: Total number of cases in the group.
* `prop`: Proportion of cases with complete vital signs formatted as a decimal number.
* `prop_label`: Proportion of cases with complete vital signs, formatted as a percentage.

# Notes
* The function assumes data are already loaded and in the format specified below.
* Must supply: epatient.15, esituation.11, esituation.12 (as a column where each entry is in one cell), eresponse.05, initial evitals.12 value, initial evitals.14 value.
* The datasource supplied to the function should be a `tibble` or `dataframe` where each row is 1 observation and each column is a feature.
* This function assumes that there is an age in years calculated for epatient_15
* This function also assumes that rows that are missing any value are blank and will be coerced to NA, not the not known / not recorded values common to NEMSIS or the value codes that correspond to "not values".
* The function assumes that vitals in the vital signs columns are likely the first vital signs, or are a list column.  This will give an indication of whether or not any vitals were taken.
* The esituation.12 is best as concatenation of all secondary impressions entered
* The first argument is a dataframe, no joining is done by the function.
* Any joins to get vitals etc. will need to be done outside the function

# Example

``` r
################################################################################
### Respiratory-01 Test  #######################################################
################################################################################

  library(tidyverse)
  library(janitor)
#> 
#> Attaching package: 'janitor'
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
  library(rlang)
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:purrr':
#> 
#>     %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
#>     flatten_raw, invoke, splice
  library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
  
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
  
#> # A tibble: 3 × 5
#>   pop    numerator denominator  prop prop_label
#>   <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Adults        84          85 0.988 98.82%    
#> 2 Peds          64          67 0.955 95.52%    
#> 3 All          148         152 0.974 97.37%

# by region

respiratory_01_test_clean %>% 
  respiratory_01(
    incident_date_col = INCIDENT_DATE,
    patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
    eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
    esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_E_SITUATION_11,
    esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_CODE_LIST_E_SITUATION_12,
    evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
    evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14,
    .by = region
  ) %>% 
  print(n = Inf)
  
#> # A tibble: 24 × 6
#>    region pop    numerator denominator  prop prop_label
#>    <chr>  <chr>      <dbl>       <int> <dbl> <chr>     
#>  1 1A     Adults        16          16 1     100%      
#>  2 1C     Adults         6           7 0.857 85.71%    
#>  3 2      Adults         9           9 1     100%      
#>  4 3      Adults        10          10 1     100%      
#>  5 4      Adults        10          10 1     100%      
#>  6 5      Adults        13          13 1     100%      
#>  7 6      Adults        10          10 1     100%      
#>  8 7      Adults        10          10 1     100%      
#>  9 1A     Peds           7           8 0.875 87.5%     
#> 10 1C     Peds          12          13 0.923 92.31%    
#> 11 2      Peds          10          10 1     100%      
#> 12 3      Peds           3           3 1     100%      
#> 13 4      Peds           6           7 0.857 85.71%    
#> 14 5      Peds          10          10 1     100%      
#> 15 6      Peds           8           8 1     100%      
#> 16 7      Peds           8           8 1     100%      
#> 17 1A     All           23          24 0.958 95.83%    
#> 18 1C     All           18          20 0.9   90%       
#> 19 2      All           19          19 1     100%      
#> 20 3      All           13          13 1     100%      
#> 21 4      All           16          17 0.941 94.12%    
#> 22 5      All           23          23 1     100%      
#> 23 6      All           18          18 1     100%      
#> 24 7      All           18          18 1     100%
```

<sup>Created on 2024-11-01 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

Original code credit goes to the illustrious Alyssa Whim.
