# `respiratory_01`

## Description
The `respiratory_01` function filters and analyzes data related to emergency 911 respiratory distress incidents, providing summary statistics for adult and pediatric populations. This function uses specific data columns for 911 response codes, primary and secondary impressions, and vital signs to calculate the proportion of cases with complete vital signs recorded, stratified by age.

## Usage
```R
respiratory_01(df, incident_date_col, patient_DOB_col, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col, ...)
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
library(rlang)
library(scales)
  
# load data

  respiratory_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory01_Export.csv") %>% 
    clean_names(case = "screaming_snake", sep_out = "_")
    
#> Rows: 458773 Columns: 11
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (9): Incident Patient Care Report Number - PCR (eRecord.01), Incident Da...
#> dbl (2): Patient Initial Pulse Oximetry (eVitals.12), Patient Initial Respir...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

respiratory_01_clean <- respiratory_01_data %>%
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
  )))

# test the function

respiratory_01_clean %>% 
respiratory_01(
  incident_date_col = INCIDENT_DATE,
  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
  evitals_12_col = PATIENT_INITIAL_PULSE_OXIMETRY_E_VITALS_12,
  evitals_14_col = PATIENT_INITIAL_RESPIRATORY_RATE_E_VITALS_14
)
  
#> # A tibble: 3 × 6
#>   measure        pop    numerator denominator  prop prop_label
#>   <chr>          <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Respiratory-01 Adults     24897       27430 0.908 90.77%    
#> 2 Respiratory-01 Peds        1529        1844 0.829 82.92%    
#> 3 Respiratory-01 All        26527       29637 0.895 89.51%
```

<sup>Created on 2024-11-01 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

Original code credit goes to the illustrious Alyssa Whim.
