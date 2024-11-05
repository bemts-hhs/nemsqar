# `hypoglycemia_01`

# Description

The `hypoglycemia_01` function calculates metrics for hypoglycemia incidents, focusing on identifying and evaluating emergency response data related to 911 calls involving altered mental status and hypoglycemia. The function filters, calculates patient ages, and evaluates the presence of hypoglycemia treatments in the administered medications.

# Usage

```r
hypoglycemia_01(
  df,
  erecord_01_col,
  incident_date_col,
  patient_DOB_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_18_col,
  evitals_23_cl,
  evitals_26_col,
  emedications_03_col,
  ...
)
```

# Arguments

* `df`: A data frame or tibble containing emergency response records.
* `erecord_01_col`: Column representing the unique record identifier.
* `incident_date_col`: Column representing the date of the incident.
* `patient_DOB_col`: Column representing the patient's date of birth.
* `eresponse_05_col`: Column containing response type codes.
* `esituation_11_col`: Column for primary impression fields, containing ICD-10 codes.
* `esituation_12_col`: Column for secondary impression fields, containing ICD-10 codes.
* `evitals_18_col`: Column for blood glucose levels.
* `evitals_23_cl`: Column for Glasgow Coma Scale (GCS) scores.
* `evitals_26_col`: Column for AVPU alertness levels.
* `emedications_03_col`: Column for administered medications.
* `...`: Additional arguments for summarization, passed to the summarize function.

# Output
A tibble summarizing the initial population and filtered subgroups (adult, pediatric, total) with hypoglycemia events. It includes:

* `measure`: The measure name for reference in output contexts with multiple measures together.
* `pop`: Population group (All, Adults, Peds).
* `numerator`: Count of cases receiving appropriate treatment.
* `denominator`: Total cases in each population group.
* `prop`: Proportion of cases receiving appropriate treatment.
* `prop_label`: Proportion formatted as a percentage.

# Features

* Dynamically checks for and installs required packages if missing.
* Calculates age in days and years for population classification.
* Uses regex filters to identify specific 911 response codes and impressions related to hypoglycemia and altered mental status.
* Applies quasiquotation for date variables, enforcing date class validation.

# Value
A tibble with hypoglycemia treatment statistics across the full, adult, and pediatric populations.

# Example

``` r
################################################################################
### Test for Hypoglycemia-01 ###################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# load data

hypoglycemia_01_data <- read_csv("hypoglycemia01_Export.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 679139 Columns: 13
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (11): Incident Patient Care Report Number - PCR (eRecord.01), Incident D...
#> dbl  (2): Vitals Blood Glucose Level (eVitals.18), Vitals Total Glasgow Coma...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean data

hypoglycemia_01_clean <- hypoglycemia_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~  mdy(str_remove_all(., pattern = "\\s12:00:00\\sAM"))
                )
         )

# run the function

hypoglycemia_01_clean %>% 
  hypoglycemia_01(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                  incident_date_col = INCIDENT_DATE,
                  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                  evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                  evitals_23_cl = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                  evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                  emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03
                  )
                  
#> # A tibble: 3 × 6
#>   measure         pop    numerator denominator  prop prop_label
#>   <chr>           <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Hypoglycemia-01 Adults      1266        2076 0.610 60.98%    
#> 2 Hypoglycemia-01 Peds          14          25 0.56  56%       
#> 3 Hypoglycemia-01 All         1280        2101 0.609 60.92%
```

<sup>Created on 2024-11-05 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

# Notes

* Data Assumptions: The input data should be a data frame or tibble. Each column passed to arguments should conform to expected formats, particularly for dates.
* Age Calculation: Assumes incident and birth dates are available to compute age in years for age group classification.
* Missing Values: Assumes that rows with missing values are NA, not coded as "not known" or "not recorded."
* Impression Codes: Assumes that primary/secondary impression fields contain ICD-10 codes. Text descriptions can be present but are not used in filtering.
* Medication Data: Assumes all administered medications are listed in emedications_03_col as either a list column or comma-separated values.





