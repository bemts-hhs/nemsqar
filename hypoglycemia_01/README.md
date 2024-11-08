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
  epatient_15_col,
  epatient_16_col,
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
* `epatient_15_col`: Column representing the patient's numeric age agnostic of unit.
* `epatient_16_col`: Column representing the patient's age unit (i.e. year, month, day, hour, minute).
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
* For filter robustness, the function will check against auto-calculated and the function's calculated age to increase likelihood of age capture.
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

hypoglycemia_01_data <- read_csv("hypoglycemia01_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 677614 Columns: 30
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (22): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (5): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02), P...
#> lgl  (3): Vitals Low Blood Glucose Level Flag (eVitals.18), Vitals Low Total...
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
                  epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                  epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                  evitals_18_col = PATIENT_LOW_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                  evitals_23_cl = PATIENT_LOW_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                  evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                  emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
                  eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03
                  )
                  
#> # A tibble: 3 × 6
#>   measure         pop    numerator denominator  prop prop_label
#>   <chr>           <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Hypoglycemia-01 Adults      1362        2424 0.562 56.19%    
#> 2 Hypoglycemia-01 Peds          19          31 0.613 61.29%    
#> 3 Hypoglycemia-01 All         1380        2455 0.562 56.21%
```

<sup>Created on 2024-11-07 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

# Notes

* Data Assumptions: The input data should be a data frame or tibble. Each column passed to arguments should conform to expected formats, particularly for dates.
* Age Calculation: Assumes incident and birth dates are available to compute age in years for age group classification. The function will also look at provided age / age unit values from the database as well.
* Missing Values: Assumes that rows with missing values are NA, not coded as "not known" or "not recorded."
* Impression Codes: Assumes that primary/secondary impression fields contain ICD-10 codes. Text descriptions can be present but are not used in filtering.
* Medication/Procedures Data: Assumes all administered medications/procedures attempted are listed in emedications_03_col and eprocedures_03 as either list columns or comma-separated values.





