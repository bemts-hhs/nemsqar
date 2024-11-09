# `respiratory_02`

# Description
The `respiratory_02` function calculates metrics for pediatric and adult respiratory populations based on pre-defined criteria, such as low oxygen saturation and specific medication or procedure codes. It returns a summary table of the overall, pediatric, and adult populations, showing counts and proportions.

# Usage

```r
respiratory_02(df, erecord_01_col, incident_date_col, patient_DOB_col, eresponse_05_col, evitals_12_col, emedications_03_col, eprocedures_03_col, ...)
```

# Arguments

* `df`: Data frame or tibble containing EMS incident data.
* `erecord_01_col`: Column name for eRecord.01, used to form a unique patient ID.
* `incident_date_col`: Column name for the incident date.
* `patient_DOB_col`: Column name for the patient's date of birth.
* `epatient_15_col`: Column giving the calculated age value.
* `epatient_16_col`: Column giving the provided age unit value.
* `eresponse_05_col`: Column name for response codes (e.g., incident type).
* `evitals_12_col`: Column name for oxygen saturation (SpO2) values.
* `emedications_03_col`: Column name for medication codes.
* `eprocedures_03_col`: Column name for procedure codes.
* `...`: arguments passed to `dplyr::summarize()`.

# Output
A tibble with two rows (`pediatric`, and `adult`) showing:

* `pop`: Population type ("Peds", "Adults", "All").
* `numerator`: Count of patients meeting the criteria.
* `denominator`: Total count of patients in the group.
* `prop`: Proportion of patients meeting the criteria.
* `prop_label`: Formatted proportion as a percentage.

# Features

* Dynamically checks and installs required packages.
* Validates input data types and expected column names.
* Calculates age in days and years for filtering.
* Generates a unique ID for each incident based on multiple columns.
* Handles grouping to create pediatric and adult populations.
* Grouping handled via tidy dots (`...`) passed to the `dplyr::summarize()` function.

# Value
Returns a tibble summarizing the overall and age-grouped respiratory-02 metrics, formatted for ease of interpretation.

# Example

``` r
################################################################################
### Test for Respiratory-02 ####################################################
################################################################################

# load needed packages

library(tidyverse)
library(janitor)
library(scales)
library(rlang)

# load data

respiratory_02_data <- read_csv("respiratory02_Export_2023.csv") %>% 
    clean_names(case = "screaming_snake", sep_out = "_")
    
#> Rows: 720096 Columns: 25
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (19): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (4): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02), P...
#> lgl  (2): Vitals Low Pulse Oximetry Flag (eVitals.12), Agency Is Demo Service
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

respiratory_02_clean <- respiratory_02_data %>% 
    mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
        str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
    )))

# test the function

respiratory_02_clean %>% 
  respiratory_02(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                 incident_date_col = INCIDENT_DATE,
                 patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                 epatient_15_col = PATIENT_AGE_E_PATIENT_15,
                 epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
                 eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                 evitals_12_col = PATIENT_LOW_PULSE_OXIMETRY_E_VITALS_12,
                 emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
                 eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03
                 )
                 
#> # A tibble: 2 × 6
#>   measure        pop    numerator denominator  prop prop_label
#>   <chr>          <chr>      <int>       <int> <dbl> <chr>     
#> 1 Respiratory-02 Peds         261         710 0.368 36.76%    
#> 2 Respiratory-02 Adults     11716       25718 0.456 45.56%
```

<sup>Created on 2024-11-08 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

# Notes
* Data Preparation: Ensure that `emedications.03` and `eprocedures.03` columns contain comma-separated codes for each incident, not list columns.
* Required Fields: Expected columns include `eRecord.01`, `eresponse.05`, `evitals.12`, `emedications.03`, `eprocedures.03`.  `patient_age_in_years` is calculated under the hood.
* Formatting: Oxygen saturation (`evitals.12`) values can be provided for each incident, as they are later concatenated by a calculated `Unique_ID` (ePCR # + incident date + patient DOB as a string separated by "-").
* Missing Values: Missing data should appear as `NA` rather than codes representing `"not known"` or `"not recorded"` values.
* Grouping: Grouping can be applied before calling the function, e.g., by region, to get results at the desired grouping level use `dplyr::group_by()` in a `dplyr` chain.

Original code by Nicolas Foss, Ed.D., MS
