# `pediatrics_03b` Measure Calculation

# Description

The `pediatrics_03b` function calculates a pediatric metric focused on EMS responses, specifically targeting responses that involve patients under 18 years of age, where certain weight-based medications were administered. This function filters EMS data to identify relevant 911 responses and further narrows down the dataset to cases involving children, calculating the proportion of cases with documented weight among those where weight-based medications were administered.

# Usage

```r
pediatrics_03b(df, erecord_01_col, incident_date_col, patient_DOB_col, eresponse_05_col, eexam_01_col, eexam_02_col, emedications_03_col, emedications_04_col, ...)
```
# Arguments
* `df`: Data frame or tibble containing EMS records.
* `erecord_01_col`: Column for unique EMS record identifiers.
* `incident_date_col`: Column indicating the date of the EMS incident.
* `patient_DOB_col`: Column specifying patient date of birth.
* `eresponse_05_col`: Column containing the EMS response codes.
* `eexam_01_col`: Column containing documented weight information.
* `eexam_02_col`: Another column for weight documentation, if applicable.
* `emedications_03_col`: Column indicating medication administration.
* `emedications_04_col`: Column listing medications administered.
* `...`: Additional parameters for the `dplyr::summarize` output.

# Features

* *Automatic Package Loading*: Ensures tidyverse, scales, and rlang are loaded.
* *Error Handling*: Includes custom error messaging for missing or invalid `df` along with the date columns.
* *Patient Age Calculation*: Derives patient age based on incident and birth dates.
* *911 Response Filtering*: Filters data based on 911-specific response codes.
* *Medication Classification*: Identifies non-weight-based medications.
* *Distinct Record Filtering*: Consolidates duplicate records by a unique identifier.

# Output
A summary tibble with the following columns:

* `measure`: Static value "Pediatrics-03b".
* `pop`: Static value "Peds" indicating pediatric population.
* `numerator`: Count of records with documented weight.
* `denominator`: Total number of pediatric records meeting criteria.
* `prop`: Proportion of records with documented weight.
* `prop_label`: Proportion as a formatted percentage.

# Value
Returns a tibble summarizing the proportion of pediatric 911 responses where weight documentation was present and weight-based medications were administered.

# Example

``` r
################################################################################
### Test for Pediatrics-03b ####################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)

# load data

pediatrics_03b_data <- read_csv("pediatrics03b_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 492496 Columns: 23
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (19): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (3): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02), P...
#> lgl  (1): Agency Is Demo Service
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

pediatrics_03b_clean <- pediatrics_03b_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
  )))

# run function

pediatrics_03b_clean %>% 
  pediatrics_03b(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
            incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            eexam_01_col = PATIENT_WEIGHT_IN_KILOGRAMS_E_EXAM_01,
            eexam_02_col = PATIENT_LENGTH_BASED_COLOR_E_EXAM_02,
            emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03,
            emedications_04_col = MEDICATION_ADMINISTERED_ROUTE_E_MEDICATIONS_04
            )
            
#> # A tibble: 1 × 6
#>   measure        pop   numerator denominator  prop prop_label
#>   <chr>          <chr>     <dbl>       <int> <dbl> <chr>     
#> 1 Pediatrics-03b Peds       1751        2187 0.801 80.06%
```

<sup>Created on 2024-11-06 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

# Notes

* This function assumes that `df` includes 911 response codes compatible with the codes_911 patterns.
* Requires date variables to be formatted as `Date` or `POSIXct` classes to ensure correct age calculations.
