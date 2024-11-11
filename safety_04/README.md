# `safety_02` Function

# Description
The `safety_02` function calculates the Safety-02 metric, evaluating the proportion of emergency medical calls involving transport where no lights and sirens were used. This function categorizes the population into adult and pediatric groups based on their age, and summarizes results with a total population count as well.

# Usage

```r
safety_02(df, incident_date_col, patient_DOB_col, eresponse_05_col, edisposition_18_col, transport_disposition_cols, ...)
```

# Arguments

* `df`: A data frame where each row is an observation, and each column represents a feature.
* `incident_date_col`: Unquoted column name representing the date of the incident.
* `patient_DOB_col`: Unquoted column name for the patient's date of birth.
* `epatient_15_col`: Column giving the calculated age value.
* `epatient_16_col`: Column giving the provided age unit value.
* `eresponse_05_col`: Column giving response codes, identifying 911 responses.
* `edisposition_18_col`: Column giving transport mode descriptors, including possible lights-and-sirens indicators.
* `edisposition_28_col`: Column giving patient evaluation and care categories for the EMS response.
* `transport_disposition_cols`: One or more unquoted column names (such as edisposition.12, edisposition.30) containing transport disposition details.
* `...`: Additional arguments for summary calculation, if needed.

# Output
A data frame summarizing the Safety-02 metric with calculated values for three populations:

* All patients
* Adults (≥18 years)
* Pediatrics (<18 years)

# Features

* *Population Segmentation*: Filters data for 911 responses involving transport, categorized by age groups (adult and pediatric).
* *Transport Check*: Evaluates transport-related codes and uses a boolean flag (`transport`) to indicate valid transport.
* *Lights and Sirens Check*: Counts cases where lights and sirens were not used.
* *Proportion Calculation*: Computes the proportion of cases without lights and sirens in each population segment, formatted as a percentage.

# Value

A data frame with columns:

* `measure`: Metric name ("Safety-02").
* `pop`: Population segment (`All`, `Adults`, `Peds`).
* `numerator`: Count of cases where no lights and sirens were used.
* `denominator`: Total cases in the population.
* `prop`: Proportion of cases without lights and sirens.
* `prop_label`: Formatted percentage of the proportion.

# Example

``` r
################################################################################
### Test for Safety-02 #########################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# load data

safety_02_data <- read_csv("safety01_02_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 458696 Columns: 28
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (22): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (3): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02), P...
#> lgl  (3): Patient Evaluation/Care Code (3.4=itDisposition.100/3.5=eDispositi...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

safety_02_clean <- safety_02_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# run function

safety_02_clean %>% 
  safety_02(incident_date_col = INCIDENT_DATE,
            PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            DISPOSITION_ADDITIONAL_TRANSPORT_MODE_DESCRIPTOR_LIST_E_DISPOSITION_18,
            transport_disposition_cols = c(DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112, 
                                           TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30)
            )
            
#> # A tibble: 3 × 6
#>   measure   pop    numerator denominator  prop prop_label
#>   <chr>     <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Safety-02 Adults     21190       34369 0.617 61.65%    
#> 2 Safety-02 Peds         808        1479 0.546 54.63%    
#> 3 Safety-02 All        22173       36228 0.612 61.2%
```

<sup>Created on 2024-11-08 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>


# Notes

* This function expects `df` to be pre-filtered for any unwanted rows, with required columns formatted correctly (e.g., dates in `Date`/`POSIXct` classes).