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
* `eresponse_05_col`: Unquoted column name with response codes, identifying 911 responses.
* `edisposition_18_col`: Unquoted column name with transport mode descriptors, including possible lights-and-sirens indicators.
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
  
#> Rows: 458694 Columns: 24
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (20): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (2): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02)
#> lgl  (2): Transport Disposition Code (3.4=itDisposition.102/3.5=eDisposition...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

safety_02_clean <- safety_02_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
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
#> 1 Safety-02 Adults    159279      183445 0.868 86.83%    
#> 2 Safety-02 Peds        7672        8994 0.853 85.3%     
#> 3 Safety-02 All       167188      192789 0.867 86.72%
```

<sup>Created on 2024-11-06 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>


# Notes

* This function expects `df` to be pre-filtered for any unwanted rows, with required columns formatted correctly (e.g., dates in `Date`/`POSIXct` classes).