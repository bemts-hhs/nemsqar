# `asthma_01`

## Description
Calculates key statistics related to asthma-related incidents in an EMS dataset, specifically focusing on cases where 911 was called for respiratory distress, and certain medications were administered. This function segments the data by age into adult and pediatric populations, computing the proportion of cases that received beta-agonist treatment.

## Usage

```r
asthma_01(df,
          incident_date_col,
          patient_DOB_col,
          eresponse_05_col,
          esituation_11_col,
          esituation_12_col,
          emedications_03_col,
          ...)
```

Arguments
* `df`: A data.frame or tibble containing EMS data where each row represents an observation, and columns represent features.
* `incident_date_col`: Column containing the incident dates.
* `patient_DOB_col`: Column containing patient dates of birth.
* `eresponse_05_col`: Column indicating the type of response; filters are applied based on specific 911 response codes.
* `esituation_11_col`: Column with primary impression codes (assumed to contain ICD-10 codes).
* `esituation_12_col`: Column with secondary impression codes.
* `emedications_03_col`: Column indicating medications administered; filters check for the presence of beta-agonist medications.
* `...`: Additional arguments passed to summarize() for extending the summary output as needed.

# Output
A data.frame summarizing results for three population groups (All, Adults, and Peds) with the following columns:

* `pop`: Population type (All, Adults, or Peds).
* `numerator`: Count of incidents where beta-agonist medications were administered.
* `denominator`: Total count of incidents.
* `prop`: Proportion of incidents involving beta-agonist medications.
* `prop_label`: Proportion formatted as a percentage with a specified number of decimal places.

# Features
* Filters for asthma-related incidents (ICD-10 codes starting with `J45` and `J98.01`).
* Distinguishes between adults (age ≥ 18) and pediatric patients (age 2–17).
* Calculates age in years based on incident date and patient date of birth.
* Formats proportions as percentages with customizable decimal precision.

# Value
A summarized data.frame with counts and proportions of beta-agonist treatment among 911 respiratory distress calls, segmented by population group.

# Example

``` r
################################################################################
### Code to test Asthma-01 #####################################################
################################################################################

# load packages
  
  library(tidyverse)
  library(janitor)
  library(scales)
  library(rlang)
  
# load data
asthma_01_data <- read_csv("asthma01_Export.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 458787 Columns: 10
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (10): Incident Patient Care Report Number - PCR (eRecord.01), Incident D...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean
asthma_01_clean <- asthma_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~  mdy(str_remove_all(., pattern = "\\s12:00:00\\sAM")
                                                                                )
                )
         )

# test the function
asthma_01_clean %>% 
  asthma_01(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
            esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
            emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03
            )
            
#> # A tibble: 3 × 6
#>   measure   pop    numerator denominator  prop prop_label
#>   <chr>     <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Asthma-01 Adults       708        1164 0.608 60.82%    
#> 2 Asthma-01 Peds         111         205 0.541 54.15%    
#> 3 Asthma-01 All          825        1406 0.587 58.68%
```

<sup>Created on 2024-11-07 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

# Notes
* Assumes each row in df represents an observation, with columns as features.
* Expects valid Date or POSIXct formats for incident_date_col and patient_DOB_col.
* Assumes eresponse.05 contains 911 response codes in string format, and emedications_03 contains descriptions of administered medications.
* The dataset should be pre-joined if it requires additional variables or information.
* Assumes missing values are represented as NA rather than coded "not values" often found in data collection systems like ImageTrend.

Original code by Nicoals Foss, Ed.D., MS