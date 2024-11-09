# `safety_01`

# Description

The `safety_01` function calculates the proportion of 911 responses where "lights and sirens" were not used in an EMS dataset. It generates age-based population summaries, calculating the count and proportion of "lights and sirens" responses among all incidents, and within adult and pediatric groups.

# Usage

```r
safety_01(df, incident_date_col, patient_DOB_col, eresponse_05_col, eresponse_24_col, ...)
```

# Arguments

* `df`: A data frame or tibble containing EMS data.
* `incident_date_col`: Column indicating the date of the incident (of type `Date` or `POSIXct`).
* `patient_DOB_col`: Column for the patient’s date of birth (of type `Date` or `POSIXct`).
* `eresponse_05_col`: Column containing response mode codes (e.g., 911 response codes).
* `eresponse_24_col`: Column detailing additional response descriptors as text.
* `...`: Additional arguments passed to the `dplyr::summarize` function.

# Details
This function assumes that:

* The EMS dataset uses a row-per-observation format.
* Missing values are NA.
* The column eresponse.05 contains standardized response codes.
* The eresponse.24 column contains a list of textual descriptors.

# Features
* Age Calculation: Computes patient age in years by subtracting patient_DOB_col from incident_date_col.
* Filter for 911 Responses: Filters records based on response codes in eresponse.05.
* Lights and Sirens Filter: Assigns a value of 1 if "lights and sirens" are not used, and 0 otherwise.
* Population Grouping: Separates data into adult (age ≥ 18) and pediatric (age < 18) populations.

# Output
A tibble with population summaries:

* `measure`: "Safety-01" (measure identifier)
* `pop`: Population group (All, Adults, or Peds)
* `numerator`: Count of incidents without lights and sirens.
* `denominator`: Total incident count.
* `prop`: Proportion of incidents without lights and sirens.
* `prop_label`: Formatted percentage label for the proportion.

# Value
Returns a tibble summarizing the Safety-01 metric for all populations, adult, and pediatric groups.

# Example

``` r
################################################################################
### Test for Safety-01 #########################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)
  
# load data

safety_01_data <- read_csv("safety01_02_Export_2023.csv") %>% 
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

safety_01_clean <- safety_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# run function

safety_01_clean %>% 
  safety_01(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            epatient_15_col = PATIENT_AGE_E_PATIENT_15,
            epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            eresponse_24_col = RESPONSE_ADDITIONAL_RESPONSE_MODE_DESCRIPTORS_LIST_E_RESPONSE_24
            )
#> # A tibble: 3 × 6
#>   measure   pop    numerator denominator  prop prop_label
#>   <chr>     <chr>      <dbl>       <int> <dbl> <chr>     
#> 1 Safety-01 Adults     93043      312991 0.297 29.73%    
#> 2 Safety-01 Peds        3892       15902 0.245 24.47%    
#> 3 Safety-01 All       111571      373282 0.299 29.89%
```

<sup>Created on 2024-11-08 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>


# Notes

* Ensure incident_date_col and patient_DOB_col columns are of `Date` or `POSIXct` type.
* Grouping the data by region or other identifiers can be done inside the function to calculate results for subsets.