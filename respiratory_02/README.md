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
* `eresponse_05_col`: Column name for response codes (e.g., incident type).
* `evitals_12_col`: Column name for oxygen saturation (SpO2) values.
* `emedications_03_col`: Column name for medication codes.
* `eprocedures_03_col`: Column name for procedure codes.
* `...`: arguments passed to `dplyr::summarize()`.

# Output
A tibble with three rows (`pediatric`, `adult`, and `overall`) showing:

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
# load needed packages

library(tidyverse)
library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:purrr':
#> 
#>     discard
#> The following object is masked from 'package:readr':
#> 
#>     col_factor
library(rlang)
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:purrr':
#> 
#>     %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
#>     flatten_raw, invoke, splice

# a function to generate random numbers
generate_random_ID <- function(n, set_seed = 12345) {
  # choose whether or not to set the seed
  
  if (is.numeric(set_seed)) {
    set.seed(set_seed)
    
  } else if (is.null(set_seed)) {
    message(
      "Set.seed was not called internally, and so your results will not be consistent across applications.  Make [set_seed] any number to set the seed and make reproducible results!"
    )
    
  }
  
  random_strings <- vector(mode = "character", length = n)
  for (i in 1:n) {
    random_strings[i] <- paste0(paste0(sample(c(
      LETTERS, letters, LETTERS
    ), size = 10), collapse = ""),
    "-",
    sample(1000000000:9999999999, size = 1))
  }
  return(random_strings)
}

# load data
  
  set.seed(50319)

  respiratory_02_test <- tibble(`Incident Patient Care Report Number - PCR (eRecord.01)` = generate_random_ID(1000, set_seed = 50319),
    `Incident Date` = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                               size = 1000, replace = TRUE),
    `Patient Date Of Birth (ePatient.17)` = sample(seq(as.Date("1980-01-01"), as.Date("2022-12-31"), by = "day"), 
                             size = 1000, replace = TRUE),
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
    `Vitals Pulse Oximetry (eVitals.12)` = sample(0:100, size = 1000, replace = T),
    `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)` = sample(
      c("7806", NA_character_, "125464","214199, 7806", "4917, 1191, 7806", "285059", "214199", "7806, 313002", "214199, 6902", "6387, 7806","7806, 318272","7242, 7242, 26225, 7806, 7806", "435, 7806","4337, 26225","125464, 727374, 727374, 727374", "7806, 313002, 317361, 317361", "7806, 7806","125464, 7806, 237653", "92821, 7806", "7052, 7806, 1191, 26225", "435, 1191, 7806, 7806", "563812", "214199, 435", "125464, 6130", "7806, 1008501", "435", "4917, 285059", "202908, 285059", "435, 6585, 6902, 435", "7806, 435"), size = 1000, replace = T
    ),
    `Patient Attempted Procedure Codes List (eProcedures.03)` = sample(c("46825001, 46825001, 46825001, 46825001, 268400002, 392230005",
"422618004, 422618004, 252465000, 56342008, 33747003, 425543005",
"255560000, 255560000, 23852006, 268400002, 33747003",
NA_character_,
"392230005, 425447009, 7443007, 79821001",
"46825001",
"47545007, 284029005, 47545007, 386053000, 268400002, 428803005",
"392230005, 268400002, 392230005",
"392230005, 392230005, 47545007",
"386053000, 392230005, 428803005, 268400002",
"392230005",
"268400002, 392230005, 392230005, 284029005",
"268400002, 392230005, 392230005",
"392230005, 392230005, 268400002",
"392230005, 392230005, 392230005, 430824005, 392230005, 46825001, 47545007",
"89666000, 89666000, 182692007, 429283006, 7443007, 425447009, 243140006, 233169004",
"46825001, 46825001, 268400002",
"182692007, 392230005, 392230005",
"23852006",
"392230005, 392230005, 386053000, 268400002, 268400002, 268400002, 428803005, 77248004, 284029005, 47545007",
"268400002, 268400002, 268400002, 392230005, 392230005",
"398041008, 392230005, 182555002",
"268400002, 392230005, 428803005, 284029005",
"268400002, 268400002, 268400002",
"23852006, 33747003",
"61746007, 422618004, 404996007, 56469005",
"89666000, 430824005, 427569000, 425447009, 429283006, 232674004, 425543005",
"429283006, 232674004, 386053000, 89666000, 430824005",
"268400002, 392230005",
"386053000, 428803005, 392230005" ), size = 1000, replace = T)
  )
  
# add the region variable for grouping later

respiratory_02_test_prep <- respiratory_02_test %>% 
  mutate(region = sample(c("1A", "1C", "2", "3", "4", "5", "6", "7"), size = nrow(respiratory_02_test), replace = T))

# test the function

respiratory_02_test_prep %>% 
  respiratory_02(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                 incident_date_col = `Incident Date`,
                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                 emedications_03_col = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`,
                 eprocedures_03_col = `Patient Attempted Procedure Codes List (eProcedures.03)`
                 )
                 
#> # A tibble: 3 × 5
#>   pop    numerator denominator  prop prop_label
#>   <chr>      <int>       <int> <dbl> <chr>     
#> 1 All          103         198 0.520 52.02%    
#> 2 Peds          46          83 0.554 55.42%    
#> 3 Adults        57         115 0.496 49.57%

# test with grouping

respiratory_02_test_prep %>% 
  respiratory_02(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                 incident_date_col = `Incident Date`,
                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                 emedications_03_col = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`,
                 eprocedures_03_col = `Patient Attempted Procedure Codes List (eProcedures.03)`,
                 .by = region # tidy dots (...) allow us to pass the .by argument from summarize() to our function
                 ) %>% 
  print(n = Inf)
  
#> # A tibble: 24 × 6
#>    region pop    numerator denominator  prop prop_label
#>    <chr>  <chr>      <int>       <int> <dbl> <chr>     
#>  1 3      All           15          25 0.6   60%       
#>  2 1A     All           14          26 0.538 53.85%    
#>  3 7      All           15          29 0.517 51.72%    
#>  4 4      All            9          22 0.409 40.91%    
#>  5 5      All           16          28 0.571 57.14%    
#>  6 2      All           12          22 0.545 54.55%    
#>  7 1C     All           10          25 0.4   40%       
#>  8 6      All           12          21 0.571 57.14%    
#>  9 3      Peds           9          13 0.692 69.23%    
#> 10 1A     Peds           6           7 0.857 85.71%    
#> 11 7      Peds           6          12 0.5   50%       
#> 12 5      Peds           6          12 0.5   50%       
#> 13 2      Peds           7          11 0.636 63.64%    
#> 14 4      Peds           4          10 0.4   40%       
#> 15 1C     Peds           5          12 0.417 41.67%    
#> 16 6      Peds           3           6 0.5   50%       
#> 17 1A     Adults         8          19 0.421 42.11%    
#> 18 4      Adults         5          12 0.417 41.67%    
#> 19 2      Adults         5          11 0.455 45.45%    
#> 20 7      Adults         9          17 0.529 52.94%    
#> 21 3      Adults         6          12 0.5   50%       
#> 22 6      Adults         9          15 0.6   60%       
#> 23 1C     Adults         5          13 0.385 38.46%    
#> 24 5      Adults        10          16 0.625 62.5%
```

<sup>Created on 2024-11-01 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>



# Notes
* Data Preparation: Ensure that `emedications.03` and `eprocedures.03` columns contain comma-separated codes for each incident, not list columns.
* Required Fields: Expected columns include `eRecord.01`, `eresponse.05`, `evitals.12`, `emedications.03`, `eprocedures.03`.  `patient_age_in_years` is calculated under the hood.
* Formatting: Oxygen saturation (`evitals.12`) values can be provided for each incident, as they are later concatenated by a calculated `Unique_ID` (ePCR # + incident date + patient DOB as a string separated by "-").
* Missing Values: Missing data should appear as `NA` rather than codes representing `"not known"` or `"not recorded"` values.
* Grouping: Grouping can be applied before calling the function, e.g., by region, to get results at the desired grouping level use `dplyr::group_by()` in a `dplyr` chain.

Original code by Nicolas Foss, Ed.D., MS
