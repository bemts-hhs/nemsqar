# `safety_04`

# Description
The `safety_04` function processes EMS incident data for specific safety and transport criteria, filtering by patient age and incident type to identify cases that meet specified exclusion or inclusion criteria. This function accommodates data with various EMS-specific codes, age descriptors, and procedure identifiers.

# Usage

```r
safety_04(df, incident_date_col, patient_DOB_col, epatient_15_col,
          epatient_16_col, eresponse_05_col, earrest_01_col, einjury_03_col,
          eprocedures_03_col, edisposition_14_col, transport_disposition_cols, ...)
```

# Arguments

* `df`: A data frame or tibble containing EMS data where each row represents an individual observation.
* `incident_date_col`: Column name containing the incident dates, expected to be of Date or POSIXct class.
* `patient_DOB_col`: Column name containing patient birth dates, expected to be of Date or POSIXct class.
* `epatient_15_col`: Column name indicating the patient age.
* `epatient_16_col`: Column name for the unit of age (e.g., “Years,” “Months”).
* `eresponse_05_col`: Column containing response transport codes.
* `earrest_01_col`: Column with cardiac arrest status information.
* `einjury_03_col`: Column describing traumatic injuries, expected as a list or text-separated entries.
* `eprocedures_03_col`: Column listing procedures, assumed to contain multiple procedure codes/texts in each cell.
* `edisposition_14_col`: Column for transport dispositions.
* `transport_disposition_cols`: Columns for primary and secondary transport dispositions.
* `...`: Additional arguments for flexibility in function customization.

# Features

* *Age Calculation*: Calculates patient age in years based on incident_date_col and patient_DOB_col.
* *Transport Identification*: Flags records for EMS transport and interfacility transports based on provided codes.
* *Cardiac Arrest & Trauma Triage Filtering*: Identifies cases with severe trauma indicators or cardiac arrest.
* *Procedure Exclusion*: Applies exclusion criteria based on specific airway and immobilization procedures.
* *Minor Age Check*: Separates pediatric cases (<8 years) by EMS-reported or calculated age.

# Output
A filtered data frame of pediatric cases meeting specific EMS criteria, including columns for calculated age, transport, trauma, and procedure flags.

# Value
Returns a data frame subset of pediatric cases (<8 years) meeting the specified EMS transport and safety criteria.

# Example

``` r
################################################################################
### Test for Safety-04 #########################################################
################################################################################

# packages
  
library(tidyverse)
library(janitor)
library(scales)
library(rlang)

# load data

safety_04_data <- read_csv("safety04_Export_2023.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")
  
#> Rows: 458909 Columns: 27
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (23): Incident Patient Care Report Number - PCR (eRecord.01), Agency Nam...
#> dbl  (3): Agency Unique State ID (dAgency.01), Agency Number (dAgency.02), P...
#> lgl  (1): Agency Is Demo Service
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# clean

safety_04_clean <- safety_04_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s\\d+:\\d+(:\\d+)?(\\s(AM|PM))?")
  )))

# run function

safety_04_clean %>% 
  safety_04(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            epatient_15_col = PATIENT_AGE_E_PATIENT_15,
            epatient_16_col = PATIENT_AGE_UNITS_E_PATIENT_16,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            earrest_01_col = CARDIAC_ARREST_DURING_EMS_EVENT_WITH_CODE_E_ARREST_01,
            einjury_03_col = INJURY_TRAUMA_CENTER_TRIAGE_CRITERIA_STEPS_1_AND_2_LIST_E_INJURY_03,
            eprocedures_03_col = PATIENT_ATTEMPTED_PROCEDURE_DESCRIPTIONS_AND_CODES_LIST_E_PROCEDURES_03,
            edisposition_14_col = DISPOSITION_POSITION_OF_PATIENT_DURING_TRANSPORT_LIST_E_DISPOSITION_14,
            transport_disposition_cols = c(DISPOSITION_INCIDENT_PATIENT_DISPOSITION_WITH_CODE_3_4_E_DISPOSITION_12_3_5_IT_DISPOSITION_112, TRANSPORT_DISPOSITION_3_4_IT_DISPOSITION_102_3_5_E_DISPOSITION_30)
            )
            
#> # A tibble: 1 × 6
#>   measure   pop   numerator denominator   prop prop_label
#>   <chr>     <chr>     <dbl>       <int>  <dbl> <chr>     
#> 1 Safety-04 Peds        614        7697 0.0798 7.98%
```

<sup>Created on 2024-11-11 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>


# Notes

* Requires tidyverse, scales, and rlang packages.
* Assumes missing values (NA) are handled by previous data cleaning steps.
* Designed to accommodate ImageTrend data formatting and NEMSIS 3.5 transport codes.