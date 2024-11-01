# `respiratory_02`

# Description
The `respiratory_02` function calculates metrics for pediatric and adult respiratory populations based on pre-defined criteria, such as low oxygen saturation and specific medication or procedure codes. It returns a summary table of the overall, pediatric, and adult populations, showing counts and proportions.

# Usage

```r
respiratory_02(df, erecord_01_col, incident_date_col, patient_DOB_col, eresponse_05_col, evitals_12_col, emedications_03_col, eprocedures_03_col)
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

# Value
Returns a tibble summarizing the overall and age-grouped respiratory-02 metrics, formatted for ease of interpretation.

# Example

# Notes
* Data Preparation: Ensure that `emedications.03` and `eprocedures.03` columns contain comma-separated codes for each incident, not list columns.
* Required Fields: Expected columns include `eRecord.01`, `eresponse.05`, `evitals.12`, `emedications.03`, `eprocedures.03`.  `patient_age_in_years` is calculated under the hood.
* Formatting: Oxygen saturation (`evitals.12`) values can be provided for each incident, as they are later concatenated by a calculated `Unique_ID` (ePCR # + incident date + patient DOB as a string separated by "-").
* Missing Values: Missing data should appear as `NA` rather than codes representing `"not known"` or `"not recorded"` values.
* Grouping: Grouping can be applied before calling the function, e.g., by region, to get results at the desired grouping level use `dplyr::group_by()` in a `dplyr` chain.