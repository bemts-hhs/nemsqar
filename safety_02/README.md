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

# Notes

* This function expects `df` to be pre-filtered for any unwanted rows, with required columns formatted correctly (e.g., dates in `Date`/`POSIXct` classes).