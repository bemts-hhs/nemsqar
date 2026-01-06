# Synthetic eVitals Data for NEMSIS from the National Emergency Medical Services Information System (NEMSIS)

This dataset contains synthetic data for the eVitals section of the
National Emergency Medical Services Information System (NEMSIS). It is
designed for testing functions within the nemsqar package. The data
structure follows the expected format for eVitals fields but does not
produce meaningful clinical results.

## Usage

``` r
nemsqar_vitals_table
```

## Format

A tibble with 10,000 rows and 19 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique incident identifier (character).

- Incident Date:

  Date of the EMS incident (Date).

- Vitals Signs Taken Date Time (eVitals.01):

  Timestamp of vital signs measurement (Datetime).

- Vitals ECG Type (eVitals.04):

  ECG type recorded (character).

- Vitals Systolic Blood Pressure SBP (eVitals.06):

  Systolic blood pressure (numeric).

- Vitals Diastolic Blood Pressure DBP (eVitals.07):

  Diastolic blood pressure (numeric).

- Vitals Heart Rate (eVitals.10):

  Heart rate in beats per minute (numeric).

- Vitals Pulse Oximetry (eVitals.12):

  Oxygen saturation percentage (numeric).

- Vitals Respiratory Rate (eVitals.14):

  Respiratory rate in breaths per minute (numeric).

- Vitals Respiratory Effort (eVitals.15):

  Observed respiratory effort (character).

- Vitals Carbon Dioxide CO2 (eVitals.16):

  End-tidal CO2 measurement (numeric).

- Vitals Blood Glucose Level (eVitals.18):

  Blood glucose level in mg/dL (numeric).

- Vitals Glasgow Coma Score GCS Motor (eVitals.21):

  GCS motor response score (character).

- Vitals Total Glasgow Coma Score GCS (eVitals.23):

  Total Glasgow Coma Score (numeric).

- Vitals Level Of Responsiveness AVPU (eVitals.26):

  AVPU scale assessment (character).

- Vitals Pain Scale Score (eVitals.27):

  Pain scale score (numeric).

- Vitals Pain Scale Score Range Sort Order (eVitals.27):

  Sort order for pain scale (numeric).

- Vitals Stroke Scale Score (eVitals.29):

  Stroke scale assessment result (character).

- Vitals Stroke Scale Type (eVitals.30):

  Type of stroke scale used (character).

## Details

Users are encouraged to use this dataset to test functions, but the
outputs may be nonsensical, as the data is solely intended to
demonstrate the expected input structure.

## Examples

``` r
data(nemsqar_vitals_table)
dplyr::glimpse(nemsqar_vitals_table)
#> Rows: 10,000
#> Columns: 19
#> $ `Incident Patient Care Report Number - PCR (eRecord.01)` <chr> "NyXFBlJfnm-8…
#> $ `Incident Date`                                          <date> 2023-05-27, …
#> $ `Vitals Signs Taken Date Time (eVitals.01)`              <dttm> 2023-05-27 0…
#> $ `Vitals ECG Type (eVitals.04)`                           <chr> "5 Lead", "Qu…
#> $ `Vitals Systolic Blood Pressure SBP (eVitals.06)`        <dbl> 164, 159, 84,…
#> $ `Vitals Diastolic Blood Pressure DBP (eVitals.07)`       <dbl> 180, 200, 61,…
#> $ `Vitals Heart Rate (eVitals.10)`                         <dbl> 193, 218, 247…
#> $ `Vitals Pulse Oximetry (eVitals.12)`                     <dbl> 91, 76, 18, 3…
#> $ `Vitals Respiratory Rate (eVitals.14)`                   <dbl> 36, 69, 168, …
#> $ `Vitals Respiratory Effort (eVitals.15)`                 <chr> "Rapid", "Apn…
#> $ `Vitals Carbon Dioxide CO2 (eVitals.16)`                 <dbl> 96, 24, 90, 1…
#> $ `Vitals Blood Glucose Level (eVitals.18)`                <dbl> 90, 186, 9, 4…
#> $ `Vitals Glasgow Coma Score GCS Motor (eVitals.21)`       <chr> "Not Recorded…
#> $ `Vitals Total Glasgow Coma Score GCS (eVitals.23)`       <dbl> 7, 8, 6, 5, 1…
#> $ `Vitals Level Of Responsiveness AVPU (eVitals.26)`       <chr> "Not Recorded…
#> $ `Vitals Pain Scale Score (eVitals.27)`                   <dbl> 10, 0, 3, 9, …
#> $ `Vitals Pain Scale Score Range Sort Order (eVitals.27)`  <dbl> 0, 3, 3, 1, 3…
#> $ `Vitals Stroke Scale Score (eVitals.29)`                 <chr> "Not Applicab…
#> $ `Vitals Stroke Scale Type (eVitals.30)`                  <chr> "Not Applicab…
```
