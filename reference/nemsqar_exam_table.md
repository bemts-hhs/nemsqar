# Synthetic Test Data for eExam Fields in National EMS Information System

This dataset provides completely synthetic test data for evaluating
patient examination-related functions in the `nemsqar` package. It
includes key variables related to patient weight, assessment findings,
lung and chest examinations, and neurological assessments. The dataset
is intended to assist users in testing the expected input structure for
examination-related measures.

## Usage

``` r
data(nemsqar_exam_table)
```

## Format

A data frame with 10,000 rows and 11 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Patient Weight In Kilograms (eExam.01):

  Patient's weight in kilograms (numeric).

- Patient Length Based Color (eExam.02):

  Color coding based on patient length for pediatric patients
  (character).

- Patient Assessment Date Time (eExam.03):

  Timestamp for patient assessment (datetime).

- Patient Extremity Assessment Findings List (eExam.16):

  List of findings related to extremity assessment (character).

- Patient Neurological Assessment Findings List (eExam.20):

  List of neurological assessment findings (character).

- Patient Lung Assessment Finding Location
  (3.4=itExam.099/3.5=eExam.22):

  Location of lung assessment findings (character).

- Patient Lung Assessment Findings List (3.4=itExam.100/3.5=eExam.23):

  List of lung assessment findings (character).

- Patient ChestExclusive Assessment Finding Location
  (3.4=itExam.101/3.5=eExam.24):

  Location of chest-exclusive assessment findings (character).

- Patient ChestExclusive Assessment Findings List
  (3.4=itExam.102/3.5=eExam.25):

  List of chest-exclusive assessment findings (character).

## Details

Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Examples

``` r
data(nemsqar_exam_table)
head(nemsqar_exam_table)
#> # A tibble: 6 × 11
#>   Incident Patient Care Report Number -…¹ `Incident Date` Patient Weight In Ki…²
#>   <chr>                                   <date>                           <dbl>
#> 1 NyXFBlJfnm-8333586176                   2023-05-27                       143. 
#> 2 XTLCINMLTP-8616021114                   2023-10-14                       131. 
#> 3 HfYjlIEQSk-9529756610                   2023-07-07                         8.3
#> 4 MOwVDhriyC-5915613206                   2023-05-13                       121  
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24                        79.9
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12                        80.5
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Patient Weight In Kilograms (eExam.01)`
#> # ℹ 8 more variables: `Patient Length Based Color (eExam.02)` <chr>,
#> #   `Patient Assessment Date Time (eExam.03)` <dttm>,
#> #   `Patient Extremity Assessment Findings List (eExam.16)` <chr>,
#> #   `Patient Neurological Assessment Findings List (eExam.20)` <chr>, …
```
