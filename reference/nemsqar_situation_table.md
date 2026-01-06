# Synthetic Test Data for eSituation Fields in National EMS Information System

This dataset provides completely synthetic test data for evaluating
situation-related functions in the `nemsqar` package. It includes
variables related to patient complaints, symptoms, injury status, and
provider impressions. The dataset is designed to assist users in testing
the expected input structure for situation-related measures.

## Usage

``` r
data(nemsqar_situation_table)
```

## Format

A data frame with 10,000 rows and 18 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Situation Symptom Onset Date Time (eSituation.01):

  Date and time when symptoms began (datetime).

- Situation Possible Injury With Code (eSituation.02):

  Indicates whether an injury is possible, including coded response
  (character).

- Situation Complaint Type (eSituation.03):

  Classification of the patient's complaint (character).

- Situation Complaint Statement (eSituation.04):

  Primary complaint reported by the patient (character).

- Situation Primary Complaint Statement List (eSituation.04):

  List of primary complaints (character).

- Situation Complaint Duration (eSituation.05):

  Duration of the complaint (numeric).

- Situation Complaint Duration Time Units (eSituation.06):

  Units of time associated with the complaint duration (character).

- Situation Chief Complaint Anatomic Location (eSituation.07):

  Anatomic location of the primary complaint (character).

- Situation Chief Complaint Organ System (eSituation.08):

  Organ system affected by the chief complaint (character).

- Situation Primary Symptom (eSituation.09):

  Primary symptom reported by the patient, including ICD code
  (character).

- Situation Other Associated Symptom Description (eSituation.10):

  Description of additional symptoms (character).

- Situation Other Associated Symptom ICD Code (eSituation.10):

  ICD code for associated symptoms (character).

- Situation Other Associated Symptoms List (eSituation.10):

  List of additional symptoms reported (character).

- Situation Provider Primary Impression Code And Description
  (eSituation.11):

  Primary impression of the provider, including ICD code (character).

- Situation Provider Secondary Impression Description And Code
  (eSituation.12):

  Secondary provider impression, including ICD code (character).

- Situation Provider Secondary Impression Description And Code List
  (eSituation.12):

  List of secondary provider impressions (character).

## Details

Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Examples

``` r
data(nemsqar_situation_table)
head(nemsqar_situation_table)
#> # A tibble: 6 × 18
#>   Incident Patient Care Report Number -…¹ `Incident Date` Situation Symptom On…²
#>   <chr>                                   <date>          <dttm>                
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      1720-05-24 03:52:00   
#> 2 XTLCINMLTP-8616021114                   2023-10-14      2023-08-12 03:30:00   
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      2023-07-06 02:00:51   
#> 4 MOwVDhriyC-5915613206                   2023-05-13      1953-05-12 05:28:00   
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      2023-09-20 06:56:00   
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      2020-08-12 08:51:28   
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Situation Symptom Onset Date Time (eSituation.01)`
#> # ℹ 15 more variables:
#> #   `Situation Possible Injury With Code (eSituation.02)` <chr>,
#> #   `Situation Complaint Type (eSituation.03)` <chr>,
#> #   `Situation Complaint Statement (eSituation.04)` <chr>, …
```
