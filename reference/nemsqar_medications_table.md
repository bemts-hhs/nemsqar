# Synthetic Test Data for eMedications Fields in National EMS Information System

This dataset provides completely synthetic test data for evaluating
medication-related functions in the `nemsqar` package. It includes key
variables related to medication administration, timing, route, and
standardized coding. The dataset is designed to assist users in testing
the expected input structure for medication-related measures.

## Usage

``` r
data(nemsqar_medications_table)
```

## Format

A data frame with 10,000 rows and 8 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Medication Administered Date Time (eMedications.01):

  Date and time the medication was administered (datetime).

- Medication Administered Prior To EMS Unit Care (eMedications.02):

  Indicator of whether medication was administered before EMS arrival
  (character).

- Medication Given or Administered Description And RXCUI Code
  (eMedications.03):

  Name of medication administered with its associated RXCUI code
  (character).

- Patient Medication Given or Administered Description And RXCUI Codes
  List (eMedications.03):

  List of all medications administered with RXCUI codes (character).

- Medication Administered Route (eMedications.04):

  Method by which the medication was administered (character).

- Medication Administered Route Code (eMedications.04):

  Standardized code for medication administration route (character).

## Details

Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Examples

``` r
data(nemsqar_medications_table)
head(nemsqar_medications_table)
#> # A tibble: 6 × 8
#>   Incident Patient Care Report Number -…¹ `Incident Date` Medication Administe…²
#>   <chr>                                   <date>          <dttm>                
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      2023-05-27 03:55:00   
#> 2 XTLCINMLTP-8616021114                   2023-10-14      2023-10-14 08:58:00   
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      2023-07-07 10:22:00   
#> 4 MOwVDhriyC-5915613206                   2023-05-13      2023-05-13 04:55:00   
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      2023-11-24 12:08:00   
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      2023-01-12 06:14:00   
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Medication Administered Date Time (eMedications.01)`
#> # ℹ 5 more variables:
#> #   `Medication Administered Prior To EMS Unit Care (eMedications.02)` <chr>,
#> #   `Medication Given or Administered Description And RXCUI Code (eMedications.03)` <chr>,
#> #   `Patient Medication Given or Administered Description And RXCUI Codes List (eMedications.03)` <chr>, …
```
