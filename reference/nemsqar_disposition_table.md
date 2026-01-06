# Synthetic Test Data for eDisposition Fields in National EMS Information System

This dataset provides completely synthetic test data for evaluating
disposition-related functions in the `nemsqar` package. It includes key
variables related to patient disposition, transport mode, hospital
capabilities, and team pre-arrival alerts. The dataset is intended to
assist users in testing the expected input structure for
disposition-related measures.

## Usage

``` r
data(nemsqar_disposition_table)
```

## Format

A data frame with 10,000 rows and 13 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Disposition Position Of Patient During Transport List
  (eDisposition.14):

  Position of the patient during transport (character).

- Disposition Additional Transport Mode Descriptor (eDisposition.18):

  Additional transport mode details (character).

- Disposition Additional Transport Mode Descriptor List
  (eDisposition.18):

  List of additional transport mode descriptors (character).

- Hospital Capability (3.4=itDisposition.105/3.5=eDisposition.23):

  Primary hospital capability associated with the incident (character).

- Hospital Capability List (3.4=itDisposition.105/3.5=eDisposition.23):

  List of hospital capabilities at the destination facility (character).

- Disposition Team Pre Arrival Alert (eDisposition.24):

  Indicates whether a pre-arrival alert was triggered for the team
  (character).

- Disposition Destination Team Pre-arrival Alert Activation
  (eDisposition.24):

  Indicates the specific type of pre-arrival alert activated
  (character).

- Destination Trauma General Team Pre-arrival Activation
  (eDisposition.24):

  Indicates whether a general trauma team was activated pre-arrival
  (logical).

- Patient Evaluation/Care (3.4=itDisposition.100/3.5=eDisposition.28):

  Evaluation or care provided to the patient (character).

- Transport Disposition (3.4=itDisposition.102/3.5=eDisposition.30):

  Final transport disposition of the patient (character).

- Disposition Incident Patient Disposition With Code
  (3.4=eDisposition.12/3.5=itDisposition.112):

  Final disposition of the patient, including standardized codes
  (character).

## Details

Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Examples

``` r
data(nemsqar_disposition_table)
head(nemsqar_disposition_table)
#> # A tibble: 6 × 13
#>   Incident Patient Care Report Number -…¹ `Incident Date` Disposition Position…²
#>   <chr>                                   <date>          <chr>                 
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      Fowlers (Semi-Upright…
#> 2 XTLCINMLTP-8616021114                   2023-10-14      Fowlers (Semi-Upright…
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      Prone,Sitting         
#> 4 MOwVDhriyC-5915613206                   2023-05-13      Semi-Fowlers,Trendele…
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      Lateral Left,Semi-Fow…
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      Fowlers (Semi-Upright…
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Disposition Position Of Patient During Transport List (eDisposition.14)`
#> # ℹ 10 more variables:
#> #   `Disposition Additional Transport Mode Descriptor (eDisposition.18)` <chr>,
#> #   `Disposition Additional Transport Mode Descriptor List (eDisposition.18)` <chr>,
#> #   `Hospital Capability (3.4=itDisposition.105/3.5=eDisposition.23)` <chr>, …
```
