# Synthetic eProcedures Data from the National Emergency Medical Services Information System (NEMSIS)

This dataset provides synthetic procedure-related information from the
eProcedures section of the National Emergency Medical Services
Information System (NEMSIS). It contains example procedure details that
can be used for testing various functions within the `nemsqar` package.
Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Usage

``` r
nemsqar_procedures_table
```

## Format

A tibble with 10,000 rows and 8 variables:

- `Incident Patient Care Report Number - PCR (eRecord.01)`:

  (chr) Unique identifier for the patient care report.

- `Incident Date`:

  (date) The date of the EMS incident.

- `Procedure Performed Date Time (eProcedures.01)`:

  (dttm) The date and time the procedure was performed.

- `Procedure Performed Prior To EMS Care (eProcedures.02)`:

  (chr) Indicates whether the procedure was performed before EMS
  arrival.

- `Procedure Performed Description And Code (eProcedures.03)`:

  (chr) Description and code of the performed procedure.

- `Patient Attempted Procedure Descriptions And Codes List (eProcedures.03)`:

  (chr) List of attempted procedures with descriptions and codes.

- `Procedure Number Of Attempts (eProcedures.05)`:

  (dbl) Number of attempts made to perform the procedure.

- `Procedure Successful (eProcedures.06)`:

  (chr) Indicates whether the procedure was successful.

## Details

The data in this table are entirely synthetic and intended solely for
testing purposes. These data do not represent real patients, incidents,
or outcomes and should not be used for research or operational
decision-making.

## Examples

``` r
data(nemsqar_procedures_table)
dplyr::glimpse(nemsqar_procedures_table)
#> Rows: 10,000
#> Columns: 8
#> $ `Incident Patient Care Report Number - PCR (eRecord.01)`                   <chr> …
#> $ `Incident Date`                                                            <date> …
#> $ `Procedure Performed Date Time (eProcedures.01)`                           <dttm> …
#> $ `Procedure Performed Prior To EMS Care (eProcedures.02)`                   <chr> …
#> $ `Procedure Performed Description And Code (eProcedures.03)`                <chr> …
#> $ `Patient Attempted Procedure Descriptions And Codes List (eProcedures.03)` <chr> …
#> $ `Procedure Number Of Attempts (eProcedures.05)`                            <dbl> …
#> $ `Procedure Successful (eProcedures.06)`                                    <chr> …
```
