# Download and read a UKB welf-reported medication code to ATC mapping file

Mapping table obtained from [Wray et al
2019](https://www.nature.com/articles/s41467-019-09572-5#Sec23),
Supplementary Data 1.

## Usage

``` r
get_ukb_self_report_med_to_atc_map(
  path = file.path(tempdir(), "self_report_med_to_atc_map.xlsx")
)
```

## Arguments

- path:

  Path where file will be downloaded to.

## Value

File path to downloaded file.

## Examples

``` r
if (FALSE) { # \dontrun{
get_nhsbsa_snomed_bnf()
} # }
```
