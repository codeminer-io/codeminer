# Download the Phecode 1.2 to ICD10 (beta) mapping file

Download link obtained from https://phewascatalog.org/phecodes.

## Usage

``` r
get_phecode_icd10_map(
  path = file.path(tempdir(), "Phecode_map_v1_2_icd10_beta.csv.zip")
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
get_phecode_icd10_map()
} # }
```
