# Download and read the NHSBSA BNF_SNOMED mapping file

Mapping table available from
[here](https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping).

## Usage

``` r
get_nhsbsa_snomed_bnf(path = file.path(tempdir(), "bnf_dmd.zip"))
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
