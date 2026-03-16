# Read UK Biobank codings file into lookup tables

Reads the UK Biobank codings file (`Codings.tsv`) and returns lookup
tables for OPCS-4 and UKB self-reported condition/medication/operation
code types.

## Usage

``` r
read_ukb_codings(
  path = get_ukb_codings(),
  tables = c("opcs4_lkp", "data_coding_3", "data_coding_4", "data_coding_5",
    "data_coding_6"),
  version = "v0",
  source = "https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide"
)
```

## Arguments

- path:

  Path to the `Codings.tsv` file. Default uses
  [`get_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_codings.md)
  to download the file.

- tables:

  Character vector of table names to read. Available tables:

  - `"opcs4_lkp"` — OPCS-4 procedure lookup

  - `"data_coding_3"` — self-reported cancer (UKB field coding 3)

  - `"data_coding_4"` — self-reported medications (UKB field coding 4)

  - `"data_coding_5"` — self-reported operations (UKB field coding 5)

  - `"data_coding_6"` — self-reported non-cancer illnesses (UKB field
    coding 6)

  By default, all five tables are returned.

- version:

  Character string for the version label (default: `"v0"`).

- source:

  Character string for the data source URL or description.

## Value

A named list where each element corresponds to a requested table,
containing:

- `lookup`: a list with `table` (data frame) and `metadata` (list)

## See also

[`add_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/add_ukb_codings.md),
[`get_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_codings.md)

## Examples

``` r
result <- read_ukb_codings(
  path = dummy_ukb_codings_path(),
  tables = "opcs4_lkp"
)
result$opcs4_lkp$lookup$table
#> # A tibble: 6 × 2
#>   opcs4_code description                                                   
#>   <chr>      <chr>                                                         
#> 1 H01        H01 Emergency excision of appendix                            
#> 2 H011       H01.1 Emergency excision of abnormal appendix and drainage HFQ
#> 3 H012       H01.2 Emergency excision of abnormal appendix NEC             
#> 4 H013       H01.3 Emergency excision of normal appendix                   
#> 5 H018       H01.8 Other specified emergency excision of appendix          
#> 6 H019       H01.9 Unspecified emergency excision of appendix              
```
