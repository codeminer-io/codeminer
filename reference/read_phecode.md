# Read Phecode 1.2 files into lookup and mapping tables

Reads the Phecode 1.2 definitions file and/or ICD mapping files and
returns structured lookup and mapping tables for use with CodeMiner.

## Usage

``` r
read_phecode(
  lkp_path = NULL,
  icd10_map_path = NULL,
  icd9_map_path = NULL,
  version = "v1.2",
  source = "https://phewascatalog.org/phecodes"
)
```

## Arguments

- lkp_path:

  Path to the Phecode 1.2 definitions CSV file. If `NULL`, this table is
  skipped.

- icd10_map_path:

  Path to the Phecode 1.2 to ICD-10 (beta) mapping CSV file. If `NULL`,
  this table is skipped.

- icd9_map_path:

  Path to the Phecode 1.2 to ICD-9 mapping `.rda` file. If `NULL`, this
  table is skipped.

- version:

  Character string for the version label (default: `"v1.2"`).

- source:

  Character string for the data source URL or description.

## Value

A named list containing any of the following elements (depending on
which paths are provided):

- `phecode_lkp`: list with `lookup` → `list(table, metadata)`

- `icd10_phecode`: list with `mapping` → `list(table, metadata)`

- `icd9_phecode`: list with `mapping` → `list(table, metadata)`

## See also

[`add_phecode()`](https://codeminer-io.github.io/codeminer/reference/add_phecode.md),
[`get_phecode()`](https://codeminer-io.github.io/codeminer/reference/get_phecode.md)

## Examples

``` r
result <- read_phecode(
  lkp_path = dummy_phecode_lkp_path(),
  icd10_map_path = dummy_icd10_phecode_map_path()
)
result$phecode_lkp$lookup$table
#> # A tibble: 33 × 8
#>    phecode phenotype    phecode_exclude_range sex   rollup leaf  category_number
#>    <chr>   <chr>        <chr>                 <chr> <chr>  <chr> <chr>          
#>  1 10      Tuberculosis 010-041.99            NA    1      1     1              
#>  2 249     Secondary d… 249-250.99            NA    1      1     3              
#>  3 250     Diabetes me… 249-250.99            Both  1      0     3              
#>  4 250.1   Type 1 diab… 249-250.99            Both  1      0     3              
#>  5 250.11  Type 1 diab… 249-250.99            Both  1      1     3              
#>  6 250.12  Type 1 diab… 249-250.99            Both  1      1     3              
#>  7 250.13  Type 1 diab… 249-250.99            Both  1      1     3              
#>  8 250.14  Type 1 diab… 249-250.99            Both  1      1     3              
#>  9 250.15  Diabetes ty… 249-250.99            Both  1      1     3              
#> 10 250.2   Type 2 diab… 249-250.99            Both  1      0     3              
#> # ℹ 23 more rows
#> # ℹ 1 more variable: category <chr>
result$icd10_phecode$mapping$table
#> # A tibble: 73 × 4
#>    ICD10 PHECODE `Exl. Phecodes`          `Excl. Phenotypes`                    
#>    <chr> <chr>   <chr>                    <chr>                                 
#>  1 E10   250.1   249-250.99               DIABETES                              
#>  2 E10.0 250.1   249-250.99               DIABETES                              
#>  3 E10.1 250.11  249-250.99               DIABETES                              
#>  4 E10.2 250.12  249-250.99               DIABETES                              
#>  5 E10.3 250.23  249-250.99               DIABETES                              
#>  6 E10.3 250.7   250.70-250.79,360-365.99 diabetic retinopathy and degenerative…
#>  7 E10.3 250.13  249-250.99               DIABETES                              
#>  8 E10.4 250.14  249-250.99               DIABETES                              
#>  9 E10.4 250.24  249-250.99               DIABETES                              
#> 10 E10.5 443.7   440-449.99               DISEASES OF ARTERIES, ARTERIOLES, AND…
#> # ℹ 63 more rows
```
