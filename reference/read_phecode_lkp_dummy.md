# Read dummy Phecode definitions file into R

Reads a dummy Phecode definitions 1.2 csv file into R (full version may
be downloaded from
[phewascatalog.org](https://phewascatalog.org/phecodes_icd10))

## Usage

``` r
read_phecode_lkp_dummy()
```

## Value

A data frame.

## See also

Other Dummy data:
[`build_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps_dummy.md),
[`dummy_all_lkps_maps_db()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_db.md),
[`dummy_all_lkps_maps_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_path.md),
[`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md),
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps_dummy.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
read_phecode_lkp_dummy()
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
```
