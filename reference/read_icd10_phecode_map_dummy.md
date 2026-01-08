# Read dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R

Reads a dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R (full
version may be downloaded from
[phewascatalog.org](https://phewascatalog.org/phecodes_icd10))

## Usage

``` r
read_icd10_phecode_map_dummy()
```

## Value

A data frame.

## See also

Other Dummy data:
[`build_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps_dummy.md),
[`dummy_all_lkps_maps_db()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_db.md),
[`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md),
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
read_icd10_phecode_map_dummy()
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
