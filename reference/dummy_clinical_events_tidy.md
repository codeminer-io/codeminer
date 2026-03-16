# Dummy UK Biobank clinical events, tidied

A dummy UK Biobank data frame, as returned by
[`ukbwranglr::tidy_clinical_events()`](https://rmgpanw.github.io/ukbwranglr/reference/tidy_clinical_events.html).

## Usage

``` r
dummy_clinical_events_tidy()
```

## Value

A data frame.

## See also

Other Dummy data:
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
dummy_clinical_events_tidy()
#> # A tibble: 7 × 5
#>     eid source  index code  date      
#>   <dbl> <chr>   <chr> <chr> <chr>     
#> 1     1 f40001  0_0   I10   1917-10-08
#> 2     1 f40002  0_0   E109  1955-02-11
#> 3     1 f41271  0_0   4019  1910-02-19
#> 4     1 gpc1_r2 1     C10.. 1965-08-08
#> 5     1 gpc1_r2 2     C10.. 1917-10-08
#> 6     1 gpc3_r3 3     XaIP9 1917-10-08
#> 7     1 gpc3_r3 3     XE0Uc 1917-10-08
```
