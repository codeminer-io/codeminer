# Dummy Phecode definitions file path

Returns the file path to a dummy Phecode definitions 1.2 csv file (full
version may be downloaded from
[phewascatalog.org](https://phewascatalog.org/phecodes_icd10)).

## Usage

``` r
dummy_phecode_lkp_path()
```

## Value

A string.

## See also

Other Dummy data:
[`build_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps_dummy.md),
[`dummy_all_lkps_maps_db()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_db.md),
[`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md),
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps_dummy.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
dummy_phecode_lkp_path()
#> [1] "/home/runner/work/_temp/Library/codeminer/extdata/dummy_phecode_definitions1.2.csv"
```
