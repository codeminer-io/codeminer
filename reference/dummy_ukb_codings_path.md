# Dummy UK Biobank codings file path

Returns the file path to a dummy [UK Biobank
codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
tsv file.

## Usage

``` r
dummy_ukb_codings_path()
```

## Value

A string.

## See also

Other Dummy data:
[`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md),
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md),
[`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)

## Examples

``` r
dummy_ukb_codings_path()
#> [1] "/home/runner/work/_temp/Library/codeminer/extdata/dummy_Codings.tsv"
```
