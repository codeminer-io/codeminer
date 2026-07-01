# Dummy NHS BSA BNF code information (pseudo-BNF) file path

Returns the file path to a dummy NHS BSA "BNF Code Information" csv file
(a small subset of the full dataset, which may be downloaded with
[`get_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/get_pseudobnf.md)).

## Usage

``` r
dummy_pseudobnf_path()
```

## Value

A string.

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
dummy_pseudobnf_path()
#> [1] "/home/runner/work/_temp/Library/codeminer/extdata/dummy_bnf_code_information.csv"
```
