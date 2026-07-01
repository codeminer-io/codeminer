# Read dummy UK Biobank codings into R

Reads a dummy [UK Biobank
codings](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
tsv file into R.

## Usage

``` r
read_ukb_codings_dummy()
```

## Value

A data frame.

## See also

Other Dummy data:
[`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md),
[`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md),
[`dummy_pseudobnf_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_pseudobnf_path.md),
[`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md),
[`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md),
[`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md)

## Examples

``` r
read_ukb_codings_dummy()
#> # A tibble: 6 × 3
#>   Coding Value Meaning                                                       
#>   <chr>  <chr> <chr>                                                         
#> 1 240    H01   H01 Emergency excision of appendix                            
#> 2 240    H011  H01.1 Emergency excision of abnormal appendix and drainage HFQ
#> 3 240    H012  H01.2 Emergency excision of abnormal appendix NEC             
#> 4 240    H013  H01.3 Emergency excision of normal appendix                   
#> 5 240    H018  H01.8 Other specified emergency excision of appendix          
#> 6 240    H019  H01.9 Unspecified emergency excision of appendix              
```
