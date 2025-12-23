# Default filtering parameters for lookup and mapping tables.

To be used as `col_filters` argument in 'Clinical code lookups and
mappings' functions. Returns a named list where each name in the list
refers to the name of a lookup or mapping table. Each item is also a
named list, where the names refer to column names in the corresponding
table, and the items are vectors of values to filter for.

## Usage

``` r
default_col_filters()
```

## Value

A named list.

## See also

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md)

## Examples

``` r
default_col_filters()
#> $sct_description
#> $sct_description$active_concept
#> [1] "0" "1"
#> 
#> $sct_description$active_description
#> [1] "1"
#> 
#> 
#> $read_v2_icd10
#> $read_v2_icd10$icd10_code_def
#> [1] "1"  "15" "3"  "5"  "7"  "8" 
#> 
#> 
#> $read_v2_read_ctv3
#> $read_v2_read_ctv3$IS_ASSURED
#> [1] "1"
#> 
#> 
#> $read_ctv3_icd10
#> $read_ctv3_icd10$mapping_status
#> [1] "E" "G" "D"
#> 
#> $read_ctv3_icd10$refine_flag
#> [1] "C" "P"
#> 
#> $read_ctv3_icd10$element_num
#> [1] "0"
#> 
#> $read_ctv3_icd10$block_num
#> [1] "0"
#> 
#> 
#> $read_ctv3_read_v2
#> $read_ctv3_read_v2$IS_ASSURED
#> [1] "1"
#> 
#> 
#> $rcsctmap2
#> $rcsctmap2$IS_ASSURED
#> [1] "1"
#> 
#> $rcsctmap2$MapStatus
#> [1] "1"
#> 
#> 
#> $ctv3sctmap2
#> $ctv3sctmap2$IS_ASSURED
#> [1] "1"
#> 
#> $ctv3sctmap2$MAPSTATUS
#> [1] "1"
#> 
#> 
```
