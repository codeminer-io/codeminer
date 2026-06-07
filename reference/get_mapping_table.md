# Get the full mapping table for a pair of code types

Returns a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
containing the mapping table with standardised column names (`from`,
`to`) plus all additional columns from the underlying database table.
Call
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to materialise the result.

## Usage

``` r
get_mapping_table(
  from,
  to,
  map_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- from:

  The source code type to map from.

- to:

  The target code type to map to.

- map_version:

  The version to retrieve. Defaults to `"latest"`.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- con:

  Optional DBI connection. If `NULL` (default), uses the workbench
  connection.

- call:

  The calling environment. Passed to
  [codeminer_abort](https://codeminer-io.github.io/codeminer/reference/conditions.md).

## Value

A lazy [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
with standardised columns (`from`, `to`) plus all other columns from the
underlying table.

## Details

This is useful for inspecting columns beyond the standard codelist
output returned by
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md).

## See also

[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md) for
standardised codelist output,
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
for discovering available tables.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/Rtmpqz7mVJ/file1b1344498438.duckdb")`
#>   `codeminer_connect()`

# Get the full Read 3 to ICD-10 mapping table
get_mapping_table("Read v3", "ICD-10") |> dplyr::collect()
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 5 × 8
#>   from  to    mapping_status refine_flag add_code_flag element_num block_num
#>   <chr> <chr> <chr>          <chr>       <chr>         <chr>       <chr>    
#> 1 X40J4 E109  D              C           P             0           0        
#> 2 C10.. E149  D              C           C             0           0        
#> 3 XaIP9 L721  D              C           C             0           0        
#> 4 XE0e0 N390  D              C           P             0           0        
#> 5 XE0Uc I10   D              C           C             0           0        
#> # ℹ 1 more variable: icd10_dagger_asterisk <chr>
```
