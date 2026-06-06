# Get the full relationship table for a code type

Returns a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
containing the relationship table with standardised column names
(`from`, `to`, `type`, `code_type`) plus all additional columns from the
underlying database table. Call
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to materialise the result.

## Usage

``` r
get_relationship_table(
  type,
  relationship_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- type:

  The code type for which to retrieve relationships.

- relationship_version:

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
with standardised columns (`from`, `to`, `type`, `code_type`) plus all
other columns from the underlying table.

## Details

This is useful for inspecting the raw relationship data used by
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
and other graph traversal functions.

## See also

[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
for graph traversal,
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
for discovering available tables.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpJ0kfSL/file1b25448f68b.duckdb")`
#>   `codeminer_connect()`

# Get the full ICD-10 relationship table
get_relationship_table("ICD-10") |> dplyr::collect()
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 140 × 4
#>    from  to    type  code_type
#>    <chr> <chr> <chr> <chr>    
#>  1 A000  A00   is a  ICD-10   
#>  2 A001  A00   is a  ICD-10   
#>  3 A009  A00   is a  ICD-10   
#>  4 A020  A02   is a  ICD-10   
#>  5 A021  A02   is a  ICD-10   
#>  6 A022  A02   is a  ICD-10   
#>  7 A028  A02   is a  ICD-10   
#>  8 A029  A02   is a  ICD-10   
#>  9 A170  A17   is a  ICD-10   
#> 10 A171  A17   is a  ICD-10   
#> # ℹ 130 more rows
```
