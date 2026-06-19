# Get the full relationship table for a code type

Returns a lazy
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
containing the relationship table with standardised column names
(`from`, `to`, `code_type`, plus `type` for multi-type tables) and all
additional columns from the underlying database table. Purely
hierarchical tables have no `type` column. Call
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
to materialise the result.

## Usage

``` r
get_relationship_table(
  type,
  codes = NULL,
  endpoints = c("both", "either"),
  relationship_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- type:

  The code type for which to retrieve relationships.

- codes:

  Optional character vector of codes used to filter edges. If `NULL`
  (default), all rows are returned. The `endpoints` argument controls
  how an edge is matched against `codes`.

- endpoints:

  One of `"both"` (default) or `"either"`. With `"both"`, an edge is
  kept only when both endpoints (`from` *and* `to`) are in `codes`. With
  `"either"`, an edge is kept when at least one endpoint is in `codes` —
  this surfaces edges crossing the boundary of the input set. For the
  common "give me ancestors / descendants of these codes as a codelist"
  question, prefer
  [`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  /
  [`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md).

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
with standardised columns (`from`, `to`, `code_type`, plus `type` for
multi-type tables) and all other columns from the underlying table.

## Details

This is useful for inspecting the raw relationship data used by
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
and other graph traversal functions.

## See also

[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
for the common "give me ancestors / descendants of these codes as a
codelist" use case — this helper returns raw edge rows and is mainly
useful when the edge structure itself is needed.
[`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)
for a `{nodes, edges}` tree view.
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
for discovering available tables.

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpYmlTfJ/file1a2e37ab293b.duckdb")`
#>   `codeminer_connect()`

# Get the full ICD-10 relationship table
get_relationship_table("ICD-10") |> dplyr::collect()
#> ℹ Using 'UKB v4' as latest version
#> # A tibble: 140 × 3
#>    from  to    code_type
#>    <chr> <chr> <chr>    
#>  1 A000  A00   ICD-10   
#>  2 A001  A00   ICD-10   
#>  3 A009  A00   ICD-10   
#>  4 A020  A02   ICD-10   
#>  5 A021  A02   ICD-10   
#>  6 A022  A02   ICD-10   
#>  7 A028  A02   ICD-10   
#>  8 A029  A02   ICD-10   
#>  9 A170  A17   ICD-10   
#> 10 A171  A17   ICD-10   
#> # ℹ 130 more rows

# Default endpoints = "both": only internal edges of the input set
# (UKB-style ICD-10 codes have no dot — e.g. E101 = E10.1).
get_relationship_table(
  "ICD-10",
  codes = c("E10", "E101", "E102")
) |> dplyr::collect()
#> # A tibble: 2 × 3
#>   from  to    code_type
#>   <chr> <chr> <chr>    
#> 1 E101  E10   ICD-10   
#> 2 E102  E10   ICD-10   

# endpoints = "either": also surfaces edges crossing the boundary —
# e.g. the parent of E10 and any siblings of E101 / E102. Rarely
# needed in user code; prefer PARENTS() / CHILDREN() for those.
get_relationship_table(
  "ICD-10",
  codes = c("E10", "E101", "E102"),
  endpoints = "either"
) |> dplyr::collect()
#> # A tibble: 10 × 3
#>    from  to    code_type
#>    <chr> <chr> <chr>    
#>  1 E100  E10   ICD-10   
#>  2 E101  E10   ICD-10   
#>  3 E102  E10   ICD-10   
#>  4 E103  E10   ICD-10   
#>  5 E104  E10   ICD-10   
#>  6 E105  E10   ICD-10   
#>  7 E106  E10   ICD-10   
#>  8 E107  E10   ICD-10   
#>  9 E108  E10   ICD-10   
#> 10 E109  E10   ICD-10   
```
