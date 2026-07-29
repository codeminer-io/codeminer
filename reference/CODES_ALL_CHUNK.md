# Chunked, bounded "all codes of a type" fetch

Bounded variant of `CODES("all", ...)` for callers that need to keep any
single call's work small (e.g. to stay under a network-layer request
timeout), rather than materialising the whole code type in one unbounded
scan. See
[`DESCRIPTION_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION_CHUNK.md)
for the equivalent for a description search - this is simpler, since
there's no search predicate: each chunk is just the `rowid`-bounded
slice of the lookup table, read directly.

## Usage

``` r
CODES_ALL_CHUNK(
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 200000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)
```

## Arguments

- type:

  character. Type of clinical code system to be searched. Optional if
  input is a data frame with code_type column. Depends on what is
  available in the lookup tables. See
  [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  on how to add new lookup tables. This can also be configured through
  the `codeminer.code_type` option.

- cursor:

  Integer. `rowid` to start this chunk's scan from. `0` for the first
  call.

- batch_size:

  Integer. Number of `rowid`s to scan in this call. Defaults to
  `getOption("codeminer.chunk_batch_size", default = 2000)`.

- total_rows:

  Integer or `NULL`. The underlying table's total row count. Pass `NULL`
  on the first call (it will be resolved and returned); pass the
  previously-returned value on subsequent calls to skip re-resolving it.

- accumulated_so_far:

  Integer. Total matched rows found by this leaf's previous chunks
  (before this one). `0` for the first call. Used to check `max_rows`
  cumulatively across the whole chunked fetch, not just this one call.

- max_rows:

  Integer. Ceiling on this leaf's cumulative matched rows across all
  chunks. Aborts with class `codeminer_max_leaf_rows_exceeded` if
  exceeded. Defaults to
  `getOption("codeminer.max_leaf_rows", default = 30000)`.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- preferred_description_only:

  Logical. If `TRUE`, return only the preferred description for each
  code.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

## Value

A list with `result`, `next_cursor`, `total_rows`, and `exhausted`

- see
  [`DESCRIPTION_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION_CHUNK.md)
  for the shape.

## See also

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`CODES_LIKE_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/CODES_LIKE_CHUNK.md),
[`DESCRIPTION_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION_CHUNK.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md),
[`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md),
[`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md),
[`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpfyR2XV/file1b525e4d3179.duckdb")`
#>   `codeminer_connect()`
chunk <- CODES_ALL_CHUNK(type = "ICD-10", batch_size = 100)
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
chunk$result
#> <codeminer_codelist>: 100 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 100 × 3
#>    code  description                                        code_type
#>    <chr> <chr>                                              <chr>    
#>  1 A00   Cholera                                            ICD-10   
#>  2 A000  Cholera due to Vibrio cholerae 01, biovar cholerae ICD-10   
#>  3 A001  Cholera due to Vibrio cholerae 01, biovar eltor    ICD-10   
#>  4 A009  Cholera, unspecified                               ICD-10   
#>  5 A010  Typhoid fever                                      ICD-10   
#>  6 A02   Other salmonella infections                        ICD-10   
#>  7 A020  Salmonella enteritis                               ICD-10   
#>  8 A021  Salmonella sepsis                                  ICD-10   
#>  9 A022  Localized salmonella infections                    ICD-10   
#> 10 A028  Other specified salmonella infections              ICD-10   
#> # ℹ 90 more rows
chunk$exhausted
#> [1] FALSE
```
