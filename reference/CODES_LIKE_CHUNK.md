# Chunked, bounded CODES_LIKE search

Bounded variant of
[`CODES_LIKE()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
for callers that need to keep any single call's work small (e.g. to stay
under a network-layer request timeout), rather than materialising every
match in one unbounded scan. Same shape as
[`DESCRIPTION_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION_CHUNK.md),
matching on `code` instead of `description`.

## Usage

``` r
CODES_LIKE_CHUNK(
  pattern,
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 200000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  max_chunk_matches = getOption("codeminer.max_chunk_matches", default = 20000L),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)
```

## Arguments

- pattern:

  The description to search for. See
  [`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
  for details.

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

- max_chunk_matches:

  Integer. Ceiling on the number of codes matched *within this single
  chunk*, checked before the matches are expanded via
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).
  Guards against a chunk whose scan range happens to match densely -
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)'s
  cost scales with match count, not rows scanned, so this bounds a
  single call's worst case independently of `batch_size`. Aborts with
  class `codeminer_chunk_match_limit_exceeded` if exceeded. Defaults to
  `getOption("codeminer.max_chunk_matches", default = 20000)`.

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- preferred_description_only:

  `logical`. If `TRUE` (default), return only preferred descriptions.

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
[`CODES_ALL_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/CODES_ALL_CHUNK.md),
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
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpOTuqjN/file1a0b69bbeefa.duckdb")`
#>   `codeminer_connect()`
chunk <- CODES_LIKE_CHUNK("^E1", type = "ICD-10", batch_size = 100)
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
chunk$result
#> <codeminer_codelist>: 55 codes
#> Code type: "ICD-10"
#> 
#> # A tibble: 55 × 3
#>    code  description                                                   code_type
#>    <chr> <chr>                                                         <chr>    
#>  1 E10   Type 1 diabetes mellitus                                      ICD-10   
#>  2 E100  Type 1 diabetes mellitus With coma                            ICD-10   
#>  3 E101  Type 1 diabetes mellitus With ketoacidosis                    ICD-10   
#>  4 E102  Type 1 diabetes mellitus With renal complications             ICD-10   
#>  5 E103  Type 1 diabetes mellitus With ophthalmic complications        ICD-10   
#>  6 E104  Type 1 diabetes mellitus With neurological complications      ICD-10   
#>  7 E105  Type 1 diabetes mellitus With peripheral circulatory complic… ICD-10   
#>  8 E106  Type 1 diabetes mellitus With other specified complications   ICD-10   
#>  9 E107  Type 1 diabetes mellitus With multiple complications          ICD-10   
#> 10 E108  Type 1 diabetes mellitus With unspecified complications       ICD-10   
#> # ℹ 45 more rows
chunk$exhausted
#> [1] FALSE
```
