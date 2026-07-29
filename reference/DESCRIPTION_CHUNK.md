# Chunked, bounded DESCRIPTION search

Bounded variant of
[`DESCRIPTION()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION.md)
for callers that need to keep any single call's work small (e.g. to stay
under a network-layer request timeout), rather than materialising every
match in one unbounded scan.

## Usage

``` r
DESCRIPTION_CHUNK(
  pattern,
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 200000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  max_chunk_matches = getOption("codeminer.max_chunk_matches", default = 20000L),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
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

- ignore_case:

  If `TRUE` (default), ignore case in `description`.

- preferred_description_only:

  `logical`. If `TRUE` (default), return only preferred descriptions.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

## Value

A list with `result` (a `codeminer_codelist` of this chunk's matches),
`next_cursor` (integer, pass back as `cursor` next call), `total_rows`
(integer, pass back as `total_rows` next call), and `exhausted` (logical
— `TRUE` once `next_cursor` has covered the whole table).

## Details

Each call scans only the `rowid` range `[cursor, cursor + batch_size)`
of the underlying lookup table for matches, expands those matches to
full rows via
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
and returns a cursor for the next chunk. Callers should keep calling
with the returned `next_cursor` (and the same `total_rows` and
`accumulated_so_far`, updated each time) until `exhausted` is `TRUE`.

## See also

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`CODES_ALL_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/CODES_ALL_CHUNK.md),
[`CODES_LIKE_CHUNK()`](https://codeminer-io.github.io/codeminer/reference/CODES_LIKE_CHUNK.md),
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
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpfyR2XV/file1b522fa27c03.duckdb")`
#>   `codeminer_connect()`
chunk <- DESCRIPTION_CHUNK("cyst", type = "ICD-10", batch_size = 100)
#> ℹ Using "UKB v4" as the latest lookup version for "ICD-10".
chunk$result
#> <codeminer_codelist>: 0 codes
#> 
#> # A tibble: 0 × 3
#> # ℹ 3 variables: code <chr>, description <chr>, code_type <chr>
chunk$exhausted
#> [1] FALSE
```
