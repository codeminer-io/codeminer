# Get relationship types for codes

These functions return the distinct relationship types that originate
from or point to the supplied codes.

## Usage

``` r
RELATIONSHIP_TYPES_FROM(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  col_filters = "default"
)

RELATIONSHIP_TYPES_TO(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  col_filters = "default"
)
```

## Arguments

- ...:

  Codes to start from. Supports flexible input like
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).

- type:

  Code type (character).

- relationship_version:

  Relationship table version (character).

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

## Value

A character vector of distinct relationship types.

## Details

- `RELATIONSHIP_TYPES_FROM()` returns relationship types originating
  from codes

- `RELATIONSHIP_TYPES_TO()` returns relationship types pointing to codes

## See also

Other Code relationships:
[`attributes()`](https://codeminer-io.github.io/codeminer/reference/attributes.md),
[`parent_child_retrieval`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/Rtmp8tnFLb/file1c5368f3bf6a.duckdb")`
#>   `codeminer_connect()`
# RELATIONSHIP_TYPES_FROM returns types originating from codes
# RELATIONSHIP_TYPES_TO returns types pointing to codes
```
