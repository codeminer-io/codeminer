# Get attributes or codes with attributes

These functions traverse the relationship graph to find attributes for
codes or codes that have specific attributes.

## Usage

``` r
ATTRIBUTES_FOR(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default"
)

HAS_ATTRIBUTES(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  relationship_types = NULL,
  preferred_description_only = TRUE,
  col_filters = "default"
)
```

## Arguments

- ...:

  Codes to start from. Supports flexible input like
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md).

- type:

  Code type (character).

- lookup_version:

  Lookup table version (character).

- relationship_version:

  Relationship table version (character).

- relationship_types:

  Relationship types to filter by. Can be a character vector, a
  `codeminer_codelist` (e.g. from
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)),
  or a data frame with a `code` column. Supports `<<description>>`
  comments. If `NULL` (default), all relationship types are included.

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

## Value

A data frame of codes and descriptions

## Details

- `ATTRIBUTES_FOR()` returns attribute codes for the supplied codes

- `HAS_ATTRIBUTES()` returns codes that have the specified attribute
  codes

## See also

Other Code relationships:
[`parent_child_retrieval`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`relationship_types`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpyHnlWf/file1a3017c58096")`
#>   `codeminer_connect()`
# ATTRIBUTES_FOR returns attributes for codes
# HAS_ATTRIBUTES returns codes that have the specified attributes
```
