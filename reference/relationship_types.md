# Relationship types

Return relationship types as a `codeminer_codelist`, with each type's
description looked up from the lookup table where available.

## Usage

``` r
RELATIONSHIP_TYPES_FROM(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)

RELATIONSHIP_TYPES_TO(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)

RELATIONSHIP_TYPES(
  pattern = NULL,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
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

- relationship_version:

  Relationship table version (character).

- lookup_version:

  Lookup table version (character).

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- pattern:

  Optional description pattern to filter the relationship types by. See
  [`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
  for details. `NULL` (default) returns all types.

- ignore_case:

  If `TRUE` (default), ignore case when matching `pattern`.

## Value

A `codeminer_codelist` of relationship types with their descriptions.

## Details

- `RELATIONSHIP_TYPES_FROM()` - types originating from the supplied
  codes.

- `RELATIONSHIP_TYPES_TO()` - types pointing to the supplied codes.

- `RELATIONSHIP_TYPES()` - all types in a code type's relationship
  table, optionally filtered by a description `pattern` (like
  [`DESCRIPTION()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION.md)).

Relationship types are themselves codes (e.g. a SNOMED CT relationship
type is a concept id), so they are described via the lookup table. A
type with no matching lookup entry falls back to using its own value as
the description.

These functions are not applicable to purely hierarchical relationship
tables (those with no type column); they error in that case. Use
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
/
[`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
to traverse a hierarchy.

## See also

Other Code relationships:
[`attributes()`](https://codeminer-io.github.io/codeminer/reference/attributes.md),
[`parent_child_retrieval`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)

## Examples

``` r
create_dummy_database()
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpzrC8Cz/file1b3d5b23d58.duckdb")`
#>   `codeminer_connect()`
# RELATIONSHIP_TYPES_FROM() returns types originating from codes
# RELATIONSHIP_TYPES_TO() returns types pointing to codes
# RELATIONSHIP_TYPES() lists / searches all types for a code type
```
