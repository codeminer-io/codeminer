# Get attributes or codes with attributes

These functions traverse the relationship graph to find attributes for
codes or codes that have specific attributes.

## Usage

``` r
ATTRIBUTES_FOR(
  codes,
  relationship_types = NULL,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

HAS_ATTRIBUTES(
  attribute_codes,
  relationship_types = NULL,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)
```

## Arguments

- codes:

  Character vector of codes to start from.

- relationship_types:

  Character vector of relationship types to filter by. If `NULL`
  (default), all relationship types are included.

- code_type:

  Code type (character).

- lookup_version:

  Lookup table version (character).

- relationship_version:

  Relationship table version (character).

- codes_only:

  Logical. If `TRUE`, return only unique codes. If `FALSE`, return a
  data frame with code and description.

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- attribute_codes:

  Character vector of attribute codes to search for.

## Value

A data frame of codes and descriptions, or a character vector if
`codes_only = TRUE`.

## Details

- `ATTRIBUTES_FOR()` returns attribute codes for the supplied codes

- `HAS_ATTRIBUTES()` returns codes that have the supplied attribute
  codes

## See also

Other Code relationships:
[`parent_child_retrieval`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md),
[`relationship_types`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)

## Examples

``` r
create_dummy_database()
#> Creating new database at /tmp/RtmpE2JCfn/file1c2425e84c56.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Relationship table icd10_relationship_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
# ATTRIBUTES_FOR returns attributes for codes
# HAS_ATTRIBUTES returns codes that have the specified attributes
```
