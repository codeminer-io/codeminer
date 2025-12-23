# Get relationship types for codes

These functions return the distinct relationship types that originate
from or point to the supplied codes.

## Usage

``` r
RELATIONSHIP_TYPES_FROM(
  codes,
  code_type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest")
)

RELATIONSHIP_TYPES_TO(
  codes,
  code_type = getOption("codeminer.code_type"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest")
)
```

## Arguments

- codes:

  Character vector of codes to start from.

- code_type:

  Code type (character).

- relationship_version:

  Relationship table version (character).

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
#> Creating new database at /tmp/RtmpE2JCfn/file1c2455ae745b.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Relationship table icd10_relationship_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!
# RELATIONSHIP_TYPES_FROM returns types originating from codes
# RELATIONSHIP_TYPES_TO returns types pointing to codes
```
