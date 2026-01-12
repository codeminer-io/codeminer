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
#> Creating new database at /tmp/RtmpR3vBPA/file1def4e14af15.duckdb
#> Reading 17 selected tables from UKB Resource 592
#> 
#> Extending read_v2_drugs_bnf with BNF hierarchy and descriptions
#> Extending read_v2_icd10 by expanding ICD-10 code ranges
#> Adding tables to database
#> ✔ Lookup table BNF_UKB v4 added successfully.
#> ✔ Relationship table BNF_relationship_UKB v4 added successfully.
#> ✔ Lookup table DM+D_UKB v4 added successfully.
#> ✔ Lookup table ICD-9_UKB v4 added successfully.
#> ✔ Relationship table ICD-9_relationship_UKB v4 added successfully.
#> ✔ Lookup table ICD-10_UKB v4 added successfully.
#> ✔ Relationship table ICD-10_relationship_UKB v4 added successfully.
#> ✔ Mapping table ICD-9_ICD-10_UKB v4 added successfully.
#> ✔ Lookup table Read 2_UKB v4 added successfully.
#> ✔ Relationship table Read 2_relationship_UKB v4 added successfully.
#> ✔ Lookup table Read 2, drugs_UKB v4 added successfully.
#> ✔ Mapping table Read 2, drugs_BNF_UKB v4 added successfully.
#> ✔ Mapping table Read 2_ICD-9_UKB v4 added successfully.
#> ✔ Mapping table Read 2_ICD-10_UKB v4 added successfully.
#> ✔ Mapping table Read 2_OPCS4_UKB v4 added successfully.
#> ✔ Mapping table Read 2_Read 3_UKB v4 added successfully.
#> ✔ Lookup table Read 3_UKB v4 added successfully.
#> ✔ Mapping table Read 3_ICD-9_UKB v4 added successfully.
#> ✔ Mapping table Read 3_ICD-10_UKB v4 added successfully.
#> ✔ Mapping table Read 3_OPCS4_UKB v4 added successfully.
#> ✔ Mapping table Read 3_Read 2_UKB v4 added successfully.
#> ✔ Dummy database ready to use!
# RELATIONSHIP_TYPES_FROM returns types originating from codes
# RELATIONSHIP_TYPES_TO returns types pointing to codes
```
