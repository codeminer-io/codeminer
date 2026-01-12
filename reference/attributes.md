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
#> Creating new database at /tmp/RtmpR3vBPA/file1def3961b034.duckdb
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
# ATTRIBUTES_FOR returns attributes for codes
# HAS_ATTRIBUTES returns codes that have the specified attributes
```
