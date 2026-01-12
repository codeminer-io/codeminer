# Retrieve parent or child codes

Returns immediate or transitive parent or child codes for the given
codes by traversing the relationship graph.

## Usage

``` r
CHILDREN(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

PARENTS(
  codes,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

N_CHILDREN(
  codes,
  depth = 1,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)

N_PARENTS(
  codes,
  depth = 1,
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

- depth:

  Integer. Maximum number of steps to traverse. Use `Inf` for transitive
  closure (all ancestors/descendants). Only used by `N_PARENTS()` and
  `N_CHILDREN()`.

## Value

A data frame of codes and descriptions, or a character vector if
`codes_only = TRUE`.

## Details

Use `N_PARENTS()`/`N_CHILDREN()` for immediate relationships (one step),
and `PARENTS()`/`CHILDREN()` for transitive closure (all reachable
ancestors/descendants).

## See also

Other Code relationships:
[`attributes()`](https://codeminer-io.github.io/codeminer/reference/attributes.md),
[`relationship_types`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)

## Examples

``` r
create_dummy_database()
#> Creating new database at /tmp/RtmpR3vBPA/file1def17587ccb.duckdb
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
PARENTS(c("E10", "E11"), code_type = "ICD-10")
#> Error in N_PARENTS(codes, depth = Inf, code_type = code_type, lookup_version = lookup_version,     relationship_version = relationship_version, codes_only = codes_only,     preferred_description_only = preferred_description_only): Code type 'ICD-10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
CHILDREN(c("E10", "E11"), code_type = "ICD-10")
#> Error in N_CHILDREN(codes, depth = Inf, code_type = code_type, lookup_version = lookup_version,     relationship_version = relationship_version, codes_only = codes_only,     preferred_description_only = preferred_description_only): Code type 'ICD-10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
N_PARENTS(c("E10", "E11"), code_type = "ICD-10")
#> Error in N_PARENTS(c("E10", "E11"), code_type = "ICD-10"): Code type 'ICD-10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
N_CHILDREN(c("E10", "E11"), code_type = "ICD-10")
#> Error in N_CHILDREN(c("E10", "E11"), code_type = "ICD-10"): Code type 'ICD-10' not found in relationship metadata.
#> ℹ Did you add the relationship table with
#>   `codeminer::add_relationship_table()`?
```
