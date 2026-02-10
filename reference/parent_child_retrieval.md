# Retrieve parent or child codes

Returns immediate or transitive parent or child codes for the given
codes by traversing the relationship graph.

## Usage

``` r
CHILDREN(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)

PARENTS(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
)

N_CHILDREN(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default",
  call = rlang::caller_env()
)

N_PARENTS(
  ...,
  depth = 1,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  relationship_version = getOption("codeminer.relationship_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default",
  call = rlang::caller_env()
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

- preferred_description_only:

  Logical. If `TRUE`, return only preferred descriptions.

- col_filters:

  Column filters to apply. See
  [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  for details.

- depth:

  Integer. Maximum number of steps to traverse. Use `Inf` for transitive
  closure (all ancestors/descendants). Only used by `N_PARENTS()` and
  `N_CHILDREN()`.

- call:

  **For internal use only.** The execution environment of a currently
  running function. Used for error reporting. Users should not need to
  set this parameter.

## Value

A data frame of codes and descriptions.

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
#> Creating new database at /tmp/RtmpLYACwM/file1c6773ec5399.duckdb
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
PARENTS("E10", "E11", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _relationship_metadata does not exist!
#> Did you mean "sqlite_temp_schema"?
#> 
#> LINE 1: SELECT * FROM _relationship_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
CHILDREN("E10", "E11", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _relationship_metadata does not exist!
#> Did you mean "sqlite_temp_schema"?
#> 
#> LINE 1: SELECT * FROM _relationship_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
N_PARENTS("E10", "E11", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _relationship_metadata does not exist!
#> Did you mean "sqlite_temp_schema"?
#> 
#> LINE 1: SELECT * FROM _relationship_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
N_CHILDREN("E10", "E11", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _relationship_metadata does not exist!
#> Did you mean "sqlite_temp_schema"?
#> 
#> LINE 1: SELECT * FROM _relationship_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
```
