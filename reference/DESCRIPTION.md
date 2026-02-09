# Search for codes that match a description

Returns a codelist with clinical codes that match the provided
description pattern.

## Usage

``` r
DESCRIPTION(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  preferred_description_only = TRUE
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

- lookup_version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- ignore_case:

  If `TRUE` (default), ignore case in `description`.

- preferred_description_only:

  `logical`. If `TRUE` (default), return only preferred descriptions.

## Value

A `codeminer_codelist` with codes that match the description.

## Examples

``` r
# build dummy database
create_dummy_database()
#> Creating new database at /tmp/Rtmp8JLRAf/file1c6d61330ee2.duckdb
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

# lookup ICD10 code descriptions matching 'cyst'
DESCRIPTION("cyst", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
```
