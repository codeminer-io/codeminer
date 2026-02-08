# Get codeminer metadata

Returns metadata about the lookup, mapping, and relationship tables in
the codeminer database.

## Usage

``` r
get_codeminer_metadata(
  type = c("lookup", "mapping", "relationship"),
  con = NULL
)
```

## Arguments

- type:

  The type of metadata to return. By default returns a list containing
  all metadata types. Otherwise, returns a data frame for the specified
  type. Must be one of "lookup", "mapping" or "relationship".

- con:

  Optional DBI connection. If `NULL` (default), uses the workbench
  connection.

## Value

If a single type is requested, a data frame. If multiple types are
requested, a named list of data frames.

## Examples

``` r
create_dummy_database()
#> Creating new database at /tmp/RtmpDQc1MV/file1c5e13879078.duckdb
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
get_codeminer_metadata()
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
get_codeminer_metadata("lookup")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
get_codeminer_metadata(c("lookup", "mapping"))
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
```
