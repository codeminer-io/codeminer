# Look up descriptions for clinical codes

Returns a codelist with descriptions for the codes of interest. Supports
flexible input: character vectors, `||` separated strings, or data
frames.

## Usage

``` r
CODES(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
)

CODES_LIKE(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE
)
```

## Arguments

- ...:

  Codes to look up. Can be:

  - Character vectors: `CODES("E10", "E11", type = "ICD-10")`

  - `||` separated strings: `CODES("E10 || E11", type = "ICD-10")`

  - Data frame with code/description/code_type columns: `CODES(my_df)`

  - Mixed: `CODES("E10", my_vector, "E13 || E14", type = "ICD-10")`

  Special values: `"all"` returns all codes; empty input returns empty
  codelist.

  Comments can be added with `<< >>` syntax:
  `"E10 << Type 1 diabetes >>"`.

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

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

- pattern:

  a regular expression to search for

## Value

A `codeminer_codelist` object (tibble) containing the codes and their
descriptions

## Details

`CODES_LIKE` searches for codes that match a given regular expression.
The matching is case-insensitive.

## See also

[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
for adding new lookup tables to the database.

Other Clinical code lookups and mappings:
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md)

## Examples

``` r
# Set up a temporary dummy database
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> Creating new database at /tmp/RtmpDQc1MV/file1c5e75222696.duckdb
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

# Multiple arguments
CODES("E10", "E11", type = "ICD-10")
#> ℹ Using database at ~/.local/share/codeminer/ontology.duckdb
#> ℹ Set `CODEMINER_DB_PATH` or use `codeminer_connect()` to change this.
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG

# With comments
CODES("E10 << Type 1 diabetes >>", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG

# || separated string
CODES("E10 || E11", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG

# Splice operator
my_codes <- c("E10", "E11")
CODES(!!!my_codes, type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG

# Data frame input
df <- data.frame(
  code = c("E10", "E11"),
  description = c("Type 1", "Type 2"),
  code_type = c("ICD-10", "ICD-10")
)
CODES(df)
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
CODES_LIKE("^E1", type = "ICD-10")
#> Error in dbSendQuery(conn, statement, ...): Catalog Error: Table with name _lookup_metadata does not exist!
#> Did you mean "duckdb_databases"?
#> 
#> LINE 1: SELECT * FROM _lookup_metadata
#>                       ^
#> ℹ Context: rapi_prepare
#> ℹ Error type: CATALOG
```
