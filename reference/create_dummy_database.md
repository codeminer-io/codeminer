# Create a dummy database

Sets up an example database for codeminer with dummy data and sets the
environment variable `CODEMINER_DB_PATH`. Any subsequent `codeminer`
actions will use this database.

## Usage

``` r
create_dummy_database(
  db_path = tempfile(fileext = ".duckdb"),
  ...,
  .envir = parent.frame()
)
```

## Arguments

- db_path:

  Path to the database file. Defaults to a temporary file. This is to
  avoid writing the dummy data to an already existing database.

- ...:

  These dots are for future extensions and must be empty.

- .envir:

  Environment in which to set the `CODEMINER_DB_PATH` variable. Defaults
  to the calling environment.

## Value

The path to the created database file, invisibly.

## Examples

``` r
# Create dummy database in a temporary location
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> Creating new database at /tmp/RtmpLYACwM/file1c677fc5ad2e.duckdb
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

# This also sets the environment variable `CODEMINER_DB_PATH`
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmpLYACwM/file1c6771e27a04.duckdb"
```
