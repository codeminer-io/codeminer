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
#> ℹ Creating new database at /tmp/RtmprDVbPc/file190110edb3a4.duckdb
#> ✔ Lookup table icd10_v0 added successfully.
#> ✔ Lookup table read3_v0 added successfully.
#> ✔ Mapping table read3_icd10_v0 added successfully.
#> ✔ Dummy database ready to use!

# This also sets the environment variable `CODEMINER_DB_PATH`
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmprDVbPc/file19011210ba17.duckdb"
```
