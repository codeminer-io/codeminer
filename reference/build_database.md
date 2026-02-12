# Build the Codeminer database

Set up the codeminer database and create the required lookup and mapping
metadata tables.

## Usage

``` r
build_database(overwrite = FALSE)
```

## Arguments

- overwrite:

  Logical indicating whether to overwrite existing tables (default:
  `FALSE`)

## Value

`TRUE` invisibly if successful.

## Examples

``` r
# Build a temporary database
db_path <- tempfile(fileext = ".duckdb")
Sys.setenv(CODEMINER_DB_PATH = db_path)
build_database()
#> Creating new database at /tmp/RtmpXyzdMY/file19d423f0733f.duckdb
file.exists(db_path)
#> [1] TRUE
```
