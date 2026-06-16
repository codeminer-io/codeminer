# Build the Codeminer database

Set up the codeminer database and create the required lookup and mapping
metadata tables.

## Usage

``` r
build_database(overwrite = FALSE, format = c("duckdb", "parquet"))
```

## Arguments

- overwrite:

  Logical indicating whether to overwrite existing tables (default:
  `FALSE`).

- format:

  Character. For folder-mode databases, controls how data tables are
  stored inside the folder. One of:

  - `"duckdb"` (default): metadata stays as parquet at the folder root;
    each data table is a `<name>.duckdb` file. Faster recursive queries
    (CHILDREN etc.) at the cost of ~50% larger disk than `"parquet"`.

  - `"parquet"`: data tables and metadata are all parquet files at the
    folder root. Smaller on disk; recursive queries hit re-scan cost.
    Ignored when `CODEMINER_DB_PATH` points at a single `.duckdb` file
    (there's only one shape — a single DuckDB file).

## Value

`TRUE` invisibly if successful.

## Examples

``` r
# Build a temporary database
db_path <- tempfile(fileext = ".duckdb")
Sys.setenv(CODEMINER_DB_PATH = db_path)
build_database()
#> Creating new database at /tmp/RtmpqPRmeg/file1ad25862e062.duckdb
file.exists(db_path)
#> [1] TRUE
```
