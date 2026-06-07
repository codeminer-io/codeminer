# Create a dummy database

Sets up an example database for codeminer with dummy data, sets the
environment variable `CODEMINER_DB_PATH`, and connects the workbench.
Any subsequent `codeminer` actions will use this database.

## Usage

``` r
create_dummy_database(
  db_path = tempfile(fileext = ".duckdb"),
  ...,
  verbose = FALSE,
  .local = FALSE,
  .envir = parent.frame()
)
```

## Arguments

- db_path:

  Path to the database file. Defaults to a temporary file. This is to
  avoid writing the dummy data to an already existing database.

- ...:

  These dots are for future extensions and must be empty.

- verbose:

  If `TRUE`, prints progress messages during the database build.
  Defaults to `FALSE` for cleaner output.

- .local:

  If `FALSE` (default), sets `CODEMINER_DB_PATH` globally via
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html). The dummy
  database persists until you reconnect manually. If `TRUE`, uses
  [`withr::local_envvar()`](https://withr.r-lib.org/reference/with_envvar.html)
  scoped to `.envir` so the environment variable and workbench are
  automatically cleaned up when `.envir` exits. Use `.local = TRUE` in
  tests and functions that need automatic teardown.

- .envir:

  Environment for scoped cleanup when `.local = TRUE`. Ignored when
  `.local = FALSE`. Defaults to the calling environment.

## Value

The path to the created database file, invisibly.

## Examples

``` r
# Create dummy database in a temporary location
temp_db <- tempfile(fileext = ".duckdb")
create_dummy_database(temp_db)
#> ✔ Dummy database ready to use!
#> ℹ To reconnect to your previous database:
#>   `Sys.setenv(CODEMINER_DB_PATH = "/tmp/RtmpElJchn/file1add239da680.duckdb")`
#>   `codeminer_connect()`

# This also sets the environment variable `CODEMINER_DB_PATH`
Sys.getenv("CODEMINER_DB_PATH")
#> [1] "/tmp/RtmpElJchn/file1add1e19b70d.duckdb"
```
