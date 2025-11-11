# Build a Duckdb database of clinical code look up and mapping tables

Write the output from
[`build_all_lkps_maps`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps.md)
to a Duckdb database.

## Usage

``` r
all_lkps_maps_to_db(
  all_lkps_maps = build_all_lkps_maps(),
  db_path = "all_lkps_maps.db",
  overwrite = FALSE
)
```

## Arguments

- all_lkps_maps:

  A named list of look up and mapping tables, created by
  [`build_all_lkps_maps`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps.md).

- db_path:

  Where the database will be created. If an Duckdb database file already
  exists here, then the lookup and mapping tables will be added to this.
  If `NULL` (default), then no database will be created/modified.

- overwrite:

  If `TRUE`, overwrite tables in the database if they already exist.
  Default value is `FALSE`.

## Value

Returns `db_path` invisibly

## See also

[`build_all_lkps_maps()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps.md)

## Examples

``` r
# build dummy all_lkps_maps resource (supressing warning messages)
all_lkps_maps_dummy <- build_all_lkps_maps_dummy()

# write to Duckdb database file
db_path <- suppressMessages(
  all_lkps_maps_to_db(
    all_lkps_maps = all_lkps_maps_dummy,
    db_path = tempfile()
  )
)

# connect to Duckdb database
con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

# create named list of tbl_dbi objects
all_lkps_maps_dummy_db <- ukbwranglr::db_tables_to_list(con)
#> Error in dbSendQuery(conn, statement, ...): Invalid connection
#> ℹ Context: rapi_prepare

head(all_lkps_maps_dummy_db$icd10_lkp)
#> Error: object 'all_lkps_maps_dummy_db' not found

# import to R with dplyr::collect()
dplyr::collect(all_lkps_maps_dummy_db$icd10_lkp)
#> Error: object 'all_lkps_maps_dummy_db' not found
```
