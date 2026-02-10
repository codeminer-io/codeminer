# Connect to the codeminer workbench

Creates an in-memory DuckDB connection and ATTACHes one or more database
files. The `main` database is attached as read-only (immutable
ontologies). The optional `extra` database is attached as read-write
(user-defined tables). A search path is set so that tables in `extra`
shadow those in `main`.

## Usage

``` r
codeminer_connect(main = NULL, extra = NULL)
```

## Arguments

- main:

  Path to the main (read-only) DuckDB database file. Defaults to the
  path from `CODEMINER_DB_PATH` env var or
  `rappdirs::user_data_dir("codeminer")`.

- extra:

  Optional path to an extra (read-write) DuckDB database file. If the
  file does not exist, it will be created with empty metadata tables.

## Value

The DBI connection object, invisibly.

## Details

If called with no arguments, uses the default database path from the
`CODEMINER_DB_PATH` environment variable (or `rappdirs` default).

## See also

[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
