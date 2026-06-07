# Disconnect the codeminer workbench

Tears down the in-memory DuckDB connection and clears cached metadata.
Non-connection state (e.g. extracted file paths) is preserved.

## Usage

``` r
codeminer_disconnect()
```

## Value

`NULL`, invisibly.

## See also

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
