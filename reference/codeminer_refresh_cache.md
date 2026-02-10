# Refresh the metadata cache

Re-reads metadata tables from all attached databases and updates the
internal cache. Called automatically by
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md)
and after write operations.

## Usage

``` r
codeminer_refresh_cache()
```

## Value

`NULL`, invisibly.

## Details

This refreshes the *metadata inventory* (which tables and versions exist
in the database). It does not affect version selections — use
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md)
to reset which version is used for each code type.

## See also

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
