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

## See also

Other Workbench management:
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
