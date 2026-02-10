# Pin table versions for the session

Overrides the default "latest" version resolution for lookup,
relationship, and/or mapping tables. Pinned versions persist until
cleared with
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md)
or
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md).

## Usage

``` r
codeminer_set_version(lookup = NULL, relationship = NULL, mapping = NULL)
```

## Arguments

- lookup:

  Named character vector of lookup versions, keyed by code type. E.g.
  `c("ICD-10" = "v42", "Read 3" = "v1")`.

- relationship:

  Named character vector of relationship versions, keyed by code type.

- mapping:

  Named character vector of mapping versions, keyed by `"from > to"`
  pairs. E.g. `c("Read 3 > ICD-10" = "v1")`.

## Value

The current pinned versions (a list), invisibly.

## Details

Pinned versions only affect "latest" resolution. Explicit version
arguments on query functions (e.g. `CODES(..., lookup_version = "v1")`)
always take precedence.

Versions are also auto-cached the first time `"latest"` is resolved for
a given code type. Calling `codeminer_set_version()` overrides any
auto-cached version.

New pins are merged with existing ones. To replace all pins, call
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md)
first.

## See also

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pin lookup versions for multiple code types
codeminer_set_version(
  lookup = c("ICD-10" = "v42", "Read 3" = "v1")
)

# Pin mapping version for a specific pair
codeminer_set_version(
  mapping = c("Read 3 > ICD-10" = "v1")
)

# Clear all pins
codeminer_clear_versions()
} # }
```
