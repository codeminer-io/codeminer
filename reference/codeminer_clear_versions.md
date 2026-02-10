# Clear active version selections

Removes version selections from the current session. This covers both
versions pinned explicitly with
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md)
and versions auto-cached on first `"latest"` resolution.

## Usage

``` r
codeminer_clear_versions(lookup = NULL, relationship = NULL, mapping = NULL)
```

## Arguments

- lookup:

  Character vector of code types whose lookup version should be cleared
  (e.g. `"ICD-10"`). `NULL` (default) means no lookup versions are
  cleared unless all arguments are `NULL`.

- relationship:

  Character vector of code types whose relationship version should be
  cleared. `NULL` (default) means no relationship versions are cleared
  unless all arguments are `NULL`.

- mapping:

  Character vector of mapping keys (`"from > to"` format) whose version
  should be cleared. `NULL` (default) means no mapping versions are
  cleared unless all arguments are `NULL`.

## Value

`NULL`, invisibly.

## Details

Called with no arguments, all version selections are cleared and
subsequent queries will re-resolve `"latest"` from the database. To
clear only specific code types, pass them as character vectors.

## See also

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all version selections
codeminer_clear_versions()

# Clear only the ICD-10 lookup version
codeminer_clear_versions(lookup = "ICD-10")

# Clear lookup and relationship versions for SNOMED CT
codeminer_clear_versions(
  lookup = "SNOMED CT",
  relationship = "SNOMED CT"
)
} # }
```
