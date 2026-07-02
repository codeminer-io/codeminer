# Extract column filters from database metadata

Reads `col_filters` from all metadata tables in the connected database.
Returns a nested list keyed by table type and table key (code type or
mapping pair).

## Usage

``` r
get_col_filters(defaults_only = TRUE)
```

## Arguments

- defaults_only:

  Logical. If `TRUE` (default), return only the default filter values.
  If `FALSE`, return the full specification including all available
  values (useful for Shiny UI checkboxes).

## Value

A `codeminer_col_filters` object (a list with entries for `lookup`,
`mapping`, and `relationship`). Each entry is a named list keyed by code
type (or `"from > to"` for mappings), containing either:

- If `defaults_only = TRUE`: a flat `list(col = c(default_values))`

- If `defaults_only = FALSE`: a full
  `list(col = list(values = ..., defaults = ...))`

The `defaults_only = TRUE` form is the shape accepted by the
`col_filters` argument on query functions (see
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
and
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
— amend it with plain assignment and pass it back. Returns an empty
object if no database is connected.

## See also

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
