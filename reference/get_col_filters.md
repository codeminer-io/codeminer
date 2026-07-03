# Extract column filters from database metadata

Reads the registered `col_filters` from the metadata tables of the
connected database. Returns a nested list keyed by table type and table
key (code type or mapping pair). This reflects the **registered
defaults**, not the active session pins — use
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
to inspect the currently pinned state.

## Usage

``` r
get_col_filters(defaults_only = TRUE)
```

## Arguments

- defaults_only:

  Logical. If `TRUE` (default), return the applied default filters — the
  shape you amend and pass back to filter a query. If `FALSE`, return
  the full specification (all `values`, the `defaults`, and any
  `description` / `value_labels`), useful for building filter UIs.

## Value

A `codeminer_col_filters` object (a list with entries for `lookup`,
`mapping`, and `relationship`). Each entry is a named list keyed by code
type (or `"from > to"` for mappings), containing either:

- If `defaults_only = TRUE`: a flat `list(col = c(default_values))`

- If `defaults_only = FALSE`: a full
  `list(col = list(values = ..., defaults = ..., description = ..., value_labels = ...))`

The `defaults_only = TRUE` form is the shape accepted by the
`col_filters` argument on query functions (see
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
and
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
— amend it with plain assignment and pass it back. Each column's default
is its applied value set (a column included in full by default lists all
of its values), so passing the output back unchanged reproduces the
default query exactly. Narrow a column by assigning a subset (e.g.
`cf$lookup[["SNOMED CT"]]$moduleId_concept <- "999000011000001104"`),
drop a column's filter with `NA`, or clear a whole table with `NA`.
Returns an empty object if no database is connected.

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
