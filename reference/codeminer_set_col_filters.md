# Pin column filters for the session

Overrides the default column filters defined in table metadata. Pinned
filters persist until cleared with
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
or
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md).

## Usage

``` r
codeminer_set_col_filters(
  col_filters = NULL,
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
)
```

## Arguments

- col_filters:

  A whole type-layered filter object — the shape returned by
  [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
  i.e. `list(lookup =, relationship =, mapping =)` — or `NA` to disable
  all filtering (pins *and* metadata defaults) for the session. Cannot
  be combined with the per-type arguments.

- lookup:

  Named list of column filters for lookup tables, keyed by code type.
  Each value is a named list of `column_name = c(values)` pairs, or `NA`
  to un-filter that table. E.g.
  `list("SNOMED CT" = list(active_concept = c("1")))`.

- relationship:

  Named list of column filters for relationship tables, keyed by code
  type.

- mapping:

  Named list of column filters for mapping tables, keyed by
  `"from > to"` pairs. E.g.
  `list("Read 3 > ICD-10" = list(mapping_status = c("E", "G")))`.

## Value

The current pinned col_filters (a list), invisibly.

## Details

A pin **replaces** the metadata-defined default filters for its table
wholesale (it does not merge column-by-column) — a message lists any
default columns the pin drops. To tweak one column while keeping the
rest, amend
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
output and pin that.

Pinned filters apply when queries run with `col_filters = "default"`; an
explicit `col_filters` argument on a query function overrides the pin
for that call (again whole-key, per table).

New pins are merged with existing ones by table key. To replace all
pins, call
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
first. Pins that match no registered table, column, or value trigger a
warning.

## See also

[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md),
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md),
[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pin lookup filters — only return active SNOMED concepts
codeminer_set_col_filters(
  lookup = list("SNOMED CT" = list(active_concept = c("1")))
)

# Pin mapping filters
codeminer_set_col_filters(
  mapping = list("Read 3 > ICD-10" = list(mapping_status = c("E", "G")))
)

# Amend the registered defaults and pin the result
cf <- get_col_filters()
cf$lookup[["SNOMED CT"]]$moduleId_concept <- "999000011000001104"
codeminer_set_col_filters(col_filters = cf)

# Disable all filtering for the session
codeminer_set_col_filters(NA)

# Clear all filter pins (back to metadata defaults)
codeminer_clear_col_filters()
} # }
```
