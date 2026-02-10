# Pin column filters for the session

Overrides the default column filters defined in table metadata. Pinned
filters persist until cleared with
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
or
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md).

## Usage

``` r
codeminer_set_col_filters(lookup = NULL, relationship = NULL, mapping = NULL)
```

## Arguments

- lookup:

  Named list of column filters for lookup tables, keyed by code type.
  Each value is a named list of `column_name = c(values)` pairs. E.g.
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

Pinned filters only affect `col_filters = "default"` resolution.
Explicit `col_filters` arguments on query functions always take
precedence.

New pins are merged with existing ones. To replace all pins, call
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
first.

## See also

[`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md),
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
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

# Clear all filter pins
codeminer_clear_col_filters()
} # }
```
