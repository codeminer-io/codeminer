# Temporarily override column filters

Sets column filter pins for the duration of the supplied code block,
then restores the previous state. This is useful when you need different
filters for a group of calls without permanently changing session state.

## Usage

``` r
with_col_filters(code, lookup = NULL, relationship = NULL, mapping = NULL)
```

## Arguments

- code:

  Code to execute with the temporary filters.

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

The result of evaluating `code`.

## See also

[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md)

Other Workbench management:
[`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md),
[`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md),
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md),
[`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md),
[`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md),
[`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md),
[`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md),
[`codeminer_snapshot_extra()`](https://codeminer-io.github.io/codeminer/reference/codeminer_snapshot_extra.md),
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Temporarily include inactive SNOMED concepts
with_col_filters(
  {
    CODES("all", type = "SNOMED CT")
  },
  lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))
)
} # }
```
