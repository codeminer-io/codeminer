# Temporarily override column filters

Applies column filters for the duration of the supplied expression, then
restores the previous state (even on error). This is useful when you
need different filters for a group of calls without permanently changing
session state. For a single call, prefer the `col_filters` argument on
the query function itself (see
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md))
— it has the same semantics, scoped to that call.

## Usage

``` r
with_col_filters(col_filters, code)
```

## Arguments

- col_filters:

  The filters to apply: a type-layered list or
  [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
  object (each table entry replaces that table's pins/defaults
  wholesale), or `NULL` / `NA` for no filtering at all.

- code:

  Code to execute with the temporary filters. The scope covers query
  *construction*: every codeminer read made while `code` evaluates
  resolves against the temporary filters.

## Value

The result of evaluating `code`.

## Details

Note the argument order (filters first, code last, as in
`withr::with_*()`): wrap the expression rather than piping into this
function.

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
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Temporarily include inactive SNOMED concepts
with_col_filters(
  list(lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))),
  CODES("all", type = "SNOMED CT")
)

# Temporarily disable all filtering
with_col_filters(NA, CODES("all", type = "SNOMED CT"))
} # }
```
