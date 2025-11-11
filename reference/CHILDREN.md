# Get descendents for a code

Retrieves children codes for a given set of codes (including the codes
themselves). Note that currently it is not possible to retrieve children
codes for certain clinical coding systems, such as Read 3.

## Usage

``` r
CHILDREN(
  codes,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
)
```

## Arguments

- codes:

  character. A vector of code strings to retrieve child codes for.

- code_type:

  character. Type of clinical code system to be searched. Depends on
  what is available in the lookup tables. See
  [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  on how to add new lookup tables. This can also be configured through
  the `codeminer.code_type` option.

- version:

  character. Version of the lookup table to use. Default: `"latest"`.
  Can be configured through the `codeminer.lookup_version` option.

- codes_only:

  bool. If `TRUE`, return a character vector of *unique* codes. If
  `FALSE` (default), return a data frame of all results including code
  descriptions (useful for manual validation).

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

A data frame

## See also

[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md)

Other Clinical code lookups and mappings:
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

## Examples

``` r
# TODO
```
