# Get children for SNOMED codes

Get children for SNOMED codes

## Usage

``` r
get_children_sct(
  codes,
  standardise_output = TRUE,
  include_self = TRUE,
  include_descendants = TRUE,
  preferred_description_only = TRUE
)
```

## Arguments

- codes:

  Character vector of SNOMED codes.

- standardise_output:

  If `TRUE` (default) return a data frame with columns 'code',
  'description' and 'code_type'.

- include_self:

  If `TRUE` (default) include input codes in the result.

- include_descendants:

  If `TRUE` (default) return all descendant codes, as well as immediate
  children.

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

A dataframe

## See also

[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

Other Clinical code lookups and mappings:
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)
