# Get parents for SNOMED codes

Get parents for SNOMED codes

## Usage

``` r
get_parents_sct(
  codes,
  standardise_output = TRUE,
  include_self = TRUE,
  include_ancestors = TRUE,
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

- include_ancestors:

  If `TRUE` (default) return all ancestor codes, as well as immediate
  parents.

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

A dataframe

## See also

[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md)

Other Clinical code lookups and mappings:
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md)
