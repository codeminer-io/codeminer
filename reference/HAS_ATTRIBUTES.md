# Get SNOMED codes with a specific set of one or more attributes

Optionally filtered for specific relationship types. See examples
(credit to
[snomedizer](https://snomedizer.web.app/articles/snomedizer.html)).

## Usage

``` r
HAS_ATTRIBUTES(
  attribute_codes,
  relationship_type = NULL,
  preferred_description_only = TRUE
)
```

## Arguments

- attribute_codes:

  Character vector of SNOMED codes.

- relationship_type:

  Character vector of SNOMED codes.

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

A dataframe

## See also

Other Clinical code lookups and mappings:
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`GET_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/GET_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Conditions associated with multiple sclerosis
HAS_ATTRIBUTES("24700007", relationship_type = "42752001")

# Medicines with active ingredient timolol maleate
HAS_ATTRIBUTES("75359005", relationship_type = "10362801000001104")

# Medicines with active ingredient beta blocker
HAS_ATTRIBUTES(CHILDREN("373254001", code_type = "sct"), relationship_type = "10362801000001104")

# Conditions that are caused by bacteria belonging to Enterobacteriaceae
HAS_ATTRIBUTES(CHILDREN("106544002", code_type = "sct"), relationship_type = "246075003")

# Infectious conditions that are caused by bacteria belonging to Enterobacteriaceae
HAS_ATTRIBUTES(CHILDREN("106544002", code_type = "sct"), relationship_type = "246075003") %AND%
  CHILDREN("40733004", code_type = "sct")
} # }
```
