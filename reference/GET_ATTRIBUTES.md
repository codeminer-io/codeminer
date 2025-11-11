# Get attributes for a set of SNOMED codes

See examples (credit to
[snomedizer](https://snomedizer.web.app/articles/snomedizer.html)).

## Usage

``` r
GET_ATTRIBUTES(
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

A data frame

## See also

Other Clinical code lookups and mappings:
[`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/CHILDREN.md),
[`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md),
[`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/HAS_ATTRIBUTES.md),
[`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md),
[`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md),
[`get_children_sct()`](https://codeminer-io.github.io/codeminer/reference/get_children_sct.md),
[`get_parents_sct()`](https://codeminer-io.github.io/codeminer/reference/get_parents_sct.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Body sites that can be affected by Enterobacteriaceae infections
enterobacteriaceae_infections <- HAS_ATTRIBUTES(
    CHILDREN("106544002 << Family Enterobacteriaceae (organism) >>",
             code_type = "sct"),
    relationship_type = "246075003 << Causative agent (attribute) >>") %AND%
  CHILDREN("40733004 << Infectious disease (disorder) >>", code_type = "sct")

GET_ATTRIBUTES(enterobacteriaceae_infections,
               relationship_type = "363698007 << Finding site (attribute) >>")
} # }
```
