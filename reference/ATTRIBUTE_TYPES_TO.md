# Find attribute types that point to a set of codes

Find attribute types that point to a set of codes

## Usage

``` r
ATTRIBUTE_TYPES_TO(codes, preferred_description_only = TRUE)
```

## Arguments

- codes:

  Codes to get attribute types for

- preferred_description_only:

  logical. If `TRUE`, only returns the preferred description for each
  code. Default: `FALSE`.

## Value

Data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# sct for acute iritis
acute_iritis <- CODES("29050005", "sct")

# attribute types pointing to this code
attribute_types <- ATTRIBUTE_TYPES_TO(acute_iritis)
print(attribute_types)

# codes with acute iritis as an attribute, with these attributes type(s)
HAS_ATTRIBUTES(acute_iritis, attribute_types)
} # }
```
