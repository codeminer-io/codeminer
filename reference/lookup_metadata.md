# Create lookup metadata

Generate the required metadata for a lookup table. This is mainly used
to generate the necessary metadata when adding a new lookup table to the
database with
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md).

## Usage

``` r
lookup_metadata(
  code_type,
  lookup_version = "v0",
  ...,
  lookup_code_col = "code",
  lookup_description_col = "description",
  lookup_source = NA_character_,
  preferred_description_col = NA_character_,
  preferred_description_indicator = NA_character_
)
```

## Arguments

- code_type:

  The type of coding system (e.g., ICD-10, SNOMED-CT)

- lookup_version:

  The version of the lookup metadata (default: "v0")

- ...:

  These dots are for future extensions and must be empty.

- lookup_code_col:

  The column name for the lookup code (default: "code")

- lookup_description_col:

  The column name for the lookup description (default: "description")

- lookup_source:

  The source of the lookup metadata (default: `NA_character_`)

- preferred_description_col:

  The name of the column that indicates the preferred description. This
  is useful for lookup tables that have multiple descriptions for the
  same code (default: `NA_character_`).

- preferred_description_indicator:

  The value in the `preferred_description_col` column that indicates the
  preferred description (default: `NA_character_`)

## Value

A list containing the lookup metadata

## See also

[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)

## Examples

``` r
lookup_metadata("ICD-10", lookup_version = "2023")
#> $lookup_table_name
#> [1] "ICD-10_2023"
#> 
#> $code_type
#> [1] "ICD-10"
#> 
#> $lookup_version
#> [1] "2023"
#> 
#> $lookup_code_col
#> [1] "code"
#> 
#> $lookup_description_col
#> [1] "description"
#> 
#> $lookup_source
#> [1] NA
#> 
#> $preferred_description_col
#> [1] NA
#> 
#> $preferred_description_indicator
#> [1] NA
#> 
```
