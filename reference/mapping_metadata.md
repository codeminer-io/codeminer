# Create mapping metadata

Generate the required metadata for a mapping table. This is mainly used
to generate the necessary metadata when adding a new mapping table to
the database with
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md).

## Usage

``` r
mapping_metadata(
  from_code_type,
  to_code_type,
  map_version = "v0",
  ...,
  from_col = "from",
  to_col = "to"
)
```

## Arguments

- from_code_type:

  The type of coding system for the source codes (e.g., ICD-10,
  SNOMED-CT)

- to_code_type:

  The type of coding system for the target codes (e.g., ICD-10,
  SNOMED-CT)

- map_version:

  The version of the mapping metadata (default: "v0")

- ...:

  These dots are for future extensions and must be empty.

- from_col:

  The column name for the source codes (default: "from")

- to_col:

  The column name for the target codes (default: "to")

## Value

A list containing the mapping metadata

## See also

[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)

## Examples

``` r
mapping_metadata("ICD-10", "SNOMED-CT", map_version = "2023")
#> $mapping_table_name
#> [1] "ICD-10_SNOMED-CT_2023"
#> 
#> $from_code_type
#> [1] "ICD-10"
#> 
#> $to_code_type
#> [1] "SNOMED-CT"
#> 
#> $map_version
#> [1] "2023"
#> 
#> $from_col
#> [1] "from"
#> 
#> $to_col
#> [1] "to"
#> 
```
