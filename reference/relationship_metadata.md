# Create relationship metadata

Generate the required metadata for a relationship table. This is mainly
used to generate the necessary metadata when adding a new relationship
table to the database with
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md).

## Usage

``` r
relationship_metadata(
  code_type,
  relationship_version = "v0",
  ...,
  from_col = "from",
  to_col = "to",
  type_col = "type",
  child_parent_relationship_code = "is a",
  relationship_source = NA_character_
)
```

## Arguments

- code_type:

  The type of coding system (e.g., ICD-10, SNOMED-CT)

- relationship_version:

  The version of the relationship metadata (default: "v0")

- ...:

  These dots are for future extensions and must be empty.

- from_col:

  The column name for the source code in the relationship (default:
  "from")

- to_col:

  The column name for the target code in the relationship (default:
  "to")

- type_col:

  The column name for the relationship type (default: "type")

- child_parent_relationship_code:

  The code value that indicates a child-parent (is-a) relationship in
  the `type_col` column (default: "is a")

- relationship_source:

  The source of the relationship metadata (default: `NA_character_`)

## Value

A list containing the relationship metadata

## See also

[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)

## Examples

``` r
relationship_metadata("SNOMED-CT", relationship_version = "2023")
#> $relationship_table_name
#> [1] "SNOMED-CT_relationship_2023"
#> 
#> $code_type
#> [1] "SNOMED-CT"
#> 
#> $relationship_version
#> [1] "2023"
#> 
#> $from_col
#> [1] "from"
#> 
#> $to_col
#> [1] "to"
#> 
#> $type_col
#> [1] "type"
#> 
#> $child_parent_relationship_code
#> [1] "is a"
#> 
#> $relationship_source
#> [1] NA
#> 
```
