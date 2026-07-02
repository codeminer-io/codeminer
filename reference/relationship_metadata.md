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
  type_col = NA_character_,
  child_parent_relationship_code = NA_character_,
  relationship_source = NA_character_,
  col_filters = NULL
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

  The column name for the relationship type, or `NA` (default) when the
  relationship table is purely hierarchical (every edge is child-parent,
  so there is no type column and no type filtering).

- child_parent_relationship_code:

  The value in `type_col` that indicates a child-parent (is-a)
  relationship, or `NA` (default) for a purely hierarchical table. Must
  be `NA` if and only if `type_col` is `NA`.

- relationship_source:

  The source of the relationship metadata (default: `NA_character_`)

- col_filters:

  Optional column filter specification. A named list where each element
  is a list with `values` (all valid values) and `defaults` (default
  filter values), plus the optional `description` (single string) and
  `value_labels` (named character vector, names a subset of `values`)
  documentation fields. See
  [`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
  for the full format. `NULL` (default) means no column filters.

## Value

A list containing the relationship metadata

## See also

[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)

## Examples

``` r
# Purely hierarchical table (no type column): `type_col` and
# `child_parent_relationship_code` both default to `NA`.
relationship_metadata("ICD-10", relationship_version = "2023")
#> $relationship_table_name
#> [1] "ICD-10_relationship_2023"
#> 
#> $code_type
#> [1] "ICD-10"
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
#> [1] NA
#> 
#> $child_parent_relationship_code
#> [1] NA
#> 
#> $relationship_source
#> [1] NA
#> 
#> $col_filters
#> [1] NA
#> 

# Multi-type table: name the type column and the value selecting is-a edges.
relationship_metadata(
  "SNOMED-CT",
  relationship_version = "2023",
  type_col = "typeId",
  child_parent_relationship_code = "116680003"
)
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
#> [1] "typeId"
#> 
#> $child_parent_relationship_code
#> [1] "116680003"
#> 
#> $relationship_source
#> [1] NA
#> 
#> $col_filters
#> [1] NA
#> 
```
