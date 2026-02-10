# Update relationship table metadata

Updates metadata fields for an existing relationship table without
re-adding the data. Currently supports updating `col_filters`.

## Usage

``` r
update_relationship_metadata(
  code_type,
  relationship_version = "latest",
  ...,
  col_filters = NULL
)
```

## Arguments

- code_type:

  The coding system type (e.g. `"SNOMED CT"`).

- relationship_version:

  The version to update. Use `"latest"` (default) to update the most
  recent version.

- ...:

  These dots are for future extensions and must be empty.

- col_filters:

  Column filter specification to set. See
  [`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md)
  for the format. Use `NULL` to clear existing filters.

## Value

`TRUE` invisibly if successful.

## See also

[`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md),
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)

Other Database management:
[`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md),
[`update_mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_mapping_metadata.md)
