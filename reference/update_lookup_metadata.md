# Update lookup table metadata

Updates metadata fields for an existing lookup table without re-adding
the data. Currently supports updating `col_filters`.

## Usage

``` r
update_lookup_metadata(
  code_type,
  lookup_version = "latest",
  ...,
  col_filters = NULL
)
```

## Arguments

- code_type:

  The coding system type (e.g. `"SNOMED CT"`).

- lookup_version:

  The version to update. Use `"latest"` (default) to update the most
  recent version.

- ...:

  These dots are for future extensions and must be empty.

- col_filters:

  Column filter specification to set. See
  [`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
  for the format. Use `NULL` to clear existing filters.

## Value

`TRUE` invisibly if successful.

## See also

[`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md),
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)

Other Database management:
[`update_mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_mapping_metadata.md),
[`update_relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_relationship_metadata.md),
[`validate_database()`](https://codeminer-io.github.io/codeminer/reference/validate_database.md)
