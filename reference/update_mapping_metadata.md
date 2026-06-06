# Update mapping table metadata

Updates metadata fields for an existing mapping table without re-adding
the data. Currently supports updating `col_filters`.

## Usage

``` r
update_mapping_metadata(
  from_code_type,
  to_code_type,
  map_version = "latest",
  ...,
  col_filters = NULL
)
```

## Arguments

- from_code_type:

  The source coding system (e.g. `"Read v3"`).

- to_code_type:

  The target coding system (e.g. `"ICD-10"`).

- map_version:

  The version to update. Use `"latest"` (default) to update the most
  recent version.

- ...:

  These dots are for future extensions and must be empty.

- col_filters:

  Column filter specification to set. See
  [`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md)
  for the format. Use `NULL` to clear existing filters.

## Value

`TRUE` invisibly if successful.

## See also

[`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md),
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)

Other Database management:
[`migrate_database()`](https://codeminer-io.github.io/codeminer/reference/migrate_database.md),
[`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md),
[`update_relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_relationship_metadata.md)
