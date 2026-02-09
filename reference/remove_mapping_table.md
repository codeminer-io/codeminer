# Remove a mapping table from the database

Removes a mapping table and its metadata entry from the database.

## Usage

``` r
remove_mapping_table(from_code_type, to_code_type, map_version)
```

## Arguments

- from_code_type:

  The source coding system (e.g. `"Read 3"`).

- to_code_type:

  The target coding system (e.g. `"ICD-10"`).

- map_version:

  The version to remove (e.g. `"UKB v4"`).

## Value

`TRUE` invisibly if successful.

## See also

[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
[`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md)
