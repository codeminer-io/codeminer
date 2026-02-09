# Remove a lookup table from the database

Removes a lookup table and its metadata entry from the database.

## Usage

``` r
remove_lookup_table(code_type, lookup_version)
```

## Arguments

- code_type:

  The coding system type (e.g. `"ICD-10"`).

- lookup_version:

  The version to remove (e.g. `"UKB v4"`).

## Value

`TRUE` invisibly if successful.

## See also

[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
[`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
