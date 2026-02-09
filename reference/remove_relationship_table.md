# Remove a relationship table from the database

Removes a relationship table and its metadata entry from the database.

## Usage

``` r
remove_relationship_table(code_type, relationship_version)
```

## Arguments

- code_type:

  The coding system type (e.g. `"ICD-10"`).

- relationship_version:

  The version to remove (e.g. `"UKB v4"`).

## Value

`TRUE` invisibly if successful.

## See also

[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md),
[`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md)
