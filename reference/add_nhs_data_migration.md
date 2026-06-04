# Add NHS Data Migration mapping tables to CodeMiner database

Reads the clinically assured Read V2 and CTV3 to SNOMED CT mapping
tables from the NHS Data Migration release and adds them to the active
CodeMiner database. This is a convenience wrapper around
[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md)
that automatically calls
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
for each table.

## Usage

``` r
add_nhs_data_migration(
  path = get_nhs_data_migration(),
  tables = c("ctv3sctmap2", "rcsctmap2", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source =
    "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
)
```

## Arguments

- path:

  Path to the NHS Data Migration release (zip file or unzipped
  directory). Default uses
  [`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)
  to download the file.

- tables:

  Character vector of table names to add. See
  [`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md)
  for available tables. By default, adds both tables.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md)
(a named list of tables with metadata).

## See also

[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md),
[`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_nhs_data_migration()
add_nhs_data_migration(tables = "ctv3sctmap2")
} # }
```
