# Add Read 2 tables to CodeMiner database

Reads Read V2 lookup, relationship, and cross-mapping files from the NHS
Data Migration release and adds them to the active CodeMiner database.
This is a convenience wrapper around
[`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md),
and
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md).

## Usage

``` r
add_read2_trud(
  path = get_read2_trud(),
  tables = c("read2_lkp", "read2_relationship", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source =
    "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
)
```

## Arguments

- path:

  Path to the NHS Data Migration release (zip file or unzipped
  directory). Default uses
  [`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md)
  to download the latest release.

- tables:

  Character vector of table names to add. See
  [`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md)
  for available tables. By default, adds all three tables.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md)
(a named list of tables with metadata).

## See also

[`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md),
[`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_read2_trud()
add_read2_trud(tables = "read2_lkp")
} # }
```
