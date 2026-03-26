# Add Read 3 (CTV3) tables to CodeMiner database

Reads Read Codes Version 3 (CTV3) files and adds lookup and relationship
tables to the active CodeMiner database. This is a convenience wrapper
around
[`read_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read3_trud.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md).

## Usage

``` r
add_read3_trud(
  path = get_read3_trud(),
  tables = c("read3_lkp", "read3_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the Read 3 release (zip file or unzipped directory). Default
  uses
  [`get_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read3_trud.md)
  to download the latest release.

- tables:

  Character vector of table names to add. See
  [`read_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read3_trud.md)
  for available tables. By default, adds both tables.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read3_trud.md)
(a named list of tables with metadata).

## See also

[`read_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read3_trud.md),
[`get_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read3_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_read3_trud()
add_read3_trud(tables = "read3_lkp")
} # }
```
