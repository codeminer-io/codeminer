# Read NHS Data Migration mapping tables into R

Reads the clinically assured Read V2 and CTV3 to SNOMED CT mapping
tables from the NHS Data Migration release.

## Usage

``` r
read_nhs_data_migration(
  path,
  tables = c("ctv3sctmap2", "rcsctmap2"),
  version = NULL,
  source =
    "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
)
```

## Arguments

- path:

  Path to the NHS Data Migration release. Can be:

  - A **zip file** (e.g., from
    [`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md))

  - An **unzipped directory** containing the `Mapping Tables`
    subdirectory

- tables:

  Character vector of table names to read. Available tables:

  - `"ctv3sctmap2"` — CTV3 (Read 3) to SNOMED CT clinically assured
    mapping

  - `"rcsctmap2"` — Read V2 to SNOMED CT clinically assured mapping

  By default, both tables are read.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

A named list with elements corresponding to requested tables, each
containing:

- `mapping`: a list with `table` (data.table) and `metadata` (list)

## See also

[`add_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/add_nhs_data_migration.md),
[`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_nhs_data_migration()
result <- read_nhs_data_migration(path)
result$ctv3sctmap2$mapping$table
result$rcsctmap2$mapping$table
} # }
```
