# Read Read 2 coding files into R

Reads the Read V2 lookup and Read 2/CTV3 cross-mapping tables from a
local copy of the NHS Data Migration release (TRUD item 9).

## Usage

``` r
read_read2_trud(
  path,
  tables = c("read2_lkp", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source =
    "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
)
```

## Arguments

- path:

  Path to the NHS Data Migration release. Can be:

  - A **zip file** (e.g., from
    [`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md))

  - An **unzipped directory** containing the `Mapping Tables`
    subdirectory

- tables:

  Character vector of table names to read. Available tables:

  - `"read2_lkp"` — Read V2 lookup (codes and descriptions)

  - `"read2_ctv3"` — Read V2 to CTV3 cross-mapping

  - `"ctv3_read2"` — CTV3 to Read V2 cross-mapping

  By default, all three tables are read.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

A named list with elements corresponding to requested tables, each
containing:

- `lookup` or `mapping`: a list with `table` (data.table) and `metadata`
  (list)

## See also

[`add_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read2_trud.md),
[`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_read2_trud()
result <- read_read2_trud(path)
result$read2_lkp$lookup$table
result$read2_ctv3$mapping$table
result$ctv3_read2$mapping$table
} # }
```
