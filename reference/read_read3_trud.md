# Read Read 3 (CTV3) coding files into R

Reads the Read Codes Version 3 (CTV3) lookup and relationship tables
from a local copy of the TRUD release files (item 19).

## Usage

``` r
read_read3_trud(
  path,
  tables = c("read3_lkp", "read3_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the Read 3 release. Can be:

  - A **zip file** (e.g., from
    [`get_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read3_trud.md))

  - An **unzipped directory** containing the `V3` subdirectory

- tables:

  Character vector of table names to read. Available tables:

  - `"read3_lkp"` — lookup table of active Read 3 codes with
    descriptions

  - `"read3_relationship"` — hierarchy table (`V3hier.v3`)

  By default, both tables are read.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

A named list with elements corresponding to requested tables, each
containing:

- `lookup` or `relationship`: a list with `table` (data.table) and
  `metadata` (list)

## Details

Only active codes (status not `"R"`) with preferred (`"P"`) and clinical
(`"C"`) descriptions are included in the lookup table.

## See also

[`add_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read3_trud.md),
[`get_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read3_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_read3_trud()
result <- read_read3_trud(path)
result$read3_lkp$lookup$table
result$read3_relationship$relationship$table
} # }
```
