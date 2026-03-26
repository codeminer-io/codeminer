# Read OPCS-4 coding files into R

Reads the OPCS-4 codes and descriptions from a local copy of the TRUD
release files.

## Usage

``` r
read_opcs4_trud(
  path,
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the OPCS-4 release. Can be:

  - A **zip file** (e.g., from
    [`get_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/get_opcs4_trud.md))

  - An **unzipped directory** containing the codes and titles file

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

A named list with element `opcs4_lkp`, containing:

- `lookup`: a list with `table` (data.table) and `metadata` (list)

The `table` has columns `opcs4_code` and `description`.

## See also

[`add_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/add_opcs4_trud.md),
[`get_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/get_opcs4_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_opcs4_trud()
result <- read_opcs4_trud(path)
result$opcs4_lkp$lookup$table
} # }
```
