# Read ICD-10 coding files into R

Reads the ICD-10 Edition 5 codes and descriptions from a local copy of
the TRUD release files.

## Usage

``` r
read_icd10_trud(
  path,
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the ICD-10 release. Can be:

  - A **zip file** (e.g., from
    [`get_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/get_icd10_trud.md))

  - An **unzipped directory** containing the `Content` subdirectory

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

A named list with element `icd10_lkp`, containing:

- `lookup`: a list with `table` (data.table) and `metadata` (list)

The `table` has columns including `CODE`, `DESCRIPTION`, and others from
the source file. `DESCRIPTION` is augmented with `MODIFIER_4` or
`MODIFIER_5` where present.

## See also

[`add_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/add_icd10_trud.md),
[`get_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/get_icd10_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_icd10_trud()
result <- read_icd10_trud(path)
result$icd10_lkp$lookup$table
} # }
```
