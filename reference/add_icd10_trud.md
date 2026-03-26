# Add ICD-10 lookup table to CodeMiner database

Reads ICD-10 Edition 5 files and adds the lookup table to the active
CodeMiner database. This is a convenience wrapper around
[`read_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_trud.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md).

## Usage

``` r
add_icd10_trud(
  path = get_icd10_trud(),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the ICD-10 release (zip file or unzipped directory). Default
  uses
  [`get_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/get_icd10_trud.md)
  to download the latest release.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_trud.md)
(a named list of tables with metadata).

## See also

[`read_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_trud.md),
[`get_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/get_icd10_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_icd10_trud()
} # }
```
