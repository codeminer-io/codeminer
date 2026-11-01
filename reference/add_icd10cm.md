# Add ICD-10-CM to CodeMiner database

Reads an ICD-10-CM release and adds the lookup and relationship tables
to the active CodeMiner database. This is a convenience wrapper around
[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md).

## Usage

``` r
add_icd10cm(
  path = get_icd10cm(),
  version = NULL,
  source = "https://www.cdc.gov/nchs/icd/icd-10-cm/files.html"
)
```

## Arguments

- path:

  Path to an ICD-10-CM release (`.zip` or order `.txt` file). Default
  uses
  [`get_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/get_icd10cm.md)
  to download the latest release.

- version:

  Character string for the version label. If `NULL` (default), the
  8-digit effective date is taken from the file name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md)
(a named list of tables with metadata), or `NULL` if all tables already
exist.

## Attribution

ICD-10-CM is published by NCHS/CDC and is available free of charge at
<https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files
does not imply endorsement by CDC, HHS or the US Government.

## See also

[`read_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/read_icd10cm.md),
[`get_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/get_icd10cm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database()
add_icd10cm()

# A specific release
add_icd10cm(get_icd10cm(release = "20250401"))
} # }
```
