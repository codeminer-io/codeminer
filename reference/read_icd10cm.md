# Read an ICD-10-CM release into lookup and relationship tables

Reads the fixed-width ICD-10-CM "order file" published by the National
Center for Health Statistics (NCHS) — one row per code, including the
non-billable category and subcategory headers — and returns an ICD-10-CM
lookup table plus a parent-child relationship table for use with
CodeMiner.

## Usage

``` r
read_icd10cm(
  path = get_icd10cm(),
  version = NULL,
  source = "https://www.cdc.gov/nchs/icd/icd-10-cm/files.html"
)
```

## Arguments

- path:

  Path to an ICD-10-CM release: either the `.zip` published by NCHS (the
  order file is located inside it) or the order `.txt` file itself.
  Default uses
  [`get_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/get_icd10cm.md)
  to download the latest release.

- version:

  Character string for the version label. If `NULL` (default), the
  8-digit effective date is taken from the file name (as written by
  [`get_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/get_icd10cm.md));
  an error is raised if none can be found.

- source:

  Character string for the data source URL or description.

## Value

A named list with elements:

- `icd10cm_lkp`: list with `lookup` -\> `list(table, metadata)`

- `icd10cm_relationship`: list with `relationship` -\>
  `list(table, metadata)`

The lookup `table` has columns:

- `icd10cm_code`: the code without its decimal point (e.g. `E1165`);
  this is the primary code column.

- `icd10cm_code_dotted`: the code as conventionally written (`E11.65`).

- `billable`: `"1"` for a billable (leaf) code, `"0"` for a category or
  subcategory header. Exposed as a column filter; both are included by
  default.

- `short_description`, `description`: the short and long descriptions.

- `category`: the ICD-10-CM chapter.

## Details

Codes are stored exactly as published: placeholder `X` characters (e.g.
`T07XXXA`) are part of the code and are retained. The hierarchy is
derived from code prefixes, so a code whose shorter prefixes are not
themselves codes attaches to the nearest prefix that is (`T07XXXA` -\>
`T07`).

## Attribution

ICD-10-CM is published by NCHS/CDC and is available free of charge at
<https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files
does not imply endorsement by CDC, HHS or the US Government.

## See also

[`add_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/add_icd10cm.md),
[`get_icd10cm()`](https://codeminer-io.github.io/codeminer/reference/get_icd10cm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_icd10cm()
result <- read_icd10cm(path)
result$icd10cm_lkp$lookup$table
result$icd10cm_relationship$relationship$table
} # }
```
