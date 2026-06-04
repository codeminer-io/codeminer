# Read Read 2 coding files into R

Reads the Read V2 lookup and relationship tables from a local copy of
the NHS Read Browser release (TRUD item 8), which ships the canonical
Read V2 terminology files in FoxPro DBF format under `Standard/V2/`.

## Usage

``` r
read_read2_trud(
  path,
  tables = c("read2_lkp", "read2_relationship"),
  version = NULL,
  source =
    "https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/9/items/8/releases"
)
```

## Arguments

- path:

  Path to the NHS Read Browser release. Can be:

  - A **zip file** (e.g., from
    [`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md))

  - An **unzipped directory** containing the `Standard/V2` subdirectory

- tables:

  Character vector of table names to read. Available tables:

  - `"read2_lkp"` — Read V2 lookup (codes, descriptions, term type,
    status)

  - `"read2_relationship"` — parent-child hierarchy derived from code
    structure (requires `"read2_lkp"`)

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

The lookup is built by joining `DESC.DBF` (codes, term ids, `TERMTYPE`,
`CCSTATUS`) with `Term.dbf` (the 30- and 60-character term forms) and
left-joining `TERM198.DBF` (the longer term forms, where present). A
composed `term` column prefers `TERM198` when available and falls back
to `TERM60`.

All rows from the source `DESC.DBF` table are retained, including codes
with `TERMTYPE = "S"` (synonyms) and `CCSTATUS != "C"` (non-active
codes). Query-time filtering to active codes and preferred terms is
handled by the `preferred_description_col` and `col_filters` entries in
the lookup metadata.

The Read 2 ↔ CTV3 mapping tables (`rctctv3map_uk`, `ctv3rctmap_uk`) live
in the NHS Data Migration release (TRUD item 9) and are available via
[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md).

## See also

[`add_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read2_trud.md),
[`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md),
[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_read2_trud()
result <- read_read2_trud(path)
result$read2_lkp$lookup$table
result$read2_relationship$relationship$table
} # }
```
