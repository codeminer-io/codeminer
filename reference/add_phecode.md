# Add Phecode 1.2 tables to CodeMiner database

Reads Phecode 1.2 definitions and/or ICD mapping files and adds them to
the active CodeMiner database. This is a convenience wrapper around
[`read_phecode()`](https://codeminer-io.github.io/codeminer/reference/read_phecode.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
and
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
for each table.

## Usage

``` r
add_phecode(
  lkp_path = NULL,
  icd10_map_path = NULL,
  icd9_map_path = NULL,
  version = "v1.2",
  source = "https://phewascatalog.org/phecodes"
)
```

## Arguments

- lkp_path:

  Path to the Phecode 1.2 definitions CSV. If `NULL`, skipped.

- icd10_map_path:

  Path to the Phecode 1.2 to ICD-10 mapping CSV. If `NULL`, skipped.

- icd9_map_path:

  Path to the Phecode 1.2 to ICD-9 mapping `.rda` file. If `NULL`,
  skipped.

- version:

  Character string for the version label (default: `"v1.2"`).

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_phecode()`](https://codeminer-io.github.io/codeminer/reference/read_phecode.md)
(a named list of tables with metadata).

## See also

[`read_phecode()`](https://codeminer-io.github.io/codeminer/reference/read_phecode.md),
[`get_phecode()`](https://codeminer-io.github.io/codeminer/reference/get_phecode.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")

# Download and add all phecode tables
paths <- get_phecode()
add_phecode(
  lkp_path = paths$lkp,
  icd10_map_path = paths$icd10_map,
  icd9_map_path = paths$icd9_map
)
} # }

# Example with dummy data
if (FALSE) { # \dontrun{
build_database(db_path = tempfile(fileext = ".db"))
add_phecode(
  lkp_path = dummy_phecode_lkp_path(),
  icd10_map_path = dummy_icd10_phecode_map_path()
)
} # }
```
