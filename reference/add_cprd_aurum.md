# Add CPRD Aurum code browser tables to CodeMiner database

Reads a CPRD Aurum code browser release and adds the medcode and
prodcode lookup tables, and their mappings to SNOMED CT and Read v2, to
the active CodeMiner database. This is a convenience wrapper around
[`read_cprd_aurum()`](https://codeminer-io.github.io/codeminer/reference/read_cprd_aurum.md)
that calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
and
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md).
Tables that already exist for the version are skipped.

## Usage

``` r
add_cprd_aurum(
  path,
  tables = cprd_aurum_tables(),
  version = NULL,
  source = "CPRD Aurum code browser"
)
```

## Arguments

- path:

  Path to the CPRD Aurum code browser release directory, containing
  `CPRDAurumMedical.txt` and `CPRDAurumProduct.txt`. Only the files
  needed for `tables` must be present.

- tables:

  Character vector of tables to read. Any of `"cprd_aurum_medcode_lkp"`,
  `"cprd_aurum_prodcode_lkp"`, `"cprd_aurum_medcode_snomed"`,
  `"cprd_aurum_medcode_read2"` and `"cprd_aurum_prodcode_snomed"`.
  Defaults to all of them. The `medcode` tables come from
  `CPRDAurumMedical.txt` and the `prodcode` tables from
  `CPRDAurumProduct.txt`.

- version:

  Character string for the version label. If `NULL` (default), the
  release date (`yyyymm`) is taken from a directory name like
  `CPRD_CodeBrowser_YYYYMM_Aurum`, with a message saying which version
  is used. If the name doesn't include a release date, an error asks for
  `version` to be supplied.

- source:

  Character string for the data source.

## Value

Invisibly returns the result from
[`read_cprd_aurum()`](https://codeminer-io.github.io/codeminer/reference/read_cprd_aurum.md)
for the tables that were added (a named list of tables with metadata),
or `NULL` if they all existed already.

## Details

CPRD supplies the code browser to researchers with access to its data,
so `path` is required. Check your CPRD data access agreement before
sharing a database that contains these tables.

## See also

[`read_cprd_aurum()`](https://codeminer-io.github.io/codeminer/reference/read_cprd_aurum.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database()
add_cprd_aurum("path/to/CPRD_CodeBrowser_YYYYMM_Aurum")
} # }
```
