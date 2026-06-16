# Validate the codeminer database for on-disk inconsistencies

Inspects the database at `CODEMINER_DB_PATH` and reports any
inconsistencies it finds — without modifying the database. The specific
checks depend on the backend:

## Usage

``` r
validate_database()
```

## Value

A named list of character vectors, one entry per kind of issue. Empty
vectors mean no issues of that kind were found.

## Details

- `codeminer_folder`:

  - **orphan data files**: `<name>.duckdb` files at the folder root with
    no matching metadata row (typically left by a previous
    `add_*_table()` that died after the data file was committed but
    before the metadata file was).

  - **dangling metadata**: metadata rows that reference a data file that
    does not exist.

  - **stale temp files**: `*.parquet.tmp` (metadata) or `*.duckdb.tmp`
    (data) files left over from an interrupted write.

- `duckdb_file`:

  - **dangling metadata**: metadata rows referencing data tables that do
    not exist in the file.

Issues are reported via informational messages. The function returns a
named list of issues invisibly so callers can act on them
programmatically.

## See also

Other Database management:
[`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md),
[`update_mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_mapping_metadata.md),
[`update_relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_relationship_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- validate_database()
} # }
```
