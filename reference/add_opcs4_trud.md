# Add OPCS-4 lookup table to CodeMiner database

Reads OPCS-4 files and adds the lookup table to the active CodeMiner
database. This is a convenience wrapper around
[`read_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/read_opcs4_trud.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md).

## Usage

``` r
add_opcs4_trud(
  path = get_opcs4_trud(),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
)
```

## Arguments

- path:

  Path to the OPCS-4 release (zip file or unzipped directory). Default
  uses
  [`get_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/get_opcs4_trud.md)
  to download the latest release.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the zip file or directory name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/read_opcs4_trud.md)
(a named list of tables with metadata).

## See also

[`read_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/read_opcs4_trud.md),
[`get_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/get_opcs4_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_opcs4_trud()
} # }
```
