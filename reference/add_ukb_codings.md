# Add UK Biobank codings tables to CodeMiner database

Reads the UK Biobank `Codings.tsv` file and adds lookup tables for
OPCS-4 and UKB self-reported code types to the active CodeMiner
database. This is a convenience wrapper around
[`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
for each table.

## Usage

``` r
add_ukb_codings(
  path = get_ukb_codings(),
  tables = c("opcs4_lkp", "data_coding_3", "data_coding_4", "data_coding_5",
    "data_coding_6"),
  version = "v0",
  source = "https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide"
)
```

## Arguments

- path:

  Path to the `Codings.tsv` file. Default uses
  [`get_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_codings.md)
  to download the file.

- tables:

  Character vector of table names to add. See
  [`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md)
  for available table names. By default, adds all five tables.

- version:

  Character string for the version label (default: `"v0"`).

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md)
(a named list of tables with metadata).

## See also

[`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md),
[`get_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_codings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_ukb_codings()
add_ukb_codings(tables = c("opcs4_lkp", "data_coding_4"))
} # }

# Example with dummy data
if (FALSE) { # \dontrun{
build_database(db_path = tempfile(fileext = ".db"))
add_ukb_codings(
  path = dummy_ukb_codings_path(),
  tables = "opcs4_lkp"
)
} # }
```
