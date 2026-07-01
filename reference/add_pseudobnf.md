# Add NHS BSA BNF code information (pseudo-BNF) to CodeMiner database

Reads the NHS Business Services Authority (NHSBSA) "BNF Code
Information" CSV and adds the BNF lookup and relationship tables to the
active CodeMiner database. This is a convenience wrapper around
[`read_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/read_pseudobnf.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md).

## Usage

``` r
add_pseudobnf(
  path = get_pseudobnf(),
  version = NULL,
  source = "https://opendata.nhsbsa.net/dataset/bnf-code-information-current-year"
)
```

## Arguments

- path:

  Path to the NHS BSA BNF code information CSV. Default uses
  [`get_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/get_pseudobnf.md)
  to download the latest release.

- version:

  Character string for the version label. If `NULL` (default), derived
  from the file name.

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/read_pseudobnf.md)
(a named list of tables with metadata), or `NULL` if all tables already
exist.

## See also

[`read_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/read_pseudobnf.md),
[`get_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/get_pseudobnf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_pseudobnf()
} # }
```
