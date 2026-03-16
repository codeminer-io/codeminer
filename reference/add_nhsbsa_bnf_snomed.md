# Add NHSBSA BNF-SNOMED mapping table to CodeMiner database

Reads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D
mapping file and adds it to the active CodeMiner database. This is a
convenience wrapper around
[`read_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/read_nhsbsa_bnf_snomed.md)
that automatically calls
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md).

## Usage

``` r
add_nhsbsa_bnf_snomed(
  path = get_nhsbsa_bnf_snomed(),
  version = "v0",
  source =
    "https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping"
)
```

## Arguments

- path:

  Path to the NHSBSA BNF-SNOMED mapping zip file. Default uses
  [`get_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_bnf_snomed.md)
  to download the file.

- version:

  Character string for the version label (default: `"v0"`).

- source:

  Character string for the data source URL or description.

## Value

Invisibly returns the result from
[`read_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/read_nhsbsa_bnf_snomed.md)
(a named list of tables with metadata).

## See also

[`read_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/read_nhsbsa_bnf_snomed.md),
[`get_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_bnf_snomed.md)

## Examples

``` r
if (FALSE) { # \dontrun{
build_database(db_path = "my_codes.db")
add_nhsbsa_bnf_snomed()
} # }
```
