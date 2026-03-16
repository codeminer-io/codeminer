# Read the NHSBSA BNF-SNOMED mapping file into a mapping table

Reads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D
mapping zip file and returns a structured mapping table for use with
CodeMiner.

## Usage

``` r
read_nhsbsa_bnf_snomed(
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

A named list with element `bnf_dmd`, containing:

- `mapping`: a list with `table` (data frame) and `metadata` (list)

## See also

[`add_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/add_nhsbsa_bnf_snomed.md),
[`get_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_bnf_snomed.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_nhsbsa_bnf_snomed()
result <- read_nhsbsa_bnf_snomed(path)
result$bnf_dmd$mapping$table
} # }
```
