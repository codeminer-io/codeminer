# Read NHS BSA BNF code information (pseudo-BNF) into lookup and relationship tables

Reads the NHS Business Services Authority (NHSBSA) "BNF Code
Information" CSV (the full BNF code hierarchy, commonly called the
pseudo-BNF classification) and returns a BNF lookup table plus a
parent-child relationship table for use with CodeMiner.

## Usage

``` r
read_pseudobnf(
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

A named list with element `bnf_lkp`, containing:

- `lookup`: a list with `table` (data frame) and `metadata` (list)

- `relationship`: a list with `table` (data frame) and `metadata` (list)

## Details

The lookup mirrors the shape of the BNF lookup historically derived from
UK Biobank resource 592 (one row per code at each level of the BNF
hierarchy, with higher-level name columns populated and deeper ones
`NA`), so it is a drop-in replacement. Unlike that approach, each
level's code is taken directly from the portal's explicit `*_CODE`
columns rather than sliced from the presentation code.

## See also

[`add_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/add_pseudobnf.md),
[`get_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/get_pseudobnf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_pseudobnf()
result <- read_pseudobnf(path)
result$bnf_lkp$lookup$table
result$bnf_lkp$relationship$table
} # }
```
