# Get NHS BSA BNF code information (pseudo-BNF)

Downloads the NHS Business Services Authority (NHSBSA) "BNF Code
Information" dataset from the NHSBSA Open Data Portal. This is the full
BNF code hierarchy (chapter -\> section -\> paragraph -\> ... -\>
presentation), commonly referred to as the pseudo-BNF classification,
published monthly.

## Usage

``` r
get_pseudobnf(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dir_path:

  Directory path to download to. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- release:

  Character string specifying which release to download. Either:

  - `"latest"` (default) — the most recent monthly release.

  - A substring matching a single resource name (e.g. a `"YYYYMM"`
    year-month or a `"version_NN"` label).

- overwrite:

  Logical. If `TRUE`, re-downloads and overwrites an existing file.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded CSV (invisibly).

## Details

Requires the nhsbsa package, which provides a low-level client for the
NHSBSA Open Data Portal API.

## See also

[`read_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/read_pseudobnf.md),
[`add_pseudobnf()`](https://codeminer-io.github.io/codeminer/reference/add_pseudobnf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_pseudobnf()
read_pseudobnf(path)
} # }
```
