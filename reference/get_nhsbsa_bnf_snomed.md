# Download the NHSBSA BNF-SNOMED mapping file

Downloads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D
mapping file from the NHSBSA website.

## Usage

``` r
get_nhsbsa_bnf_snomed(dir_path = tempdir(), overwrite = FALSE, quiet = FALSE)
```

## Arguments

- dir_path:

  Directory to download the file to. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- overwrite:

  Logical. If `TRUE`, re-downloads the file even if it already exists.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded zip file (invisibly).

## See also

[`read_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/read_nhsbsa_bnf_snomed.md),
[`add_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/add_nhsbsa_bnf_snomed.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_nhsbsa_bnf_snomed()
} # }
```
