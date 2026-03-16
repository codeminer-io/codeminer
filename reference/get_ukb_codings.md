# Download the UK Biobank codings file

Downloads the UK Biobank `Codings.tsv` file, which contains lookup
tables for OPCS-4 and UKB self-reported condition/medication/operation
codes.

## Usage

``` r
get_ukb_codings(dir_path = tempdir(), overwrite = FALSE, quiet = FALSE)
```

## Arguments

- dir_path:

  Directory to download the file to. Will error if it does not exist.
  Defaults to [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- overwrite:

  Logical. If `TRUE`, re-downloads the file even if it already exists.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded `Codings.tsv` (invisibly).

## See also

[`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md),
[`add_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/add_ukb_codings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_ukb_codings()
path <- get_ukb_codings(dir_path = "~/ukb_data", overwrite = TRUE)
} # }
```
