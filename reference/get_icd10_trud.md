# Get ICD-10 coding system files from NHS TRUD

Downloads the ICD-10 Edition 5 release (TRUD item 258) from NHS TRUD.

## Usage

``` r
get_icd10_trud(
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

  Character string specifying which release to download. Can be:

  - `"latest"` (default) — downloads the most recent release

  - A specific release identifier string

- overwrite:

  Logical. If `TRUE`, re-downloads and overwrites existing files.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded ICD-10 zip file (invisibly).

## Details

This function requires a valid NHS TRUD API key set as the environment
variable `TRUD_API_KEY`. You must also be subscribed to item 258 on the
NHS TRUD website.

## See also

[`read_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_trud.md),
[`add_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/add_icd10_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_icd10_trud()
read_icd10_trud(path)
} # }
```
