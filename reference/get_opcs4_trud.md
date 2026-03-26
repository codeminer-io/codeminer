# Get OPCS-4 coding system files from NHS TRUD

Downloads the OPCS-4 release (TRUD item 119) from NHS TRUD.

## Usage

``` r
get_opcs4_trud(
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

File path to the downloaded OPCS-4 zip file (invisibly).

## Details

This function requires a valid NHS TRUD API key set as the environment
variable `TRUD_API_KEY`. You must also be subscribed to item 119 on the
NHS TRUD website.

## See also

[`read_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/read_opcs4_trud.md),
[`add_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/add_opcs4_trud.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_opcs4_trud()
read_opcs4_trud(path)
} # }
```
