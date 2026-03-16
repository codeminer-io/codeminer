# Download Phecode 1.2 files

Downloads the Phecode 1.2 definitions file and/or mapping files from
[phewascatalog.org](https://phewascatalog.org/phecodes).

## Usage

``` r
get_phecode(
  dir_path = tempdir(),
  files = c("lkp", "icd10_map", "icd9_map"),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- dir_path:

  Directory to download files to. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- files:

  Character vector of files to download. Available options:

  - `"lkp"` — Phecode 1.2 definitions (lookup)

  - `"icd10_map"` — Phecode 1.2 to ICD-10 mapping (beta)

  - `"icd9_map"` — Phecode 1.2 to ICD-9 mapping

  By default, downloads all three.

- overwrite:

  Logical. If `TRUE`, re-downloads existing files. Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

A named list of file paths (invisibly), with names `"lkp"`,
`"icd10_map"`, and/or `"icd9_map"` corresponding to the requested files.

## See also

[`read_phecode()`](https://codeminer-io.github.io/codeminer/reference/read_phecode.md),
[`add_phecode()`](https://codeminer-io.github.io/codeminer/reference/add_phecode.md)

## Examples

``` r
if (FALSE) { # \dontrun{
paths <- get_phecode()
paths <- get_phecode(files = c("lkp", "icd10_map"), dir_path = "~/phecode")
} # }
```
