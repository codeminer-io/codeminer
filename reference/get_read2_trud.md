# Get Read 2 coding system files from NHS TRUD

Downloads the NHS Read Browser release (TRUD item 8), which contains the
canonical Read V2 terminology files (FoxPro DBF format) under
`Standard/V2/`.

## Usage

``` r
get_read2_trud(
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

File path to the downloaded NHS Read Browser zip file (invisibly).

## Details

This function requires a valid NHS TRUD API key set as the environment
variable `TRUD_API_KEY`. You must also be subscribed to item 8 on the
NHS TRUD website.

The NHS Read Browser supersedes the Read 2 lookup files that were
previously read from the NHS Data Migration release (TRUD item 9). Item
8 is the canonical Read V2 terminology and contains the complete set of
Read codes (including codes without a SNOMED CT mapping), along with a
preferred / synonym indicator (`TERMTYPE`) that the migration files
lacked.

The Read 2 ↔ CTV3 cross-mapping tables remain in TRUD item 9 and are
available via
[`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)
/
[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md).

## See also

[`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md),
[`add_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read2_trud.md),
[`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_read2_trud()
read_read2_trud(path)
} # }
```
