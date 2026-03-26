# Get Read 2 coding system files from NHS TRUD

Downloads the NHS Data Migration release (TRUD item 9), which contains
the Read V2 lookup and Read 2/CTV3 cross-mapping files.

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

File path to the downloaded NHS Data Migration zip file (invisibly).

## Details

This is a convenience wrapper around
[`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md),
since both the Read 2 files and the NHS Data Migration mapping tables
come from the same TRUD item.

This function requires a valid NHS TRUD API key set as the environment
variable `TRUD_API_KEY`. You must also be subscribed to item 9 on the
NHS TRUD website.

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
