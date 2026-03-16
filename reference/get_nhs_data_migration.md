# Get NHS Data Migration mapping tables from NHS TRUD

Downloads the NHS Data Migration release (TRUD item 9), which contains
clinically assured Read V2 and CTV3 to SNOMED CT mapping tables.

## Usage

``` r
get_nhs_data_migration(
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

This function requires a valid NHS TRUD API key set as the environment
variable `TRUD_API_KEY`. You must also be subscribed to item 9 on the
NHS TRUD website.

## See also

[`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md),
[`add_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/add_nhs_data_migration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- get_nhs_data_migration()
read_nhs_data_migration(path)
} # }
```
