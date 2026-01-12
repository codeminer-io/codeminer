# Get SNOMED CT UK Monolith Edition from NHS TRUD

Downloads the SNOMED CT UK Monolith Edition (item 1799) from NHS TRUD.

## Usage

``` r
get_snomed_ct_uk_monolith(
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

  - `"latest"` (default) - Downloads the most recent release

  - A specific release identifier string (e.g.,
    `"uk_sct2mo_41.3.0_20251217000001Z.zip"`)

- overwrite:

  Logical. If `TRUE`, re-downloads and overwrites existing files.
  Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to the downloaded SNOMED CT UK Monolith zip file (invisibly).

## Details

This function requires a valid NHS TRUD API key set as an environment
variable `TRUD_API_KEY`. You must also be subscribed to item 1799 on the
NHS TRUD website.

By default, the latest release is downloaded. The function checks if the
target zip file already exists and skips the download if so (unless
`overwrite = TRUE`).

The returned zip file path can be passed directly to
[`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md),
which will extract the contents on-demand.

## See also

- [`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
  to read the downloaded data into R

- [`trud::download_item()`](https://docs.ropensci.org/trud/reference/download_item.html)
  for the underlying download function

- [`trud::get_item_metadata()`](https://docs.ropensci.org/trud/reference/get_item_metadata.html)
  to retrieve available release identifiers

## Examples

``` r
if (FALSE) { # \dontrun{
# Download latest SNOMED CT UK Monolith to tempdir
snomed_zip <- get_snomed_ct_uk_monolith()

# Get available releases
metadata <- trud::get_item_metadata(1799, release_scope = "all")
available_releases <- purrr::map_chr(metadata$releases, "id")

# Download specific release
snomed_zip <- get_snomed_ct_uk_monolith(
  release = "uk_sct2mo_41.3.0_20251217000001Z.zip"
)

# Download to specific directory for persistent storage
snomed_zip <- get_snomed_ct_uk_monolith(
  dir_path = "~/data/snomed",
  overwrite = TRUE
)

# Read the downloaded data (extracts on-demand)
snomed <- read_snomed_ct_uk_monolith(snomed_zip)
} # }
```
