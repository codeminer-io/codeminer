# Get UK Biobank resource 592 directly from UK Biobank website

Downloads the UK Biobank code mappings file (`all_lkps_maps_v4.xlsx`,
[resource 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592))
directly from the UKB website.

## Usage

``` r
get_ukb_resource_592(dir_path = tempdir(), overwrite = FALSE, quiet = FALSE)
```

## Arguments

- dir_path:

  Directory path to download to. Will be created if it doesn't exist.

- overwrite:

  Logical. If `TRUE`, re-downloads and overwrites existing file. Default
  is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages. Default is
  `FALSE`.

## Value

File path to downloaded `all_lkps_maps_v4.xlsx` (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# download UKB resource 592, returning file path invisibly
file_path <- get_ukb_resource_592()

# download to specific directory, overwriting if exists
file_path <- get_ukb_resource_592(
  dir_path = tempdir(),
  overwrite = TRUE
)

# view path to downloaded file
file_path
} # }
```
