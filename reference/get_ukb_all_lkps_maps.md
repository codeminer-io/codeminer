# Get UK Biobank resource 592 directly from UKB website

Downloads the UK Biobank code mappings file (`all_lkps_maps_v4.xlsx`,
[resource 592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592))
directly from the UKB website.

## Usage

``` r
get_ukb_all_lkps_maps(dir_path = tempdir())
```

## Arguments

- dir_path:

  Directory path to download to.

## Value

File path to downloaded `all_lkps_maps_v4.xlsx`.

## Details

**Note:** This is a large object (\>450 MB)

## Examples

``` r
if (FALSE) { # \dontrun{
# download UKB resource 592, returning file path invisibly
file_path <- get_ukb_all_lkps_maps()

# view path to downloaded file
file_path
} # }
```
