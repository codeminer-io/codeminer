# Get path to dummy UK Biobank Resource 592 file

Returns the file path to a subset of UK Biobank Resource 592
(`all_lkps_maps_v4.xlsx`, [resource
592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)) included in
the package for testing and examples.

This is a **subset** of the full UKB Resource 592 file, containing a
reduced set of codes suitable for unit tests and documentation examples.
For production use, download the full file using
[`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md).

## Usage

``` r
dummy_ukb_resource_592_path()
```

## Value

Character string with the path to the dummy Excel file in the package's
`extdata` directory.

## See also

- [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  to download the full UKB Resource 592 file

- [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
  to read the file

## Examples

``` r
# Get path to dummy file
dummy_ukb_resource_592_path()
#> [1] "/home/runner/work/_temp/Library/codeminer/extdata/all_lkps_maps_v4.xlsx"

# Use in read_ukb_resource_592()
result <- read_ukb_resource_592(
  path = dummy_ukb_resource_592_path(),
  sheets = "icd10_lkp"
)
#> Reading 1 selected table from UKB Resource 592
#> 
```
