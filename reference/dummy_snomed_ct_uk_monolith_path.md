# Get full path to the dummy SNOMED CT GPS RF2 files

This function returns the full path to the dummy SNOMED CT GPS dataset
included in the `codeminer` package for testing and examples.

The dummy dataset is stored as a zip file within the package and is
extracted to a temporary directory on first use. The extracted path is
cached for the duration of the R session.

The dummy dataset is based on the SNOMED CT GPS (General Practitioner
Subset) release and contains a minimal set of concepts, descriptions,
relationships, and mappings suitable for unit tests and documentation
examples.

## Usage

``` r
dummy_snomed_ct_uk_monolith_path()
```

## Value

A character string with the full path to the dummy SNOMED CT GPS folder.

## Data Source and License

- **Source**: SNOMED International GPS (General Practice Subset) Release

- **URL**: <https://www.snomed.org/gps>

- **License**: Creative Commons Attribution 4.0 International (CC BY
  4.0)

- **Modifications**: This dummy dataset is a subset of the GPS release
  with additional mock concepts, descriptions, relationships, and
  mappings added for testing purposes. Made-up codes follow the pattern
  `000xxx000` (e.g., `000001000`) and made-up descriptions are marked
  with tildes (e.g., `~description~`).

## See also

[`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
to read SNOMED CT data

## Examples

``` r
dummy_snomed_ct_uk_monolith_path()
#> [1] "/tmp/RtmpJ0kfSL/codeminer_snomed_gps/SnomedCT_GPS_PRODUCTION"
```
