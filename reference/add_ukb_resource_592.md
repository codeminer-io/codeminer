# Add UK Biobank Resource 592 tables to CodeMiner database

Reads [UK Biobank Resource
592](https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) and adds these
tables to the active codeminer database. This is a convenience wrapper
around
[`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)
for each sheet.

The function processes lookup tables, mapping tables, and relationship
tables from the specified sheets and adds them to the database with
their associated metadata.

## Usage

``` r
add_ukb_resource_592(
  path = get_ukb_resource_592(),
  sheets = c("bnf_lkp", "dmd_lkp", "icd9_lkp", "icd10_lkp", "icd9_icd10", "read_v2_lkp",
    "read_v2_drugs_lkp", "read_v2_drugs_bnf", "read_v2_icd9", "read_v2_icd10",
    "read_v2_opcs4", "read_v2_read_ctv3", "read_ctv3_lkp", "read_ctv3_icd9",
    "read_ctv3_icd10", "read_ctv3_opcs4", "read_ctv3_read_v2"),
  ukb_version = "UKB v4",
  ukb_source = "https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592"
)
```

## Arguments

- path:

  Path to the UKB Resource 592 Excel file. Default uses
  [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  to download the file if needed.

- sheets:

  Character vector of sheet names to read. See
  [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
  for available sheet names. By default, reads all 17 sheets.

- ukb_version:

  Version label for the UKB resource (default: `"UKB v4"`).

- ukb_source:

  Source URL or description for the UKB resource.

## Value

Invisibly returns the result from
[`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
(a named list of tables with metadata).

## Details

The function performs the following steps:

1.  Calls
    [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
    to read the specified sheets

2.  Iterates through each sheet and table type
    (lookup/mapping/relationship)

3.  Adds each table to the database using the appropriate `add_*`
    function

4.  Returns the parsed data invisibly

This function requires an active database connection set up via
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
or similar.

## See also

- [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
  for reading without adding to database

- [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
  [`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
  [`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)

- [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  for downloading the file

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up database connection
build_database(db_path = "my_codes.db")

# Add all sheets from UKB Resource 592
add_ukb_resource_592()

# Add specific sheets only
add_ukb_resource_592(
  sheets = c("icd10_lkp", "read_v2_icd10")
)

# Use local file
add_ukb_resource_592(
  path = "path/to/all_lkps_maps_v4.xlsx",
  sheets = "icd9_icd10"
)
} # }

# Example with dummy data (for testing/demonstration)
if (FALSE) { # \dontrun{
build_database(db_path = tempfile(fileext = ".db"))
add_ukb_resource_592(
  path = dummy_ukb_resource_592_path(),
  sheets = "icd10_lkp"
)
} # }
```
