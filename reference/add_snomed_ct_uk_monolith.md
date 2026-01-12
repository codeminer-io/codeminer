# Add SNOMED CT UK Monolith tables to CodeMiner database

Reads SNOMED CT UK Monolith Edition RF2 files and adds these tables to
the active codeminer database. This is a convenience wrapper around
[`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
that automatically calls
[`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
[`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
and
[`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)
for each table.

The function processes lookup tables, mapping tables, and relationship
tables from the SNOMED CT release and adds them to the database with
their associated metadata.

## Usage

``` r
add_snomed_ct_uk_monolith(
  path = get_snomed_ct_uk_monolith(),
  tables = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/",
  .icd10_refset_id = "999002271000000101",
  .opcs4_refset_id = "999002321000000109"
)
```

## Arguments

- path:

  Path to the SNOMED CT UK Monolith Edition. This can be either:

  - A **zip file** (e.g., from
    [`get_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/get_snomed_ct_uk_monolith.md))

  - An **unzipped directory** containing the subdirectories
    `Snapshot/Terminology` and `Snapshot/Refset`

  Default uses
  [`get_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/get_snomed_ct_uk_monolith.md)
  to download the latest release.

- tables:

  Character vector of table names to read and add. Available tables:

  - Lookup table: `"sct_lookup"`

  - Relationship table: `"sct_relationship"`

  - Mapping tables: `"sct_icd10"`, `"sct_opcs4"`

  By default, adds all tables.

- version:

  Character string specifying the version label for the SNOMED CT
  release. If not provided, it will be derived from the zip filename or
  folder name.

- source:

  Character string specifying the source URL or description. Defaults to
  the NHS TRUD website.

- .icd10_refset_id:

  Character string. The SNOMED CT Concept ID identifying the specific
  Reference Set (Refset) used for ICD-10 mappings. Defaults to
  `"999002271000000101"`. This is an advanced parameter.

- .opcs4_refset_id:

  Character string. The SNOMED CT Concept ID identifying the specific
  Reference Set (Refset) used for OPCS-4 mappings. Defaults to
  `"999002321000000109"`. This is an advanced parameter.

## Value

Invisibly returns the result from
[`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
(a named list of tables with metadata).

## Details

The function performs the following steps:

1.  Calls
    [`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
    to read the specified tables

2.  Iterates through each table and table type
    (lookup/mapping/relationship)

3.  Adds each table to the database using the appropriate `add_*`
    function

4.  Returns the parsed data invisibly

This function requires an active database connection set up via
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
or similar.

## See also

- [`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
  for reading without adding to database

- [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md),
  [`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md),
  [`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)

- [`get_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/get_snomed_ct_uk_monolith.md)
  for downloading from NHS TRUD

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up database connection
build_database(db_path = "my_codes.db")

# Download latest release and add all tables
add_snomed_ct_uk_monolith()

# Add specific tables only
add_snomed_ct_uk_monolith(
  tables = c("sct_lookup", "sct_icd10")
)

# Use local zip file
snomed_zip <- "~/data/uk_sct2mo_41.3.0_20251217000001Z.zip"
add_snomed_ct_uk_monolith(path = snomed_zip)

# Use already-extracted directory
add_snomed_ct_uk_monolith(
  path = "~/data/SnomedCT_MonolithRF2_PRODUCTION_20251217T120000Z"
)
} # }

# Example with dummy data (for testing/demonstration)
if (FALSE) { # \dontrun{
build_database(db_path = tempfile(fileext = ".db"))
add_snomed_ct_uk_monolith(
  path = dummy_snomed_ct_uk_monolith_path()
)
} # }
```
