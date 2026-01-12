# Read the SNOMED CT UK Monolith Edition into R

Reads the Terminology and Refset snapshot tables from a local copy of
the **SNOMED CT UK Monolith Edition** release files.

## Usage

``` r
read_snomed_ct_uk_monolith(
  path,
  tables = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/",
  .icd10_refset_id = "999002271000000101",
  .opcs4_refset_id = "999002321000000109"
)
```

## Arguments

- path:

  A character string giving the path to the SNOMED CT UK Monolith
  Edition. This can be either:

  - A **zip file** (e.g., from
    [`get_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/get_snomed_ct_uk_monolith.md)) -
    will be extracted to a temporary directory on-demand

  - An **unzipped directory** containing the subdirectories
    `Snapshot/Terminology` and `Snapshot/Refset`

- tables:

  Character vector of table names to read. Available tables are:

  - Lookup table: `"sct_lookup"`

  - Relationship table: `"sct_relationship"`

  - Mapping tables: `"sct_icd10"`, `"sct_opcs4"`

  By default, all tables are read.

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
  `"999002271000000101"` (UK Clinical Extension Extended Map to ICD-10).
  This is an advanced parameter that typically does not need to be
  changed.

- .opcs4_refset_id:

  Character string. The SNOMED CT Concept ID identifying the specific
  Reference Set (Refset) used for OPCS-4 mappings. Defaults to
  `"999002321000000109"` (UK Clinical Extension Extended Map to OPCS-4).
  This is an advanced parameter that typically does not need to be
  changed.

## Value

A named list with elements corresponding to requested tables, each
containing tables and metadata:

- `sct_lookup`: Lookup table with SNOMED CT concepts and descriptions.
  Contains `table` (data.table) and `metadata` (list).

- `sct_relationship`: Relationship table with concept relationships.
  Contains `table` (data.table) and `metadata` (list).

- `sct_icd10`: Mapping table with SNOMED CT to ICD-10 mappings. Contains
  `table` (data.table) and `metadata` (list).

- `sct_opcs4`: Mapping table with SNOMED CT to OPCS-4 mappings. Contains
  `table` (data.table) and `metadata` (list).

## Details

This function imports the key data files as `data.table` objects,
performs basic normalisation, and constructs joined tables for concept
descriptions and ICD-10/OPCS-4 mappings.

For more information on the SNOMED CT UK Monolith Edition, see the [NHS
TRUD
website](https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read from a zip file (extracts on-demand)
snomed_zip <- get_snomed_ct_uk_monolith()
snomed <- read_snomed_ct_uk_monolith(snomed_zip)

# Read from an already-extracted directory
snomed <- read_snomed_ct_uk_monolith("~/SNOMEDCT_Release_UK")

# Read only specific tables
snomed <- read_snomed_ct_uk_monolith(
  path = snomed_zip,
  tables = c("sct_lookup", "sct_icd10")
)

# Access lookup table and metadata
sct_lookup <- snomed$sct_lookup$lookup$table
sct_metadata <- snomed$sct_lookup$lookup$metadata

# Specify custom version
snomed <- read_snomed_ct_uk_monolith(
  path = "~/SNOMEDCT_Release_UK",
  version = "UK_20240101"
)
} # }
```
