# Developer Guide

## Overview

This guide provides technical details for developers contributing to
codeminer, including the on-disk data model, the schema-versioning
contract, and the dummy datasets used for testing.

## Data model

codeminer stores everything in one of three on-disk layouts. All three
hold the data tables for each code system (lookups, mappings,
relationships) plus a small set of **metadata tables** that index them;
the layouts differ only in how those tables are persisted to disk.

### Storage backends

The active backend is decided by what `CODEMINER_DB_PATH` points at, and
(for folder paths) what was stamped at
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
time. `backend_kind()` in `R/backend.R` is the source of truth for the
decision.

| backend (`backend_kind()` return) | path | data tables | metadata tables | choice of |
|----|----|----|----|----|
| `duckdb_file` | a single `.duckdb` file | inside the file | inside the file | default for file paths |
| `codeminer_folder` | a directory | one `<name>.duckdb` per data table at the folder root | `_lookup_metadata.parquet`, `_mapping_metadata.parquet`, `_relationship_metadata.parquet`, `_db_metadata.parquet` | default for folder paths (`build_database(format = "duckdb")`) |
| `parquet_folder` | a directory | one `<name>.parquet` per data table at the folder root | same parquet metadata files as `codeminer_folder` | opt-in via `build_database(format = "parquet")` |

All three look identical from the rest of the package’s perspective —
[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md)
projects the data tables into a workbench schema and
`dplyr::tbl(con, "<name>")` works the same way regardless.
`dbListTables()` returns the same listing in every backend. Writes
(`add_*_table()`, `remove_*_table()`, `update_*_metadata()`) all go
through the dispatcher in `R/backend.R`; no other file should hard-code
DuckDB or parquet specifics for the main backend.

The folder backends use a write-to-temp + atomic-rename transaction for
`add_*_table()` so that readers always see either the complete pre-add
state or the complete post-add state — never a torn read. See
[`validate_database()`](https://codeminer-io.github.io/codeminer/reference/validate_database.md)
for the on-disk consistency checks (orphan data files, dangling metadata
rows, stale `.tmp` files).

### Index tables

Three index tables, one per data-table flavour. Each row indexes one
underlying data table; the `*_table_name` column is the primary key and
matches the actual table name in the DB.

| table | key field | purpose |
|----|----|----|
| `_lookup_metadata` | `lookup_table_name` | one row per lookup table. Records `code_type`, `lookup_version`, which column carries the code / description / category / preferred-description flag, the data source URL, and a JSON `col_filters` spec. |
| `_mapping_metadata` | `mapping_table_name` | one row per `from -> to` mapping table. Records `from_code_type`, `to_code_type`, `map_version`, the from/to columns, the source URL, and `col_filters`. |
| `_relationship_metadata` | `relationship_table_name` | one row per relationship table. Records `code_type`, `relationship_version`, the child / parent / type columns, the value that means “is a”, the source URL, and `col_filters`. |

The exact column lists live in `required_lookup_metadata_columns()`,
`required_mapping_metadata_columns()`, and
`required_relationship_metadata_columns()` — these are the source of
truth and are checked in
[`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md).

### The `_db_metadata` stamp table

A fourth metadata table — `_db_metadata` — is *DB-level state*, not a
per-data-table index. It carries a single row describing the database
itself:

| column | purpose |
|----|----|
| `codeminer_version` | `packageVersion("codeminer")` at the time the DB was last built or migrated. |
| `schema_version` | Integer. The codeminer-defined schema version this DB is on. Compared against `current_schema_version()` at every connect. |
| `storage_format` | One of `"duckdb_file"`, `"codeminer_folder"`, `"parquet_folder"`. Recorded once at [`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md) time. Used by `backend_kind()` to disambiguate folder layouts that have no data tables yet (otherwise the file extensions at the folder root drive the decision). |
| `built_at` | ISO timestamp of the original build. Immutable. |
| `last_migrated_at` | ISO timestamp of the most recent in-place migration. Always `NA` while the project is pre-1.0 (no migration path; rebuild on schema change). Kept on the row for forward compatibility. |
| `codeminer_source` | Where this codeminer was installed from (`"CRAN"`, `"GitHub"`, `"Local"`, …) — `packageDescription()$Source`. |
| `codeminer_remote_type` / `codeminer_remote_host` / `codeminer_remote_repo` / `codeminer_remote_username` / `codeminer_remote_sha` | renv-style provenance fields. Populated when codeminer was installed via `devtools::install_github()` / `pak`/`remotes`; `NA` for CRAN or local installs. |

`_db_metadata` is intentionally excluded from the user-facing
[`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
API — inspect it via
[`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
or by querying the table directly.

## Schema versioning

The on-disk database carries its own schema version, separate from the
package version. The contract:

- The package declares `current_schema_version()` (what fresh
  [`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
  writes).
- Every codeminer DB has a `_db_metadata` row stamping which schema
  version produced it, when, by which codeminer version, and the install
  provenance.
- On connect, codeminer compares the DB’s stamp to
  `current_schema_version()` and refuses anything that doesn’t match.

There is intentionally **no in-place migration path** while the package
is pre-1.0. When the format changes the user rebuilds the DB. See issue
\#139 for the discussion that landed this policy and \#139’s “Options”
for what the longer-term story might look like once we have external
users.

### Connect-time gate

[`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md)
runs `enforce_schema_gate()` against the main DB before attaching. The
behaviour:

| DB stamp vs package | behaviour |
|----|----|
| `schema_version == current_schema_version()` | proceed silently |
| `> current_schema_version()` | refuse: DB built with a newer codeminer; upgrade the package |
| `< current_schema_version()` | refuse: format has changed; rebuild via `build_database(overwrite = TRUE)` |

An *unstamped* DB (built before `_db_metadata` existed) is treated as
`schema_version = 0` and falls into the third row — same “rebuild”
refusal.

### When to bump `current_schema_version()`

Bump if and only if your change alters **what is stored on disk**. See
`CLAUDE.md` for the full list of triggers and non-triggers. Examples:

| change | bump? |
|----|----|
| New column in `required_lookup_metadata_columns()` | yes |
| Rename a column in an existing metadata table | yes |
| Change the `paste(code_type, lookup_version, ...)` rule for `*_table_name` | yes |
| New top-level R function | no |
| Change how [`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md) renames columns at read time | no |
| Bug fix that doesn’t alter stored data | no |

When you bump, the next connect against any pre-bump DB will refuse with
a “rebuild” message. Users (currently just the team) rebuild via
`build_database(overwrite = TRUE)`.

## Dummy Data

### SNOMED CT

#### Overview

The codeminer package includes a dummy SNOMED CT dataset for testing and
documentation purposes. This dataset is stored as a zip file at:

    inst/extdata/snomed_gps.zip

The zip file contains a `SnomedCT_GPS_PRODUCTION_20251015T120000Z/`
directory which is extracted on first use by
[`dummy_snomed_ct_uk_monolith_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_snomed_ct_uk_monolith_path.md).
This approach avoids R CMD check warnings about non-portable file paths
(the RF2 format uses deeply nested directory structures with long path
names).

The dummy dataset follows the SNOMED CT RF2 (Release Format 2) structure
and is based on the **SNOMED International GPS (General Practice Subset)
Release**. It contains a minimal but comprehensive set of concepts,
descriptions, relationships, and mappings designed to test all major
codeminer functions.

#### Data Sources and Licensing

**SNOMED CT GPS Release**: The dummy dataset uses concept codes from the
SNOMED International GPS Release (available from
<https://www.snomed.org/gps>). The GPS subset is freely available and
can be used for testing purposes.

**Made-up Data Convention**:

To clearly distinguish fabricated test data from authentic SNOMED CT
content:

- **Concept codes**: Use the pattern `000xxx000` (starting and ending
  with three zeros), e.g., `000001000`, `000002000`, `000100000`
- **Description IDs**: Use numeric IDs starting with `000`, e.g.,
  `000010001`, `000020001`
- **Relationship IDs**: Use numeric IDs starting with `000`, e.g.,
  `000100001`, `000100002`
- **UUIDs** (for map refsets): Use the pattern
  `00000xxx-0000-0000-0000-0000000000xx` (starts with five zeros, middle
  groups all zeros), e.g., `10000001-0000-0000-0000-000000000001`
- **Descriptions**: Mark made-up concept descriptions with tildes
  `~description~`, e.g., `~Optic neuritis due to multiple sclerosis~`

Examples: - `000001000` - Made-up concept code -
`~Optic neuritis due to multiple sclerosis~` - Made-up description -
`10000001-0000-0000-0000-000000000001` - Made-up UUID for map refset
entries

#### Dataset Structure

The dummy dataset includes the following files in RF2 format:

##### Terminology Files (`Snapshot/Terminology/`)

1.  **sct2_Concept_GPSSnapshot_INT_20250701.txt** - Core concepts (18
    concepts)
2.  **sct2_Description_GPSSnapshot-en_INT_20250701.txt** - Descriptions
    (36 descriptions: FSN + PT for each concept)
3.  **sct2_Relationship_GPSSnapshot_INT_20250701.txt** - Relationships
    (14 relationships)

##### Reference Set Files (`Snapshot/Refset/`)

4.  **Map/der2_iisssccRefset_GPSExtendedMapSnapshot_INT_20250701.txt** -
    ICD-10 and OPCS-4 mappings (9 mappings: 6 ICD-10, 3 OPCS-4)

#### Test Concepts Overview

The dummy dataset includes concepts from the following clinical domains:

**Disorders (ICD-10 mapped)**: - Multiple sclerosis and subtypes
(24700007, 426373005, 425500002) - Optic neuritis (66760008) - Made-up:
Optic neuritis due to MS (000001000) - Chronic pharyngitis (140004)

**Procedures (OPCS-4 mapped)**: - Total thyroidectomy (24443003) - Total
colectomy (26390003) - Cardiac pacing (18590009)

**Products and Substances**: - Bisoprolol products (774880006,
318604001, 318590006) - Made-up: Bisoprolol substance (000002000)

**Attributes**: - Has active ingredient (127489000) - Due to (42752001)

#### Test Concepts Hierarchy

The dummy dataset includes the following concept hierarchy:

    ~Clinical finding~ (000100000)
    ├─ Multiple sclerosis (24700007) [GPS]
    │  ├─ Relapsing remitting MS (426373005) [GPS]
    │  ├─ Secondary progressive MS (425500002) [GPS]
    │  └─ ~Optic neuritis due to MS~ (000001000) [Made-up]
    │     - Has parent: Optic neuritis (66760008) [GPS]
    │     - Has attribute "Due to": Multiple sclerosis
    ├─ Optic neuritis (66760008) [GPS]
    └─ Chronic pharyngitis (140004) [GPS]

    ~Product~ (000200000)
    └─ Bisoprolol product (774880006) [GPS]
       ├─ Bisoprolol 1.25mg tablet (318604001) [GPS]
       │  - Has attribute "Has active ingredient": ~Bisoprolol substance~
       └─ Bisoprolol 5mg tablet (318590006) [GPS]
          - Has attribute "Has active ingredient": ~Bisoprolol substance~

    ~Substance~ (000300000)
    └─ ~Bisoprolol substance~ (000002000) [Made-up]

    Attributes:
    - Has active ingredient (127489000) [GPS]
    - Due to (42752001) [GPS]

#### Testing Coverage

The dummy dataset is designed to test:

1.  **CODES()** - Lookup codes and descriptions
2.  **DESCRIPTION()** - Search by description text
3.  **MAP()** - Map SNOMED CT to ICD-10 or OPCS-4
4.  **CHILDREN()** / **PARENTS()** - Navigate hierarchical relationships
5.  **N_CHILDREN()** / **N_PARENTS()** - Navigate with depth control
6.  **RELATIONSHIP_TYPES_FROM()** / **RELATIONSHIP_TYPES_TO()** - Query
    relationship types
7.  **HAS_ATTRIBUTES()** / **ATTRIBUTES_FOR()** - Query concept
    attributes

Key test scenarios:

- Simple hierarchies (MS -\> subtypes)
- Multiple inheritance (Optic neuritis due to MS has 2 parents)
- Drug hierarchies with active ingredients
- Attribute relationships (drugs contain substances, diseases caused by
  other diseases)
- SNOMED -\> ICD-10 mappings

#### Clinical Code Mappings

The dataset includes these mappings:

##### ICD-10 Mappings

| SNOMED CT Concept                      | ICD-10 Code | Dagger/Asterisk |
|----------------------------------------|-------------|-----------------|
| Multiple sclerosis (24700007)          | G35D        | D (dagger)      |
| Relapsing remitting MS (426373005)     | G35         | —               |
| Secondary progressive MS (425500002)   | G35         | —               |
| Optic neuritis (66760008)              | H46A        | A (asterisk)    |
| ~Optic neuritis due to MS~ (000001000) | H46         | —               |
| Chronic pharyngitis (140004)           | J312        | —               |

The dagger/asterisk suffix is stripped from `mapTarget` during
processing and stored in a separate `icd10_dagger_asterisk` column.

##### OPCS-4 Mappings

| SNOMED CT Concept              | OPCS-4 Code |
|--------------------------------|-------------|
| Total thyroidectomy (24443003) | B08         |
| Total colectomy (26390003)     | H05         |
| Cardiac pacing (18590009)      | K60         |

#### Referential Integrity

The dummy dataset maintains referential integrity across files:

**Concept Table -\> Description Table**: Every concept in
`sct2_Concept_GPSSnapshot_INT_20250701.txt` must have at least one
description (typically both FSN and PT) in
`sct2_Description_GPSSnapshot-en_INT_20250701.txt`.

**Map Table -\> Concept/Description Tables**: Every SNOMED CT code
referenced in the mapping table (`referencedComponentId` column in
`der2_iisssccRefset_GPSExtendedMapSnapshot_INT_20250701.txt`) must exist
in both the concept and description tables. This applies to both ICD-10
mappings (`refsetId` = `999002271000000101`) and OPCS-4 mappings
(`refsetId` = `999002321000000109` in the dummy data). Note that the
OPCS-4 map `refsetId` is version specific in real releases (a new refset
per OPCS-4 edition) and is auto-detected at read time from the refset’s
description term (see `.opcs4_refset_id` in
[`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md));
the dummy `999002321000000109` therefore also needs a description row
naming it as an OPCS-4 complex map reference set.

**Relationship Table -\> Concept Table**: Both `sourceId` and
`destinationId` in relationship entries should reference valid concepts
(though `destinationId` may reference concepts not in our minimal set,
like `116680003` for “Is a”).

##### Validation Script

Use this code to verify referential integrity:

```` r
library(codeminer)

# Read dummy data
snomed <- read_snomed_ct_uk_monolith(dummy_snomed_ct_uk_monolith_path())

# Extract unique concept IDs from each table
concepts <- unique(snomed$sct_lookup$conceptId)
descriptions <- unique(snomed$sct_lookup$conceptId)
mapped_concepts <- unique(snomed$sct_icd10_mapping$referencedComponentId)
rel_sources <- unique(snomed$sct_relationship$sourceId)
rel_destinations <- unique(snomed$sct_relationship$destinationId)

# Check: All concepts have descriptions
cat("Concepts without descriptions:\n")
print(setdiff(concepts, descriptions))

# Check: All mapped concepts exist in concept/description tables
cat("\nMapped concepts not in concept table:\n")
print(setdiff(mapped_concepts, concepts))

# Check: Relationship sources exist
cat("\nRelationship sources not in concept table:\n")
print(setdiff(rel_sources, concepts))

# Check: Relationship destinations (excluding known external references)
external_refs <- c("116680003")  # "Is a" type - not in our minimal set
cat("\nRelationship destinations not in concept table (excluding known external refs):\n")
print(setdiff(setdiff(rel_destinations, concepts), external_refs))

# Summary
cat("\nSummary:\n")
cat("Total concepts:", length(concepts), "\n")
cat("Total descriptions:", nrow(snomed$sct_lookup), "\n")
cat("Total relationships:", nrow(snomed$sct_relationship), "\n")
cat("Total ICD-10 mappings:", nrow(snomed$sct_icd10_mapping), "\n")
cat("Total OPCS-4 mappings:", nrow(snomed$sct_opcs4_mapping), "\n")
```
````

#### Adding Additional Test Concepts

To extend the dummy dataset with additional concepts:

1.  **Select GPS codes**: Use codes from the GPS freeset format
    (`SnomedINTL_GPSRelease_PRODUCTION_20250701T120000Z.txt`) when
    possible
2.  **Mark made-up codes**: Use the pattern `000xxx000` for any new
    concept codes (e.g., `000001000`, `000002000`)
3.  **Mark made-up descriptions**: Use tilde notation (`~description~`)
    for made-up concept names
4.  **Follow RF2 format**: Maintain tab-separated format with proper
    column structure
5.  **Update all related files** while maintaining referential
    integrity:
    - Add concept to `sct2_Concept_GPSSnapshot_INT_20250701.txt`
    - Add FSN and PT descriptions to
      `sct2_Description_GPSSnapshot-en_INT_20250701.txt` (required)
    - Add relationships to
      `sct2_Relationship_GPSSnapshot_INT_20250701.txt` (if needed)
    - Add ICD-10 mappings if needed to
      `der2_iisssccRefset_GPSExtendedMapSnapshot_INT_20250701.txt`
6.  **Maintain referential integrity**: Ensure all mapped concepts and
    relationship participants have entries in the concept and
    description tables
7.  **Update documentation**: Review and update this developer guide
    vignette to reflect any structural changes to the dummy dataset
8.  **Test**: Run the validation script above to verify integrity

#### Standard Values Reference

When adding new entries, use these standard SNOMED CT values:

- `effectiveTime`: `20250701`
- `active`: `1`
- `moduleId`: `900000000000207008` (SNOMED CT core)
- `definitionStatusId`: `900000000000074008` (defined)
- Description `typeId`:
  - `900000000000003001` = FSN (Fully Specified Name)
  - `900000000000013009` = Synonym/PT (Preferred Term)
- `caseSignificanceId`: `900000000000448009` (case insensitive)
- `languageCode`: `en`
- Relationship `typeId`:
  - `116680003` = “Is a” (subsumption)
  - `127489000` = “Has active ingredient”
  - `42752001` = “Due to”
- `characteristicTypeId`: `900000000000011006` (inferred)
- `modifierId`: `900000000000451002` (some)
- `refsetId`: `900000000000509007` (US English)
- `acceptabilityId`: `900000000000548007` (preferred)
- ICD-10 map `refsetId`: `999002271000000101` (UK Extension ICD-10 map)
- OPCS-4 map `refsetId`: `999002321000000109` (dummy value; the real
  OPCS-4 map refset is version specific and auto-detected — see
  `.opcs4_refset_id`)
- `correlationId`: `447561005` (SNOMED CT to target code correlation not
  specified)

#### Acknowledgments

This dummy dataset is derived from the **SNOMED International GPS
Release** and is used for testing purposes only. SNOMED CT is a
registered trademark of the International Health Terminology Standards
Development Organisation (IHTSDO).
