# Package index

## Clinical codes

Functions for looking up clinical codes and mapping between different
coding systems.

- [`DESCRIPTION()`](https://codeminer-io.github.io/codeminer/reference/DESCRIPTION.md)
  : Search for codes that match a description
- [`CODES()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  [`CODES_LIKE()`](https://codeminer-io.github.io/codeminer/reference/CODES.md)
  : Look up descriptions for clinical codes
- [`CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  [`PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  [`N_CHILDREN()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  [`N_PARENTS()`](https://codeminer-io.github.io/codeminer/reference/parent_child_retrieval.md)
  : Retrieve parent or child codes
- [`ATTRIBUTES_FOR()`](https://codeminer-io.github.io/codeminer/reference/attributes.md)
  [`HAS_ATTRIBUTES()`](https://codeminer-io.github.io/codeminer/reference/attributes.md)
  : Get attributes or codes with attributes
- [`RELATIONSHIP_TYPES_FROM()`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)
  [`RELATIONSHIP_TYPES_TO()`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)
  [`RELATIONSHIP_TYPES()`](https://codeminer-io.github.io/codeminer/reference/relationship_types.md)
  : Relationship types
- [`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md) :
  Map clinical codes from one coding system to another
- [`get_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/get_lookup_table.md)
  : Get the full lookup table for a code type
- [`get_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/get_mapping_table.md)
  : Get the full mapping table for a pair of code types
- [`get_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_table.md)
  : Get the full relationship table for a code type
- [`get_relationship_tree()`](https://codeminer-io.github.io/codeminer/reference/get_relationship_tree.md)
  : Build a flat nodes/edges tree view for a set of codes
- [`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md)
  : Default column filters
- [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
  : Extract column filters from database metadata

## Workbench management

Connect to and manage the codeminer workbench (persistent DuckDB
connection).

- [`codeminer_connect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_connect.md)
  : Connect to the codeminer workbench
- [`codeminer_disconnect()`](https://codeminer-io.github.io/codeminer/reference/codeminer_disconnect.md)
  : Disconnect the codeminer workbench
- [`codeminer_status()`](https://codeminer-io.github.io/codeminer/reference/codeminer_status.md)
  : Show workbench status
- [`codeminer_refresh_cache()`](https://codeminer-io.github.io/codeminer/reference/codeminer_refresh_cache.md)
  : Refresh the metadata cache
- [`codeminer_set_version()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_version.md)
  : Pin table versions for the session
- [`codeminer_clear_versions()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_versions.md)
  : Clear active version selections
- [`codeminer_set_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_set_col_filters.md)
  : Pin column filters for the session
- [`codeminer_clear_col_filters()`](https://codeminer-io.github.io/codeminer/reference/codeminer_clear_col_filters.md)
  : Clear all pinned column filters
- [`with_col_filters()`](https://codeminer-io.github.io/codeminer/reference/with_col_filters.md)
  : Temporarily override column filters

## Database management

Build a local database with lookup and mapping tables for various
clinical coding systems.

- [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  : Get UK Biobank resource 592 directly from UK Biobank website
- [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
  : Read UK Biobank Resource 592 clinical code mappings
- [`add_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/add_ukb_resource_592.md)
  : Add UK Biobank Resource 592 tables to CodeMiner database
- [`get_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/get_snomed_ct_uk_monolith.md)
  : Get SNOMED CT UK Monolith Edition from NHS TRUD
- [`read_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/read_snomed_ct_uk_monolith.md)
  : Read the SNOMED CT UK Monolith Edition into R
- [`add_snomed_ct_uk_monolith()`](https://codeminer-io.github.io/codeminer/reference/add_snomed_ct_uk_monolith.md)
  : Add SNOMED CT UK Monolith tables to CodeMiner database
- [`get_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_codings.md)
  : Download the UK Biobank codings file
- [`read_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings.md)
  : Read UK Biobank codings file into lookup tables
- [`add_ukb_codings()`](https://codeminer-io.github.io/codeminer/reference/add_ukb_codings.md)
  : Add UK Biobank codings tables to CodeMiner database
- [`get_phecode()`](https://codeminer-io.github.io/codeminer/reference/get_phecode.md)
  : Download Phecode 1.2 files
- [`read_phecode()`](https://codeminer-io.github.io/codeminer/reference/read_phecode.md)
  : Read Phecode 1.2 files into lookup and mapping tables
- [`add_phecode()`](https://codeminer-io.github.io/codeminer/reference/add_phecode.md)
  : Add Phecode 1.2 tables to CodeMiner database
- [`get_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_bnf_snomed.md)
  : Download the NHSBSA BNF-SNOMED mapping file
- [`read_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/read_nhsbsa_bnf_snomed.md)
  : Read the NHSBSA BNF-SNOMED mapping file into a mapping table
- [`add_nhsbsa_bnf_snomed()`](https://codeminer-io.github.io/codeminer/reference/add_nhsbsa_bnf_snomed.md)
  : Add NHSBSA BNF-SNOMED mapping table to CodeMiner database
- [`get_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/get_nhs_data_migration.md)
  : Get NHS Data Migration mapping tables from NHS TRUD
- [`read_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/read_nhs_data_migration.md)
  : Read NHS Data Migration mapping tables into R
- [`add_nhs_data_migration()`](https://codeminer-io.github.io/codeminer/reference/add_nhs_data_migration.md)
  : Add NHS Data Migration mapping tables to CodeMiner database
- [`get_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/get_icd10_trud.md)
  : Get ICD-10 coding system files from NHS TRUD
- [`read_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_trud.md)
  : Read ICD-10 coding files into R
- [`add_icd10_trud()`](https://codeminer-io.github.io/codeminer/reference/add_icd10_trud.md)
  : Add ICD-10 lookup table to CodeMiner database
- [`get_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/get_opcs4_trud.md)
  : Get OPCS-4 coding system files from NHS TRUD
- [`read_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/read_opcs4_trud.md)
  : Read OPCS-4 coding files into R
- [`add_opcs4_trud()`](https://codeminer-io.github.io/codeminer/reference/add_opcs4_trud.md)
  : Add OPCS-4 lookup table to CodeMiner database
- [`get_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read3_trud.md)
  : Get Read 3 (CTV3) coding system files from NHS TRUD
- [`read_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read3_trud.md)
  : Read Read 3 (CTV3) coding files into R
- [`add_read3_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read3_trud.md)
  : Add Read 3 (CTV3) tables to CodeMiner database
- [`get_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/get_read2_trud.md)
  : Get Read 2 coding system files from NHS TRUD
- [`read_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/read_read2_trud.md)
  : Read Read 2 coding files into R
- [`add_read2_trud()`](https://codeminer-io.github.io/codeminer/reference/add_read2_trud.md)
  : Add Read 2 tables to CodeMiner database
- [`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
  : Build the Codeminer database
- [`validate_database()`](https://codeminer-io.github.io/codeminer/reference/validate_database.md)
  : Validate the codeminer database for on-disk inconsistencies
- [`dummy_ukb_resource_592_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_resource_592_path.md)
  : Get path to dummy UK Biobank Resource 592 file
- [`create_dummy_database()`](https://codeminer-io.github.io/codeminer/reference/create_dummy_database.md)
  : Create a dummy database
- [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  : Add a lookup table to the database
- [`remove_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/remove_lookup_table.md)
  : Remove a lookup table from the database
- [`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
  : Create lookup metadata
- [`update_lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_lookup_metadata.md)
  : Update lookup table metadata
- [`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
  : Add a mapping table to the database
- [`remove_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/remove_mapping_table.md)
  : Remove a mapping table from the database
- [`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md)
  : Create mapping metadata
- [`update_mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_mapping_metadata.md)
  : Update mapping table metadata
- [`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)
  : Add a relationship table to the database
- [`remove_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/remove_relationship_table.md)
  : Remove a relationship table from the database
- [`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md)
  : Create relationship metadata
- [`update_relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/update_relationship_metadata.md)
  : Update relationship table metadata
- [`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
  : Get codeminer metadata

## Dummy data

Dummy data for tests.

- [`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md)
  : Read dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R
- [`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md)
  : Read dummy Phecode definitions file into R
- [`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)
  : Read dummy UK Biobank codings into R
- [`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md)
  : Dummy Phecode Map 1.2 with ICD-10 codes (beta) file path
- [`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md)
  : Dummy Phecode definitions file path
- [`dummy_snomed_ct_uk_monolith_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_snomed_ct_uk_monolith_path.md)
  : Get full path to the dummy SNOMED CT GPS RF2 files
- [`dummy_ukb_codings_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_codings_path.md)
  : Dummy UK Biobank codings file path
- [`dummy_ukb_resource_592_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_resource_592_path.md)
  : Get path to dummy UK Biobank Resource 592 file
- [`example_ontology`](https://codeminer-io.github.io/codeminer/reference/example_ontology.md)
  : Example ontology data

## Utilities

Miscellaneous utility functions.

- [`` `%AND%` ``](https://codeminer-io.github.io/codeminer/reference/infix_setops.md)
  [`` `%OR%` ``](https://codeminer-io.github.io/codeminer/reference/infix_setops.md)
  [`` `%NOT%` ``](https://codeminer-io.github.io/codeminer/reference/infix_setops.md)
  : Infix set operations
- [`with_type()`](https://codeminer-io.github.io/codeminer/reference/with_type.md)
  : Set code type context
- [`collect_codes_input()`](https://codeminer-io.github.io/codeminer/reference/collect_codes_input.md)
  : Collect and validate codes input from ... argument
