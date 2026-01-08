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
  : Get relationship types for codes
- [`MAP()`](https://codeminer-io.github.io/codeminer/reference/MAP.md) :
  Map clinical codes from one coding system to another
- [`default_col_filters()`](https://codeminer-io.github.io/codeminer/reference/default_col_filters.md)
  : Default filtering parameters for lookup and mapping tables.
- [`get_col_filters()`](https://codeminer-io.github.io/codeminer/reference/get_col_filters.md)
  : Extract column filters from metadata tables

## Database management

Build a local database with lookup and mapping tables for various
clinical coding systems.

- [`get_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_resource_592.md)
  : Get UK Biobank resource 592 directly from UK Biobank website
- [`read_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_resource_592.md)
  : Read UK Biobank Resource 592 clinical code mappings
- [`add_ukb_resource_592()`](https://codeminer-io.github.io/codeminer/reference/add_ukb_resource_592.md)
  : Add UK Biobank Resource 592 tables to CodeMiner database
- [`read_all_lkps_maps()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps.md)
  : Read UK Biobank resource 592 into a named list
- [`build_all_lkps_maps()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps.md)
  : Build named list of clinical code look up and mapping tables
- [`all_lkps_maps_to_db()`](https://codeminer-io.github.io/codeminer/reference/all_lkps_maps_to_db.md)
  : Build a Duckdb database of clinical code look up and mapping tables
- [`build_database()`](https://codeminer-io.github.io/codeminer/reference/build_database.md)
  : Build the Codeminer database
- [`dummy_ukb_resource_592_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_ukb_resource_592_path.md)
  : Get path to dummy UK Biobank Resource 592 file
- [`create_dummy_database()`](https://codeminer-io.github.io/codeminer/reference/create_dummy_database.md)
  : Create a dummy database
- [`add_lookup_table()`](https://codeminer-io.github.io/codeminer/reference/add_lookup_table.md)
  : Add a lookup table to the database
- [`lookup_metadata()`](https://codeminer-io.github.io/codeminer/reference/lookup_metadata.md)
  : Create lookup metadata
- [`add_mapping_table()`](https://codeminer-io.github.io/codeminer/reference/add_mapping_table.md)
  : Add a mapping table to the database
- [`mapping_metadata()`](https://codeminer-io.github.io/codeminer/reference/mapping_metadata.md)
  : Create mapping metadata
- [`add_relationship_table()`](https://codeminer-io.github.io/codeminer/reference/add_relationship_table.md)
  : Add a relationship table to the database
- [`relationship_metadata()`](https://codeminer-io.github.io/codeminer/reference/relationship_metadata.md)
  : Create relationship metadata
- [`get_codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/get_codeminer_metadata.md)
  : Get codeminer metadata

## Dummy data

Dummy data for tests.

- [`build_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/build_all_lkps_maps_dummy.md)
  : Create a dummy all_lkps_maps
- [`read_all_lkps_maps_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_all_lkps_maps_dummy.md)
  : Read dummy UK Biobank resource 592 into R
- [`read_icd10_phecode_map_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_icd10_phecode_map_dummy.md)
  : Read dummy Phecode Map 1.2 with ICD-10 codes (beta) file into R
- [`read_phecode_lkp_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_phecode_lkp_dummy.md)
  : Read dummy Phecode definitions file into R
- [`read_ukb_codings_dummy()`](https://codeminer-io.github.io/codeminer/reference/read_ukb_codings_dummy.md)
  : Read dummy UK Biobank codings into R
- [`dummy_all_lkps_maps_db()`](https://codeminer-io.github.io/codeminer/reference/dummy_all_lkps_maps_db.md)
  : Set up a dummy all_lkps_maps database
- [`dummy_clinical_events_tidy()`](https://codeminer-io.github.io/codeminer/reference/dummy_clinical_events_tidy.md)
  : Dummy UK Biobank clinical events, tidied
- [`dummy_icd10_phecode_map_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_icd10_phecode_map_path.md)
  : Dummy Phecode Map 1.2 with ICD-10 codes (beta) file path
- [`dummy_phecode_lkp_path()`](https://codeminer-io.github.io/codeminer/reference/dummy_phecode_lkp_path.md)
  : Dummy Phecode definitions file path
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
- [`codeminer_metadata()`](https://codeminer-io.github.io/codeminer/reference/codeminer_metadata.md)
  : Metadata
- [`get_bnf_from_open_prescribing()`](https://codeminer-io.github.io/codeminer/reference/get_bnf_from_open_prescribing.md)
  : Get the BNF terminology from OpenPrescribing
- [`get_nhsbsa_snomed_bnf()`](https://codeminer-io.github.io/codeminer/reference/get_nhsbsa_snomed_bnf.md)
  : Download and read the NHSBSA BNF_SNOMED mapping file
- [`get_ukb_self_report_med_to_atc_map()`](https://codeminer-io.github.io/codeminer/reference/get_ukb_self_report_med_to_atc_map.md)
  : Download and read a UKB welf-reported medication code to ATC mapping
  file
- [`get_phecode_definitions()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_definitions.md)
  : Download the Phecode 1.2 definitions file
- [`get_phecode_icd9_map()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_icd9_map.md)
  : Download the Phecode 1.2 to ICD9 mapping file
- [`get_phecode_icd10_map()`](https://codeminer-io.github.io/codeminer/reference/get_phecode_icd10_map.md)
  : Download the Phecode 1.2 to ICD10 (beta) mapping file
- [`icd10()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`icd9()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`read3()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`read2()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`sct()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`opcs4()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`phecode()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`read2_drugs()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`bnf()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`dmd()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`data_coding_3()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`data_coding_4()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`data_coding_5()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`data_coding_6()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`medcode_aurum()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  [`prodcode_aurum()`](https://codeminer-io.github.io/codeminer/reference/with_code_type_helpers.md)
  : With Code Type Functions
- [`start_api()`](https://codeminer-io.github.io/codeminer/reference/start_api.md)
  : Start Plumber API
