#' Add SNOMED CT UK Monolith tables to CodeMiner database
#'
#' @description Reads SNOMED CT UK Monolith Edition RF2 files and adds these
#'   tables to the active codeminer database. This is a convenience wrapper
#'   around [read_snomed_ct_uk_monolith()] that automatically calls
#'   [add_lookup_table()], [add_mapping_table()], and [add_relationship_table()]
#'   for each table.
#'
#'   The function processes lookup tables, mapping tables, and relationship
#'   tables from the SNOMED CT release and adds them to the database with their
#'   associated metadata.
#'
#' @param path Path to the SNOMED CT UK Monolith Edition. This can be either:
#'   * A **zip file** (e.g., from [get_snomed_ct_uk_monolith()])
#'   * An **unzipped directory** containing the subdirectories
#'     `Snapshot/Terminology` and `Snapshot/Refset`
#'
#'   Default uses [get_snomed_ct_uk_monolith()] to download the latest release.
#' @param tables Character vector of table names to read and add. Available tables:
#'   * Lookup table: `"sct_lookup"`
#'   * Relationship table: `"sct_relationship"`
#'   * Mapping tables: `"sct_icd10"`, `"sct_opcs4"`
#'
#'   By default, adds all tables.
#' @param version Character string specifying the version label for the SNOMED
#'   CT release. If not provided, it will be derived from the zip filename or
#'   folder name.
#' @param source Character string specifying the source URL or description.
#'   Defaults to the NHS TRUD website.
#' @param .icd10_refset_id Character string. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for ICD-10 mappings.
#'   Defaults to `"999002271000000101"`. This is an advanced parameter.
#' @param .opcs4_refset_id Character string. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for OPCS-4 mappings.
#'   Defaults to `"999002321000000109"`. This is an advanced parameter.
#'
#' @returns Invisibly returns the result from [read_snomed_ct_uk_monolith()] (a
#'   named list of tables with metadata).
#'
#' @details The function performs the following steps:
#'
#'   1. Calls [read_snomed_ct_uk_monolith()] to read the specified tables
#'   2. Iterates through each table and table type (lookup/mapping/relationship)
#'   3. Adds each table to the database using the appropriate `add_*` function
#'   4. Returns the parsed data invisibly
#'
#'   This function requires an active database connection set up via
#'   [build_database()] or similar.
#'
#' @seealso
#' * [read_snomed_ct_uk_monolith()] for reading without adding to database
#' * [add_lookup_table()], [add_mapping_table()], [add_relationship_table()]
#' * [get_snomed_ct_uk_monolith()] for downloading from NHS TRUD
#'
#' @export
#' @md
#' @examples
#' \dontrun{
#' # Set up database connection
#' build_database(db_path = "my_codes.db")
#'
#' # Download latest release and add all tables
#' add_snomed_ct_uk_monolith()
#'
#' # Add specific tables only
#' add_snomed_ct_uk_monolith(
#'   tables = c("sct_lookup", "sct_icd10")
#' )
#'
#' # Use local zip file
#' snomed_zip <- "~/data/uk_sct2mo_41.3.0_20251217000001Z.zip"
#' add_snomed_ct_uk_monolith(path = snomed_zip)
#'
#' # Use already-extracted directory
#' add_snomed_ct_uk_monolith(
#'   path = "~/data/SnomedCT_MonolithRF2_PRODUCTION_20251217T120000Z"
#' )
#' }
#'
#' # Example with dummy data (for testing/demonstration)
#' \dontrun{
#' build_database(db_path = tempfile(fileext = ".db"))
#' add_snomed_ct_uk_monolith(
#'   path = dummy_snomed_ct_uk_monolith_path()
#' )
#' }
add_snomed_ct_uk_monolith <- function(
  path = get_snomed_ct_uk_monolith(),
  tables = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/",
  .icd10_refset_id = "999002271000000101",
  .opcs4_refset_id = "999002321000000109"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  expected_names <- c(
    sct_lookup = paste("SNOMED CT", version, sep = "_"),
    sct_relationship = paste("SNOMED CT", "relationship", version, sep = "_"),
    sct_icd10 = paste("SNOMED CT", "ICD-10", version, sep = "_"),
    sct_opcs4 = paste("SNOMED CT", "OPCS4", version, sep = "_")
  )
  expected_types <- c(
    sct_lookup = "lookup",
    sct_relationship = "relationship",
    sct_icd10 = "mapping",
    sct_opcs4 = "mapping"
  )

  sel <- names(expected_names) %in% tables
  if (tables_all_exist(expected_names[sel], expected_types[sel])) {
    codeminer_inform(
      "All requested SNOMED CT tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  snomed_data <- read_snomed_ct_uk_monolith(
    path = path,
    tables = tables,
    version = version,
    source = source,
    .icd10_refset_id = .icd10_refset_id,
    .opcs4_refset_id = .opcs4_refset_id
  )

  add_tables_to_database(snomed_data)

  invisible(snomed_data)
}
