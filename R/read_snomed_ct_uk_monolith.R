#' Read the SNOMED CT UK Monolith Edition into R
#'
#' Reads the Terminology and Refset snapshot tables from a local copy of the
#' **SNOMED CT UK Monolith Edition** release files.
#'
#' This function imports the key data files as `data.table` objects, performs
#' basic normalisation, and constructs joined tables for concept descriptions
#' and ICD-10/OPCS-4 mappings.
#'
#' For more information on the SNOMED CT UK Monolith Edition, see the [NHS TRUD
#' website](https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases).
#'
#' @param path A character string giving the path to the unzipped SNOMED CT UK
#'   Monolith Edition directory. This directory must contain the subdirectories
#'   `Snapshot/Terminology` and `Snapshot/Refset`.
#' @param tables Character vector of table names to read. Available tables are:
#'   * Lookup table: `"sct_lookup"`
#'   * Relationship table: `"sct_relationship"`
#'   * Mapping tables: `"sct_icd10"`, `"sct_opcs4"`
#'
#'   By default, all tables are read.
#' @param version Character string specifying the version label for the SNOMED
#'   CT release. If not provided, it will be derived from the folder name by
#'   appending `.zip` (matching the TRUD download filename format).
#' @param source Character string specifying the source URL or description.
#'   Defaults to the NHS TRUD website.
#' @param .icd10_refset_id Character string. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for ICD-10 mappings.
#'   Defaults to `"999002271000000101"` (UK Clinical Extension Extended Map to
#'   ICD-10). This is an advanced parameter that typically does not need to be
#'   changed.
#' @param .opcs4_refset_id Character string. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for OPCS-4 mappings.
#'   Defaults to `"999002321000000109"` (UK Clinical Extension Extended Map to
#'   OPCS-4). This is an advanced parameter that typically does not need to be
#'   changed.
#'
#' @return A named list with elements corresponding to requested tables, each
#'   containing tables and metadata:
#' * `sct_lookup`: Lookup table with SNOMED CT concepts and descriptions.
#'   Contains `table` (data.table) and `metadata` (list).
#' * `sct_relationship`: Relationship table with concept relationships.
#'   Contains `table` (data.table) and `metadata` (list).
#' * `sct_icd10`: Mapping table with SNOMED CT to ICD-10 mappings.
#'   Contains `table` (data.table) and `metadata` (list).
#' * `sct_opcs4`: Mapping table with SNOMED CT to OPCS-4 mappings.
#'   Contains `table` (data.table) and `metadata` (list).
#'
#' @examples
#' \dontrun{
#' # Read all tables from a local SNOMED CT UK Monolith Edition
#' snomed <- read_snomed_ct_uk_monolith("~/SNOMEDCT_Release_UK")
#'
#' # Read only specific tables
#' snomed <- read_snomed_ct_uk_monolith(
#'   path = "~/SNOMEDCT_Release_UK",
#'   tables = c("sct_lookup", "sct_icd10")
#' )
#'
#' # Access lookup table and metadata
#' sct_lookup <- snomed$sct_lookup$lookup$table
#' sct_metadata <- snomed$sct_lookup$lookup$metadata
#'
#' # Specify custom version
#' snomed <- read_snomed_ct_uk_monolith(
#'   path = "~/SNOMEDCT_Release_UK",
#'   version = "UK_20240101"
#' )
#' }
#' @export
read_snomed_ct_uk_monolith <- function(
  path,
  tables = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/",
  .icd10_refset_id = "999002271000000101",
  .opcs4_refset_id = "999002321000000109"
) {
  # 1. Validation -----------------------------------------------------------
  if (!dir.exists(path)) {
    cli::cli_abort("Directory does not exist: {.path {path}}")
  }

  # Validate tables argument
  rlang::arg_match(
    tables,
    values = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
    multiple = TRUE
  )

  cli::cli_inform("Checking directory...")
  # Define critical subdirectories
  dirs_to_check <- file.path(
    path,
    "Snapshot",
    c("Refset/Map", "Terminology")
  )

  missing <- dirs_to_check[!dir.exists(dirs_to_check)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "!" = "The following required subdirectories are missing:",
      "x" = paste(missing, collapse = "\n")
    ))
  }

  term_dir <- file.path(path, "Snapshot", "Terminology")
  refset_dir <- file.path(path, "Snapshot", "Refset", "Map")

  # Determine version from folder name if not provided
  if (is.null(version)) {
    version <- paste0(basename(path), ".zip")
  }

  # 2. File Discovery -------------------------------------------------------
  # Uses the extracted helper 'find_snomed_file'

  # Determine which files need to be loaded
  need_terminology <- any(c("sct_lookup", "sct_relationship") %in% tables)
  need_map <- any(c("sct_icd10", "sct_opcs4") %in% tables)

  if (need_terminology) {
    path_desc <- find_snomed_file(term_dir, "^sct2_Description_.*Snapshot")
    path_conc <- find_snomed_file(term_dir, "^sct2_Concept_.*Snapshot")
  }

  if ("sct_relationship" %in% tables) {
    path_rel <- find_snomed_file(term_dir, "^sct2_Relationship_.*Snapshot")
  }

  if (need_map) {
    path_map_all <- find_snomed_file(
      refset_dir,
      "^der2_.*ExtendedMap.*Snapshot"
    )
  }

  # 3. Data Loading & Processing --------------------------------------------

  result <- list()

  # --- Concepts & Descriptions (Lookup) ---
  if ("sct_lookup" %in% tables) {
    cli::cli_inform("Loading Concepts and Descriptions...")

    dt_desc <- fread_sct(path_desc) |>
      dplyr::rename_with(\(x) paste0(x, "_description"))

    dt_conc <- fread_sct(path_conc) |>
      dplyr::rename_with(\(x) paste0(x, "_concept"))

    # Merge (Full Join)
    sct_lookup_table <- dt_desc |>
      dplyr::full_join(
        dt_conc,
        by = c("conceptId_description" = "id_concept")
      ) |>
      dplyr::rename(conceptId = .data$conceptId_description)

    sct_lookup_metadata <- lookup_metadata(
      code_type = "sct",
      lookup_version = version,
      lookup_code_col = "conceptId",
      lookup_description_col = "term_description",
      lookup_source = source,
      preferred_description_col = "typeId_description",
      preferred_description_indicator = "900000000000003001"
    )

    result$sct_lookup <- list(
      lookup = list(
        table = sct_lookup_table,
        metadata = sct_lookup_metadata
      )
    )
  }

  # --- Relationships ---
  if ("sct_relationship" %in% tables) {
    cli::cli_inform("Loading Relationships...")
    sct_relationship_table <- fread_sct(path_rel)

    sct_relationship_metadata <- relationship_metadata(
      code_type = "sct",
      relationship_version = version,
      from_col = "sourceId",
      to_col = "destinationId",
      type_col = "typeId",
      child_parent_relationship_code = "116680003",
      relationship_source = source
    )

    result$sct_relationship <- list(
      relationship = list(
        table = sct_relationship_table,
        metadata = sct_relationship_metadata
      )
    )
  }

  # --- Clinical Code Mappings ---
  if (need_map) {
    cli::cli_inform("Loading Clinical Code Mappings...")
    # There may be more than one matching file
    raw_maps <- purrr::map(path_map_all, fread_sct) |>
      dplyr::bind_rows()
  }

  if ("sct_icd10" %in% tables) {
    # Filter for ICD-10 Refset ID and exclude blocks (entries with '#' in mapTarget)
    sct_icd10_table <- raw_maps |>
      dplyr::filter(
        .data$refsetId == .env$.icd10_refset_id,
        !stringr::str_detect(.data$mapTarget, "#")
      )

    sct_icd10_metadata <- mapping_metadata(
      from_code_type = "sct",
      to_code_type = "icd10",
      map_version = version,
      from_col = "referencedComponentId",
      to_col = "mapTarget",
      map_source = source
    )

    result$sct_icd10 <- list(
      mapping = list(
        table = sct_icd10_table,
        metadata = sct_icd10_metadata
      )
    )
  }

  if ("sct_opcs4" %in% tables) {
    # Filter for OPCS-4 Refset ID and exclude blocks
    sct_opcs4_table <- raw_maps |>
      dplyr::filter(
        .data$refsetId == .env$.opcs4_refset_id,
        !stringr::str_detect(.data$mapTarget, "#")
      )

    sct_opcs4_metadata <- mapping_metadata(
      from_code_type = "sct",
      to_code_type = "opcs4",
      map_version = version,
      from_col = "referencedComponentId",
      to_col = "mapTarget",
      map_source = source
    )

    result$sct_opcs4 <- list(
      mapping = list(
        table = sct_opcs4_table,
        metadata = sct_opcs4_metadata
      )
    )
  }

  # 4. Return ---------------------------------------------------------------
  cli::cli_progress_done()

  result
}

#' Find a SNOMED file by pattern
#'
#' @description Locates a single file matching a regex pattern within a
#' directory. Errors if multiple matches are found or if none are found.
#'
#' @param dir Directory path to search.
#' @param pattern Regex pattern to match.
#' @param call The execution environment of a currently running function. Used
#'   for error reporting. Defaults to the caller environment.
#'
#' @return The full path to the matched file.
#' @noRd
find_snomed_file <- function(dir, pattern, call = rlang::caller_env()) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)

  if (length(f) == 0) {
    cli::cli_abort(
      c(
        "x" = "Could not find file matching {.val {pattern}} in {.path {dir}}",
        "i" = "Ensure the SNOMED CT directory structure is correct and contains the required RF2 files."
      ),
      call = call
    )
  }

  if (length(f) > 1) {
    cli::cli_inform(
      c(
        "!" = "Multiple files found for {.val {pattern}} in {.path {dir}}",
        "i" = "Found: {.file {basename(f)}}"
      )
    )
  }

  return(f)
}

#' Fast Read for SNOMED CT Files
#'
#' Wraps data.table::fread with default settings for SNOMED RF2 files
#' (tab-separated, all character columns to preserve IDs).
#'
#' @param file_path Path to the file.
#' @noRd
fread_sct <- function(file_path) {
  data.table::fread(
    file_path,
    sep = "\t",
    colClasses = "character",
    quote = ""
  )
}
