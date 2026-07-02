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
#' @param path A character string giving the path to the SNOMED CT UK Monolith
#'   Edition. This can be either:
#'   * A **zip file** (e.g., from [get_snomed_ct_uk_monolith()]) - will be
#'     extracted to a temporary directory on-demand
#'   * An **unzipped directory** containing the subdirectories
#'     `Snapshot/Terminology` and `Snapshot/Refset`
#' @param tables Character vector of table names to read. Available tables are:
#'   * Lookup table: `"sct_lookup"`
#'   * Relationship table: `"sct_relationship"`
#'   * Mapping tables: `"sct_icd10"`, `"sct_opcs4"`
#'
#'   By default, all tables are read.
#' @param version Character string specifying the version label for the SNOMED
#'   CT release. If not provided, it will be derived from the zip filename or
#'   folder name.
#' @param source Character string specifying the source URL or description.
#'   Defaults to the NHS TRUD website.
#' @param .icd10_refset_id Character string. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for ICD-10 mappings.
#'   Defaults to `"999002271000000101"` (UK Clinical Extension Extended Map to
#'   ICD-10). This is an advanced parameter that typically does not need to be
#'   changed.
#' @param .opcs4_refset_id Character string or `NULL`. The SNOMED CT Concept ID
#'   identifying the specific Reference Set (Refset) used for OPCS-4 mappings.
#'   Defaults to `NULL`, which auto-detects the latest OPCS-4 complex map
#'   reference set present in the release (the OPCS-4 map refset is version
#'   specific and changes with each OPCS-4 edition). Pass an explicit Concept ID
#'   to override. This is an advanced parameter that typically does not need to
#'   be changed.
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
#' # Read from a zip file (extracts on-demand)
#' snomed_zip <- get_snomed_ct_uk_monolith()
#' snomed <- read_snomed_ct_uk_monolith(snomed_zip)
#'
#' # Read from an already-extracted directory
#' snomed <- read_snomed_ct_uk_monolith("~/SNOMEDCT_Release_UK")
#'
#' # Read only specific tables
#' snomed <- read_snomed_ct_uk_monolith(
#'   path = snomed_zip,
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
  .opcs4_refset_id = NULL
) {
  # Constants
  snomed_code_type <- "SNOMED CT"
  icd10_code_type <- "ICD-10"
  opcs4_code_type <- "OPCS-4"

  # 1. Validation -----------------------------------------------------------
  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("Path does not exist: {.path {path}}")
  }

  # Validate tables argument
  rlang::arg_match(
    tables,
    values = c("sct_lookup", "sct_relationship", "sct_icd10", "sct_opcs4"),
    multiple = TRUE
  )

  # Handle zip file input - extract on-demand
  if (
    file.exists(path) &&
      !dir.exists(path) &&
      grepl("\\.zip$", path, ignore.case = TRUE)
  ) {
    cli::cli_inform("Extracting SNOMED CT from zip file...")

    # Derive version from zip filename if not provided
    if (is.null(version)) {
      version <- basename(path)
    }

    # Extract to temp directory
    extract_dir <- file.path(tempdir(), "codeminer_snomed_extract")
    if (!dir.exists(extract_dir)) {
      dir.create(extract_dir, recursive = TRUE)
    }

    utils::unzip(path, exdir = extract_dir, overwrite = TRUE)

    # Find the extracted directory (should be the only subdirectory)
    extracted_dirs <- list.dirs(
      extract_dir,
      recursive = FALSE,
      full.names = TRUE
    )

    if (length(extracted_dirs) == 0) {
      cli::cli_abort(c(
        "x" = "No directory found in extracted zip",
        "i" = "The zip file should contain a single top-level directory"
      ))
    }

    if (length(extracted_dirs) > 1) {
      cli::cli_abort(c(
        "x" = "Multiple directories found in extracted zip",
        "i" = "Expected a single top-level directory, found: {.file {basename(extracted_dirs)}}"
      ))
    }

    path <- extracted_dirs[[1]]
    cli::cli_inform("Using extracted directory: {.path {basename(path)}}")
  }

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
  # Descriptions are also needed (just to read the refset concept terms) when
  # auto-detecting the OPCS-4 map refset, even for map-only builds.
  need_desc <- need_terminology ||
    ("sct_opcs4" %in% tables && is.null(.opcs4_refset_id))

  if (need_desc) {
    path_desc <- find_snomed_file(term_dir, "^sct2_Description_.*Snapshot")
  }
  if (need_terminology) {
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
      dplyr::rename("conceptId" = dplyr::all_of("conceptId_description"))

    # Per-concept category derived from the FSN trailing parenthetical, e.g.
    # "Hypertensive disorder (disorder)" -> "Disorder". Each SNOMED concept
    # has at most one active FSN; non-FSN rows inherit the FSN-derived
    # category via the join.
    sct_lookup_table <- snomed_attach_category(sct_lookup_table)

    # SNOMED modules present in this release. The modules are themselves
    # concepts in the lookup (e.g. the UK drug extension module,
    # "999000011000001104", which carries the dm+d content), so derive the
    # set from the data rather than hard-coding it — it then stays accurate
    # across releases. Declared as a filterable column with empty `defaults`,
    # so the default "SNOMED CT" query still returns the full UK release;
    # users can opt into a single module via `col_filters` (e.g.
    # `col_filters = list(moduleId_concept = "999000011000001104")` for a
    # drug-extension-only view).
    module_ids <- sct_lookup_table$moduleId_concept
    module_ids <- sort(unique(module_ids[!is.na(module_ids)]))

    active_labels <- c("1" = "Active", "0" = "Inactive")

    sct_lookup_col_filters <- list(
      active_concept = list(
        values = c("0", "1"),
        defaults = c("0", "1"),
        description = "Whether the concept is active in the release. Inactive concepts are retained for history.",
        value_labels = active_labels
      ),
      active_description = list(
        values = c("0", "1"),
        defaults = c("1"),
        description = "Whether the description (term) is active. Defaults to active descriptions only.",
        value_labels = active_labels
      )
    )
    if (length(module_ids) > 0) {
      # Each module is itself a concept in the lookup, so its human label is
      # derived from its own FSN (see `snomed_module_labels()`) rather than
      # hard-coded — self-documenting and accurate across releases.
      sct_lookup_col_filters$moduleId_concept <- list(
        values = module_ids,
        defaults = character(0),
        description = "SNOMED CT module the concept belongs to. Filter to the UK drug extension for dm+d-only results.",
        value_labels = snomed_module_labels(sct_lookup_table, module_ids)
      )
    }

    sct_lookup_metadata <- lookup_metadata(
      code_type = snomed_code_type,
      lookup_version = version,
      lookup_code_col = "conceptId",
      lookup_description_col = "term_description",
      lookup_category_col = "category",
      lookup_source = source,
      preferred_description_col = "typeId_description",
      preferred_description_indicator = "900000000000003001",
      col_filters = sct_lookup_col_filters
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
      code_type = snomed_code_type,
      relationship_version = version,
      from_col = "sourceId",
      to_col = "destinationId",
      type_col = "typeId",
      child_parent_relationship_code = "116680003",
      relationship_source = source,
      # SNOMED relationships carry an `active` flag: retired (`"0"`) edges are
      # kept in the release for history but no longer hold. Default to active
      # edges only so hierarchy traversals (e.g. `get_relationship_tree()`)
      # don't follow withdrawn is-a links; users can opt back in with
      # `col_filters = NULL`.
      col_filters = list(
        active = list(
          values = c("0", "1"),
          defaults = c("1"),
          description = "Whether the relationship edge is active. Defaults to active edges only.",
          value_labels = c("1" = "Active", "0" = "Inactive")
        )
      )
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
      ) |>
      # Separate trailing dagger (D) / asterisk (A) flag from ICD-10 codes,
      # then strip the X placeholder used to pad 3-char categories so the
      # mapTarget joins cleanly to the ICD-10 lookup. A/D are stripped first
      # because they are appended after the X (e.g., J46XA).
      dplyr::mutate(
        icd10_dagger_asterisk = dplyr::case_when(
          stringr::str_detect(.data$mapTarget, "A$") ~ "A",
          stringr::str_detect(.data$mapTarget, "D$") ~ "D",
          TRUE ~ NA_character_
        ),
        mapTarget = stringr::str_remove(.data$mapTarget, "[AD]$"),
        mapTarget = strip_icd10_x_placeholder(.data$mapTarget)
      )

    # Fail loudly if the ICD-10 refset is absent, rather than silently storing
    # an empty mapping table.
    if (nrow(sct_icd10_table) == 0) {
      cli::cli_abort(c(
        "x" = "No rows found for ICD-10 map refset {.val {(.icd10_refset_id)}}.",
        "i" = "The refset id may have changed in this release; pass an explicit {.arg .icd10_refset_id}."
      ))
    }

    sct_icd10_metadata <- mapping_metadata(
      from_code_type = snomed_code_type,
      to_code_type = icd10_code_type,
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
    # The OPCS-4 map refset id is version specific (a new refset per OPCS-4
    # edition), so auto-detect the latest one present unless overridden. Only
    # refsets actually in `raw_maps` are candidates, so we never pick a map we
    # cannot load.
    opcs4_refset_id <- .opcs4_refset_id
    if (is.null(opcs4_refset_id)) {
      opcs4_refset_id <- detect_opcs4_refset_id(
        path_desc,
        unique(raw_maps$refsetId)
      )
      cli::cli_inform(
        "Auto-detected OPCS-4 map reference set {.val {opcs4_refset_id}}"
      )
    }

    # Filter for OPCS-4 Refset ID and exclude blocks
    sct_opcs4_table <- raw_maps |>
      dplyr::filter(
        .data$refsetId == .env$opcs4_refset_id,
        !stringr::str_detect(.data$mapTarget, "#")
      )

    sct_opcs4_metadata <- mapping_metadata(
      from_code_type = snomed_code_type,
      to_code_type = opcs4_code_type,
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

  result
}

#' Auto-detect the latest OPCS-4 map reference set
#'
#' @description Resolves the SNOMED CT Concept ID of the latest OPCS-4 complex
#' map reference set by matching the refset concept terms in the description
#' file. The OPCS-4 map refset id is version specific (one refset per OPCS-4
#' edition, e.g. 4.9, 4.10, 4.11), so the highest minor version is chosen.
#'
#' The match is pinned to the `Version 4.x` family on purpose: the code type is
#' `"OPCS-4"`, so a future OPCS-5 classification must not be silently absorbed
#' here.
#'
#' @param path_desc Path to the SNOMED CT description snapshot file.
#' @param candidate_refset_ids Character vector of refset ids actually present
#'   in the map data, so we only ever pick a loadable map.
#' @param call The execution environment, for error reporting.
#'
#' @return A character scalar: the Concept ID of the latest OPCS-4 map refset.
#' @noRd
detect_opcs4_refset_id <- function(
  path_desc,
  candidate_refset_ids,
  call = rlang::caller_env()
) {
  desc <- fread_sct(path_desc)

  cand <- desc |>
    dplyr::filter(
      .data$conceptId %in% .env$candidate_refset_ids,
      .data$active == "1",
      stringr::str_detect(
        .data$term,
        stringr::regex(
          "Classification of Interventions and Procedures Version 4\\.\\d+.*complex map reference set",
          ignore_case = TRUE
        )
      )
    ) |>
    dplyr::mutate(
      opcs4_version = as.integer(
        stringr::str_match(.data$term, "Version 4\\.(\\d+)")[, 2]
      )
    ) |>
    dplyr::filter(!is.na(.data$opcs4_version)) |>
    dplyr::distinct(.data$conceptId, .keep_all = TRUE)

  if (nrow(cand) == 0) {
    cli::cli_abort(
      c(
        "x" = "Could not auto-detect an OPCS-4 map reference set in this release.",
        "i" = "Pass an explicit {.arg .opcs4_refset_id}."
      ),
      call = call
    )
  }

  cand |>
    dplyr::slice_max(.data$opcs4_version, n = 1, with_ties = FALSE) |>
    dplyr::pull(.data$conceptId)
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

#' Attach a per-concept `category` derived from the FSN
#'
#' Extracts the trailing parenthetical (e.g. "(disorder)") from each concept's
#' Fully Specified Name and sentence-cases it. Active descriptions are
#' preferred when picking which FSN to extract from, but inactive concepts
#' fall back to their historical FSN so they still get a category.
#'
#' Inactive concepts (`active_concept == "0"`) are flagged with an
#' `(Inactive)` prefix on the category:
#'
#' * concept has an FSN parenthetical -> `(Inactive) Disorder`
#' * concept has no FSN parenthetical -> `(Inactive)`
#'
#' Non-FSN rows for the same concept inherit the category via the left
#' join on `conceptId`.
#'
#' Derive human labels for SNOMED module ids from their own FSNs
#'
#' Each SNOMED module is itself a concept in the lookup, so its label is its
#' active Fully Specified Name with the trailing "(...)" semantic tag stripped
#' (e.g. "SNOMED CT UK drug extension module (core metadata concept)" ->
#' "SNOMED CT UK drug extension module"). Used to populate the `value_labels`
#' of the `moduleId_concept` column filter. Modules with no active FSN in the
#' release are omitted, so `names()` of the result is a subset of `module_ids`.
#'
#' @param sct_lookup_table The merged description/concept lookup table.
#' @param module_ids Character vector of module concept ids present in the
#'   release.
#' @return A named character vector mapping module id to label (possibly
#'   empty).
#' @noRd
snomed_module_labels <- function(sct_lookup_table, module_ids) {
  fsn_type_id <- "900000000000003001"

  rows <- sct_lookup_table |>
    dplyr::filter(
      .data$conceptId %in% .env$module_ids,
      .data$active_description == "1",
      .data$typeId_description == .env$fsn_type_id
    ) |>
    dplyr::distinct(.data$conceptId, .keep_all = TRUE)

  labels <- stringr::str_remove(
    rows$term_description,
    "\\s*\\([^()]*\\)\\s*$"
  )
  names(labels) <- rows$conceptId
  labels
}

#' @param sct_lookup_table The merged description/concept lookup table.
#' @return The same table with a new `category` column.
#' @noRd
snomed_attach_category <- function(sct_lookup_table) {
  fsn_type_id <- "900000000000003001"

  # FSN-derived category per concept. Prefer active descriptions but fall
  # back to any FSN so retired concepts still get a category from their
  # historical FSN.
  fsn_categories <- sct_lookup_table |>
    dplyr::filter(.data$typeId_description == .env$fsn_type_id) |>
    dplyr::mutate(
      category = stringr::str_to_sentence(
        stringr::str_match(
          .data$term_description,
          "\\(([^()]*)\\)\\s*$"
        )[, 2]
      ),
      .desc_priority = dplyr::if_else(
        !is.na(.data$active_description) & .data$active_description == "1",
        1L,
        2L
      )
    ) |>
    dplyr::arrange(.data$conceptId, .data$.desc_priority) |>
    dplyr::distinct(.data$conceptId, .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c("conceptId", "category")))

  sct_lookup_table |>
    dplyr::left_join(fsn_categories, by = "conceptId") |>
    dplyr::mutate(
      category = dplyr::case_when(
        # Inactive concept with an extractable FSN category -> prefix it.
        !is.na(.data$active_concept) &
          .data$active_concept == "0" &
          !is.na(.data$category) ~
          paste0("(Inactive) ", .data$category),
        # Inactive concept with no FSN parenthetical -> bare marker.
        !is.na(.data$active_concept) &
          .data$active_concept == "0" ~
          "(Inactive)",
        # Active concept (or unknown status) -> keep the extracted category
        # as-is, including NA when no parenthetical was present.
        TRUE ~ .data$category
      )
    )
}
