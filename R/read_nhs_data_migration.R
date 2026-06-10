#' Read NHS Data Migration mapping tables into R
#'
#' Reads the clinically assured Read V2 ↔ CTV3 ↔ SNOMED CT mapping tables
#' from the NHS Data Migration release (TRUD item 9).
#'
#' @param path Path to the NHS Data Migration release. Can be:
#'   * A **zip file** (e.g., from [get_nhs_data_migration()])
#'   * An **unzipped directory** containing the `Mapping Tables` subdirectory
#' @param tables Character vector of table names to read. Available tables:
#'   * `"ctv3sctmap2"` — CTV3 (Read 3) to SNOMED CT clinically assured mapping
#'   * `"rcsctmap2"` — Read V2 to SNOMED CT clinically assured mapping
#'   * `"read2_ctv3"` — Read V2 to CTV3 (Read 3) cross-mapping
#'   * `"ctv3_read2"` — CTV3 (Read 3) to Read V2 cross-mapping
#'
#'   By default, all tables are read.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with elements corresponding to requested tables, each
#'   containing:
#'   * `mapping`: a list with `table` (data.table) and `metadata` (list)
#'
#' @seealso [add_nhs_data_migration()], [get_nhs_data_migration()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_nhs_data_migration()
#' result <- read_nhs_data_migration(path)
#' result$ctv3sctmap2$mapping$table
#' result$rcsctmap2$mapping$table
#' result$read2_ctv3$mapping$table
#' result$ctv3_read2$mapping$table
#' }
read_nhs_data_migration <- function(
  path,
  tables = c("ctv3sctmap2", "rcsctmap2", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
) {
  rlang::arg_match(
    tables,
    values = c("ctv3sctmap2", "rcsctmap2", "read2_ctv3", "ctv3_read2"),
    multiple = TRUE
  )

  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("Path does not exist: {.path {path}}")
  }

  # Handle zip input — extract on-demand
  if (
    file.exists(path) &&
      !dir.exists(path) &&
      grepl("\\.zip$", path, ignore.case = TRUE)
  ) {
    if (is.null(version)) {
      version <- basename(path)
    }

    cli::cli_inform("Extracting NHS Data Migration from zip file...")
    extract_dir <- file.path(tempdir(), "codeminer_nhs_data_migration_extract")
    if (!dir.exists(extract_dir)) {
      dir.create(extract_dir, recursive = TRUE)
    }
    utils::unzip(path, exdir = extract_dir, overwrite = TRUE)

    extracted_dirs <- list.dirs(
      extract_dir,
      recursive = FALSE,
      full.names = TRUE
    )
    if (length(extracted_dirs) == 0) {
      cli::cli_abort(c(
        "x" = "No directory found after extracting zip",
        "i" = "The zip file should contain a single top-level directory"
      ))
    }
    path <- extracted_dirs[[1]]
  }

  if (is.null(version)) {
    version <- basename(path)
  }

  # Locate the clinically assured mapping tables directory
  clinically_assured_dir <- file.path(path, "Updated", "Clinically Assured")
  if (!dir.exists(clinically_assured_dir)) {
    clinically_assured_dir <- file.path(
      path,
      "Mapping Tables",
      "Updated",
      "Clinically Assured"
    )
  }

  if (!dir.exists(clinically_assured_dir)) {
    cli::cli_abort(c(
      "x" = "Expected subdirectory not found: {.file {clinically_assured_dir}}",
      "i" = "Check that the path points to a valid NHS Data Migration release directory."
    ))
  }

  # File name patterns for each table (date suffix varies between releases)
  file_patterns <- c(
    ctv3sctmap2 = "^ctv3sctmap2_uk_",
    rcsctmap2 = "^rcsctmap2_uk_",
    read2_ctv3 = "^rctctv3map_uk_",
    ctv3_read2 = "^ctv3rctmap_uk_"
  )

  result <- list()

  for (tbl in tables) {
    matched <- list.files(
      clinically_assured_dir,
      pattern = file_patterns[[tbl]],
      full.names = TRUE
    )

    if (length(matched) == 0) {
      cli::cli_abort(c(
        "x" = "Could not find {.val {tbl}} file (pattern: {.val {file_patterns[[tbl]]}})",
        "i" = "Expected under: {.file {clinically_assured_dir}}"
      ))
    }

    tbl_data <- data.table::fread(
      matched[[1]],
      sep = "\t",
      colClasses = "character"
    )

    meta <- switch(
      tbl,
      ctv3sctmap2 = mapping_metadata(
        from_code_type = "Read v3",
        to_code_type = "SNOMED CT",
        map_version = version,
        from_col = "CTV3_CONCEPTID",
        to_col = "SCT_CONCEPTID",
        map_source = source
      ),
      rcsctmap2 = mapping_metadata(
        from_code_type = "Read v2",
        to_code_type = "SNOMED CT",
        map_version = version,
        from_col = "ReadCode",
        to_col = "ConceptId",
        map_source = source
      ),
      read2_ctv3 = mapping_metadata(
        from_code_type = "Read v2",
        to_code_type = "Read v3",
        map_version = version,
        from_col = "V2_CONCEPTID",
        to_col = "CTV3_CONCEPTID",
        map_source = source
      ),
      ctv3_read2 = mapping_metadata(
        from_code_type = "Read v3",
        to_code_type = "Read v2",
        map_version = version,
        from_col = "CTV3_CONCEPTID",
        to_col = "V2_CONCEPTID",
        map_source = source
      )
    )

    result[[tbl]] <- list(
      mapping = list(
        table = tbl_data,
        metadata = meta
      )
    )
  }

  result
}
