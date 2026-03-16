#' Read NHS Data Migration mapping tables into R
#'
#' Reads the clinically assured Read V2 and CTV3 to SNOMED CT mapping tables
#' from the NHS Data Migration release.
#'
#' @param path Path to the NHS Data Migration release. Can be:
#'   * A **zip file** (e.g., from [get_nhs_data_migration()])
#'   * An **unzipped directory** containing the `Mapping Tables` subdirectory
#' @param tables Character vector of table names to read. Available tables:
#'   * `"ctv3sctmap2"` — CTV3 (Read 3) to SNOMED CT clinically assured mapping
#'   * `"rcsctmap2"` — Read V2 to SNOMED CT clinically assured mapping
#'
#'   By default, both tables are read.
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
#' }
read_nhs_data_migration <- function(
  path,
  tables = c("ctv3sctmap2", "rcsctmap2"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
) {
  rlang::arg_match(
    tables,
    values = c("ctv3sctmap2", "rcsctmap2"),
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
  clinically_assured_dir <- file.path(
    path,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )

  if (!dir.exists(clinically_assured_dir)) {
    cli::cli_abort(c(
      "x" = "Expected subdirectory not found: {.file {clinically_assured_dir}}",
      "i" = "Check that the path points to a valid NHS Data Migration release directory."
    ))
  }

  # Known file names for the two tables
  file_map <- c(
    ctv3sctmap2 = "ctv3sctmap2_uk_20200401000001.txt",
    rcsctmap2 = "rcsctmap2_uk_20200401000001.txt"
  )

  result <- list()

  for (tbl in tables) {
    file_path <- file.path(clinically_assured_dir, file_map[[tbl]])

    if (!file.exists(file_path)) {
      cli::cli_abort(c(
        "x" = "File not found: {.file {file_map[[tbl]]}}",
        "i" = "Expected at: {.file {clinically_assured_dir}}"
      ))
    }

    tbl_data <- data.table::fread(
      file_path,
      sep = "\t",
      colClasses = "character"
    )

    if (tbl == "ctv3sctmap2") {
      meta <- mapping_metadata(
        from_code_type = "read3",
        to_code_type = "sct",
        map_version = version,
        map_source = source
      )
    } else {
      meta <- mapping_metadata(
        from_code_type = "read2",
        to_code_type = "sct",
        map_version = version,
        map_source = source
      )
    }

    result[[tbl]] <- list(
      mapping = list(
        table = tbl_data,
        metadata = meta
      )
    )
  }

  result
}
