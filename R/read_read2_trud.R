#' Read Read 2 coding files into R
#'
#' Reads the Read V2 lookup and Read 2/CTV3 cross-mapping tables from a local
#' copy of the NHS Data Migration release (TRUD item 9).
#'
#' @param path Path to the NHS Data Migration release. Can be:
#'   * A **zip file** (e.g., from [get_read2_trud()])
#'   * An **unzipped directory** containing the `Mapping Tables` subdirectory
#' @param tables Character vector of table names to read. Available tables:
#'   * `"read2_lkp"` — Read V2 lookup (codes and descriptions)
#'   * `"read2_ctv3"` — Read V2 to CTV3 cross-mapping
#'   * `"ctv3_read2"` — CTV3 to Read V2 cross-mapping
#'
#'   By default, all three tables are read.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with elements corresponding to requested tables, each
#'   containing:
#'   * `lookup` or `mapping`: a list with `table` (data.table) and `metadata`
#'     (list)
#'
#' @seealso [add_read2_trud()], [get_read2_trud()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_read2_trud()
#' result <- read_read2_trud(path)
#' result$read2_lkp$lookup$table
#' result$read2_ctv3$mapping$table
#' result$ctv3_read2$mapping$table
#' }
read_read2_trud <- function(
  path,
  tables = c("read2_lkp", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
) {
  rlang::arg_match(
    tables,
    values = c("read2_lkp", "read2_ctv3", "ctv3_read2"),
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
    extract_dir <- file.path(tempdir(), "codeminer_read2_extract")
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

  updated_dir <- file.path(path, "Mapping Tables", "Updated")

  if (!dir.exists(updated_dir)) {
    cli::cli_abort(c(
      "x" = "Expected subdirectory not found: {.file {updated_dir}}",
      "i" = "Check that the path points to a valid NHS Data Migration release directory."
    ))
  }

  not_assured_dir <- file.path(updated_dir, "Not Clinically Assured")
  clinically_assured_dir <- file.path(updated_dir, "Clinically Assured")

  result <- list()

  if ("read2_lkp" %in% tables) {
    cli::cli_inform("Loading Read V2 lookup...")

    lkp_files <- list.files(
      not_assured_dir,
      pattern = "^rctermsctmap_uk_",
      full.names = TRUE
    )

    if (length(lkp_files) == 0) {
      cli::cli_abort(c(
        "x" = "Could not find Read V2 lookup file (rctermsctmap_uk_*.txt)",
        "i" = "Expected under: {.file {not_assured_dir}}"
      ))
    }

    read2_lkp_table <- data.table::fread(
      lkp_files[[1]],
      sep = "\t",
      colClasses = "character"
    )

    read2_lkp_metadata <- lookup_metadata(
      code_type = "read2",
      lookup_version = version,
      lookup_code_col = "ReadCode",
      lookup_description_col = "Term",
      lookup_source = source
    )

    result$read2_lkp <- list(
      lookup = list(
        table = read2_lkp_table,
        metadata = read2_lkp_metadata
      )
    )
  }

  if ("read2_ctv3" %in% tables) {
    cli::cli_inform("Loading Read V2 to CTV3 cross-mapping...")

    r2c3_files <- list.files(
      clinically_assured_dir,
      pattern = "^rctctv3map_uk_",
      full.names = TRUE
    )

    if (length(r2c3_files) == 0) {
      cli::cli_abort(c(
        "x" = "Could not find Read V2 to CTV3 mapping file (rctctv3map_uk_*.txt)",
        "i" = "Expected under: {.file {clinically_assured_dir}}"
      ))
    }

    read2_ctv3_table <- data.table::fread(
      r2c3_files[[1]],
      sep = "\t",
      colClasses = "character"
    )

    read2_ctv3_metadata <- mapping_metadata(
      from_code_type = "read2",
      to_code_type = "read3",
      map_version = version,
      from_col = "V2_CONCEPTID",
      to_col = "CTV3_CONCEPTID",
      map_source = source
    )

    result$read2_ctv3 <- list(
      mapping = list(
        table = read2_ctv3_table,
        metadata = read2_ctv3_metadata
      )
    )
  }

  if ("ctv3_read2" %in% tables) {
    cli::cli_inform("Loading CTV3 to Read V2 cross-mapping...")

    c3r2_files <- list.files(
      clinically_assured_dir,
      pattern = "^ctv3rctmap_uk_",
      full.names = TRUE
    )

    if (length(c3r2_files) == 0) {
      cli::cli_abort(c(
        "x" = "Could not find CTV3 to Read V2 mapping file (ctv3rctmap_uk_*.txt)",
        "i" = "Expected under: {.file {clinically_assured_dir}}"
      ))
    }

    ctv3_read2_table <- data.table::fread(
      c3r2_files[[1]],
      sep = "\t",
      colClasses = "character"
    )

    ctv3_read2_metadata <- mapping_metadata(
      from_code_type = "read3",
      to_code_type = "read2",
      map_version = version,
      from_col = "CTV3_CONCEPTID",
      to_col = "V2_CONCEPTID",
      map_source = source
    )

    result$ctv3_read2 <- list(
      mapping = list(
        table = ctv3_read2_table,
        metadata = ctv3_read2_metadata
      )
    )
  }

  result
}
