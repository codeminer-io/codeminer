#' Read OPCS-4 coding files into R
#'
#' Reads the OPCS-4 codes and descriptions from a local copy of the TRUD
#' release files.
#'
#' @param path Path to the OPCS-4 release. Can be:
#'   * A **zip file** (e.g., from [get_opcs4_trud()])
#'   * An **unzipped directory** containing the codes and titles file
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with element `opcs4_lkp`, containing:
#'   * `lookup`: a list with `table` (data.table) and `metadata` (list)
#'
#'   The `table` has columns `opcs4_code` and `description`.
#'
#' @seealso [add_opcs4_trud()], [get_opcs4_trud()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_opcs4_trud()
#' result <- read_opcs4_trud(path)
#' result$opcs4_lkp$lookup$table
#' }
read_opcs4_trud <- function(
  path,
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
) {
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

    cli::cli_inform("Extracting OPCS-4 from zip file...")
    extract_dir <- file.path(tempdir(), "codeminer_opcs4_extract")
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
    if (length(extracted_dirs) > 1) {
      cli::cli_abort(c(
        "x" = "Multiple directories found in extracted zip",
        "i" = "Expected a single top-level directory, found: {.file {basename(extracted_dirs)}}"
      ))
    }
    path <- extracted_dirs[[1]]
    cli::cli_inform("Using extracted directory: {.path {basename(path)}}")
  }

  if (is.null(version)) {
    version <- basename(path)
  }

  cli::cli_inform("Discovering OPCS-4 codes file...")

  opcs4_files <- list.files(
    path,
    pattern = "CodesAndTitles",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(opcs4_files) == 0) {
    cli::cli_abort(c(
      "x" = "Could not find OPCS-4 codes file",
      "i" = "Expected a file matching 'CodesAndTitles' under {.path {path}}"
    ))
  }

  if (length(opcs4_files) > 1) {
    cli::cli_inform(c(
      "!" = "Multiple OPCS-4 files found; using the first:",
      "i" = "{.file {basename(opcs4_files[[1]])}}"
    ))
  }

  cli::cli_inform("Loading OPCS-4 codes...")

  opcs4_table <- data.table::fread(
    opcs4_files[[1]],
    sep = "\t",
    header = FALSE,
    col.names = c("opcs4_code", "description"),
    colClasses = "character"
  )

  opcs4_metadata <- lookup_metadata(
    code_type = "OPCS4",
    lookup_version = version,
    lookup_code_col = "opcs4_code",
    lookup_description_col = "description",
    lookup_source = source
  )

  list(
    opcs4_lkp = list(
      lookup = list(
        table = opcs4_table,
        metadata = opcs4_metadata
      )
    )
  )
}
