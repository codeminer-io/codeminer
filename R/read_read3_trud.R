#' Read Read 3 (CTV3) coding files into R
#'
#' Reads the Read Codes Version 3 (CTV3) lookup and relationship tables from a
#' local copy of the TRUD release files (item 19).
#'
#' Only active codes (status not `"R"`) with preferred (`"P"`) and clinical
#' (`"C"`) descriptions are included in the lookup table.
#'
#' @param path Path to the Read 3 release. Can be:
#'   * A **zip file** (e.g., from [get_read3_trud()])
#'   * An **unzipped directory** containing the `V3` subdirectory
#' @param tables Character vector of table names to read. Available tables:
#'   * `"read3_lkp"` — lookup table of active Read 3 codes with descriptions
#'   * `"read3_relationship"` — hierarchy table (`V3hier.v3`)
#'
#'   By default, both tables are read.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with elements corresponding to requested tables, each
#'   containing:
#'   * `lookup` or `relationship`: a list with `table` (data.table) and
#'     `metadata` (list)
#'
#' @seealso [add_read3_trud()], [get_read3_trud()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_read3_trud()
#' result <- read_read3_trud(path)
#' result$read3_lkp$lookup$table
#' result$read3_relationship$relationship$table
#' }
read_read3_trud <- function(
  path,
  tables = c("read3_lkp", "read3_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
) {
  rlang::arg_match(
    tables,
    values = c("read3_lkp", "read3_relationship"),
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

    cli::cli_inform("Extracting Read 3 from zip file...")
    extract_dir <- file.path(tempdir(), "codeminer_read3_extract")
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
    if (length(extracted_dirs) == 1) {
      path <- extracted_dirs[[1]]
    } else {
      # V3 is one of several top-level directories (e.g. Document, Testdata,
      # V3, Vaf) — use the extract root so file.path(path, "V3") works
      path <- extract_dir
    }
    cli::cli_inform("Using extracted directory: {.path {basename(path)}}")
  }

  if (is.null(version)) {
    version <- basename(path)
  }

  v3_dir <- file.path(path, "V3")

  if (!dir.exists(v3_dir)) {
    cli::cli_abort(c(
      "x" = "Expected subdirectory {.file V3} not found under {.path {path}}",
      "i" = "Check that the path points to a valid Read 3 release directory."
    ))
  }

  result <- list()

  if ("read3_lkp" %in% tables) {
    cli::cli_inform("Loading Read 3 concepts, descriptions, and terms...")

    concept <- data.table::fread(
      file.path(v3_dir, "Concept.v3"),
      sep = "|",
      header = FALSE,
      col.names = c("code", "status", "type", "related_code"),
      colClasses = "character"
    )

    descrip <- data.table::fread(
      file.path(v3_dir, "Descrip.v3"),
      sep = "|",
      header = FALSE,
      col.names = c("code", "description_id", "desc_type"),
      colClasses = "character"
    )

    terms <- data.table::fread(
      file.path(v3_dir, "Terms.v3"),
      sep = "|",
      header = FALSE,
      col.names = c("description_id", "term_type", "term", "alt1", "alt2"),
      colClasses = "character"
    )

    # Active codes only (status != "R"), preferred descriptions (P), clinical terms (C)
    read3_lkp_table <- concept |>
      dplyr::filter(.data$status != "R") |>
      dplyr::inner_join(
        descrip |> dplyr::filter(.data$desc_type == "P"),
        by = "code"
      ) |>
      dplyr::inner_join(
        terms |> dplyr::filter(.data$term_type == "C"),
        by = "description_id"
      )

    read3_lkp_metadata <- lookup_metadata(
      code_type = "read3",
      lookup_version = version,
      lookup_code_col = "code",
      lookup_description_col = "term",
      lookup_source = source
    )

    result$read3_lkp <- list(
      lookup = list(
        table = read3_lkp_table,
        metadata = read3_lkp_metadata
      )
    )
  }

  if ("read3_relationship" %in% tables) {
    cli::cli_inform("Loading Read 3 hierarchy...")

    hier <- data.table::fread(
      file.path(v3_dir, "V3hier.v3"),
      sep = "|",
      header = FALSE,
      col.names = c("child_code", "parent_code", "relationship_type"),
      colClasses = "character"
    )

    read3_relationship_metadata <- relationship_metadata(
      code_type = "read3",
      relationship_version = version,
      from_col = "child_code",
      to_col = "parent_code",
      type_col = "relationship_type",
      child_parent_relationship_code = "01",
      relationship_source = source
    )

    result$read3_relationship <- list(
      relationship = list(
        table = hier,
        metadata = read3_relationship_metadata
      )
    )
  }

  result
}
