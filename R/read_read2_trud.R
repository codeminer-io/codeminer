#' Read Read 2 coding files into R
#'
#' Reads the Read V2 lookup and relationship tables from a local copy of the
#' NHS Read Browser release (TRUD item 8), which ships the canonical Read V2
#' terminology files in FoxPro DBF format under `Standard/V2/`.
#'
#' The lookup is built by joining `DESC.DBF` (codes, term ids, `TERMTYPE`,
#' `CCSTATUS`) with `Term.dbf` (the 30- and 60-character term forms) and
#' left-joining `TERM198.DBF` (the longer term forms, where present). A
#' composed `term` column prefers `TERM198` when available and falls back to
#' `TERM60`.
#'
#' All rows from the source `DESC.DBF` table are retained, including codes
#' with `TERMTYPE = "S"` (synonyms) and `CCSTATUS != "C"` (non-active codes).
#' Query-time filtering to active codes and preferred terms is handled by the
#' `preferred_description_col` and `col_filters` entries in the lookup
#' metadata.
#'
#' The Read 2 ↔ CTV3 mapping tables (`rctctv3map_uk`, `ctv3rctmap_uk`) live in
#' the NHS Data Migration release (TRUD item 9) and are available via
#' [read_nhs_data_migration()].
#'
#' @param path Path to the NHS Read Browser release. Can be:
#'   * A **zip file** (e.g., from [get_read2_trud()])
#'   * An **unzipped directory** containing the `Standard/V2` subdirectory
#' @param tables Character vector of table names to read. Available tables:
#'   * `"read2_lkp"` — Read V2 lookup (codes, descriptions, term type, status)
#'   * `"read2_relationship"` — parent-child hierarchy derived from code
#'     structure (requires `"read2_lkp"`)
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
#' @seealso [add_read2_trud()], [get_read2_trud()], [read_nhs_data_migration()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_read2_trud()
#' result <- read_read2_trud(path)
#' result$read2_lkp$lookup$table
#' result$read2_relationship$relationship$table
#' }
read_read2_trud <- function(
  path,
  tables = c("read2_lkp", "read2_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/9/items/8/releases"
) {
  rlang::arg_match(
    tables,
    values = c("read2_lkp", "read2_relationship"),
    multiple = TRUE
  )

  # read2_relationship requires read2_lkp to derive hierarchy from codes
  if ("read2_relationship" %in% tables && !"read2_lkp" %in% tables) {
    tables <- c("read2_lkp", tables)
  }

  rlang::check_installed(
    "foreign",
    reason = "to read the NHS Read Browser DBF files."
  )

  if (!file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("Path does not exist: {.path {path}}")
  }

  # Handle zip input — extract on-demand. The Read Browser archive expands to
  # several sibling top-level dirs (Scottish/, Standard/, Document/, ...), so
  # we use the extract directory itself as the working path.
  if (
    file.exists(path) &&
      !dir.exists(path) &&
      grepl("\\.zip$", path, ignore.case = TRUE)
  ) {
    if (is.null(version)) {
      version <- basename(path)
    }

    cli::cli_inform("Extracting NHS Read Browser from zip file...")
    extract_dir <- file.path(tempdir(), "codeminer_read2_extract")
    if (!dir.exists(extract_dir)) {
      dir.create(extract_dir, recursive = TRUE)
    }
    utils::unzip(path, exdir = extract_dir, overwrite = TRUE)

    path <- extract_dir
  }

  if (is.null(version)) {
    version <- basename(path)
  }

  v2_dir <- file.path(path, "Standard", "V2")
  if (!dir.exists(v2_dir)) {
    cli::cli_abort(c(
      "x" = "Expected subdirectory {.file Standard/V2} not found under {.path {path}}",
      "i" = "Check that the path points to a valid NHS Read Browser release directory."
    ))
  }

  result <- list()

  if ("read2_lkp" %in% tables) {
    cli::cli_inform(
      "Loading Read V2 lookup (DESC.DBF + Term.dbf + TERM198.DBF)..."
    )

    # NHS Read Browser ships these with uppercase `.DBF` extensions, but
    # foreign::write.dbf() produces lowercase `.dbf` — so look up by
    # case-insensitive pattern to support both real data and test fixtures.
    find_dbf <- function(name) {
      pattern <- paste0("^", name, "$")
      files <- list.files(
        v2_dir,
        pattern = pattern,
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(files) == 0) {
        cli::cli_abort(c(
          "x" = "Required file not found: {.file {name}}",
          "i" = "Expected at: {.file {v2_dir}}"
        ))
      }
      files[[1]]
    }

    desc <- foreign::read.dbf(find_dbf("DESC.DBF"), as.is = TRUE)
    term <- foreign::read.dbf(find_dbf("Term.dbf"), as.is = TRUE)
    term198 <- foreign::read.dbf(find_dbf("TERM198.DBF"), as.is = TRUE)

    # Term.dbf has all term ids with TERM30/TERM60 (full coverage of DESC).
    # TERM198.DBF only contains the subset of terms longer than 60 chars,
    # so it is brought in via left join. The composed `term` column prefers
    # TERM198 when present and falls back to TERM60.
    read2_lkp_table <- desc |>
      dplyr::inner_join(
        term[, c("TERMID", "TERM30", "TERM60")],
        by = "TERMID"
      ) |>
      dplyr::left_join(term198, by = "TERMID") |>
      dplyr::mutate(
        term = dplyr::coalesce(.data$TERM198, .data$TERM60)
      ) |>
      data.table::as.data.table()

    ccstatus_values <- sort(unique(read2_lkp_table$CCSTATUS))

    read2_lkp_metadata <- lookup_metadata(
      code_type = "Read v2",
      lookup_version = version,
      lookup_code_col = "CC",
      lookup_description_col = "term",
      lookup_source = source,
      preferred_description_col = "TERMTYPE",
      preferred_description_indicator = "P",
      col_filters = list(
        CCSTATUS = list(
          values = ccstatus_values,
          defaults = "C"
        )
      )
    )

    result$read2_lkp <- list(
      lookup = list(
        table = read2_lkp_table,
        metadata = read2_lkp_metadata
      )
    )
  }

  if ("read2_relationship" %in% tables) {
    cli::cli_inform(
      "Deriving Read 2 parent-child hierarchy from code structure..."
    )

    read2_relationship <- read2_lkp_table$CC |>
      unique() |>
      stringr::str_replace("\\.+$", "") |>
      build_prefix_hierarchy_len()

    if (nrow(read2_relationship) > 0) {
      read2_relationship <- read2_relationship |>
        dplyr::mutate(dplyr::across(
          dplyr::all_of(c("from", "to")),
          \(x) stringr::str_pad(x, 5, pad = ".", side = "right")
        ))
    }

    result$read2_relationship <- list(
      relationship = list(
        table = read2_relationship,
        metadata = relationship_metadata(
          code_type = "Read v2",
          relationship_version = version,
          from_col = "from",
          to_col = "to",
          type_col = "type",
          child_parent_relationship_code = "is a",
          relationship_source = source
        )
      )
    )
  }

  result
}
