#' Read Read 3 (CTV3) coding files into R
#'
#' Reads the Read Codes Version 3 (CTV3) lookup and relationship tables from a
#' local copy of the TRUD release files (item 19).
#'
#' All rows from the source concept / description / term files are retained
#' (including retired codes, synonyms, and non-clinical term types). Query-time
#' filtering to active concepts and clinical terms is handled by the
#' `col_filters` entry in the lookup metadata; preferred-vs-synonym is handled
#' via `preferred_description_col`.
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

  # `read3_lkp` needs the hierarchy to derive each code's chapter category,
  # so always read V3hier.v3 when either table is requested.
  hier <- data.table::fread(
    file.path(v3_dir, "V3hier.v3"),
    sep = "|",
    header = FALSE,
    col.names = c("child_code", "parent_code", "relationship_type"),
    colClasses = "character"
  )

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

    read3_lkp_table <- concept |>
      dplyr::inner_join(descrip, by = "code") |>
      dplyr::inner_join(terms, by = "description_id") |>
      data.table::as.data.table()

    # Per-code category derived by walking V3hier to the chapter-level
    # ancestor (children of the single root `.....`).
    read3_lkp_table <- read3_attach_category(read3_lkp_table, hier)

    status_values <- sort(unique(read3_lkp_table$status))
    term_type_values <- sort(unique(read3_lkp_table$term_type))

    read3_lkp_metadata <- lookup_metadata(
      code_type = "Read v3",
      lookup_version = version,
      lookup_code_col = "code",
      lookup_description_col = "term",
      lookup_category_col = "category",
      lookup_source = source,
      preferred_description_col = "desc_type",
      preferred_description_indicator = "P",
      col_filters = list(
        # NHS Concept.v3 status codes: C = Current (active); O = Optional
        # (clinically valid but non-preferred subtype/synonym); E = Extinct
        # (withdrawn); R = Redundant. The default keeps both Current and
        # Optional so a CHILDREN() walk surfaces subtypes / complications
        # the way browsers like opencodelists do; Extinct/Redundant stay
        # filtered.
        status = list(
          values = status_values,
          defaults = intersect(c("C", "O"), status_values)
        ),
        term_type = list(
          values = term_type_values,
          defaults = "C"
        )
      )
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

    # CTV3 V3hier.v3's `relationship_type` column is a per-pair sequence
    # number (not a semantic label like "is a"). Every row is a valid
    # parent-child edge, so set `child_parent_relationship_code = NA` to tell
    # `graph_closure()` to skip the type filter entirely.
    read3_relationship_metadata <- relationship_metadata(
      code_type = "Read v3",
      relationship_version = version,
      from_col = "child_code",
      to_col = "parent_code",
      type_col = "relationship_type",
      child_parent_relationship_code = NA_character_,
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

#' Attach a per-code `category` derived from the V3 chapter ancestor
#'
#' Read 3 / CTV3 has a single root `.....` ("Read thesaurus") whose
#' children are the 18 top-level chapters (e.g. "Clinical findings",
#' "Occupations"). For each code we walk `V3hier.v3` upward, looking for
#' the first chapter-level ancestor reached. Codes with multiple paths to
#' different chapters take the alphabetically-first chapter description.
#' Top-level codes (root, chapters themselves) self-join to their own
#' preferred + current description.
#'
#' @param read3_lkp_table The merged concept/descrip/terms lookup table.
#' @param hier_table The V3hier.v3 parent-child table.
#' @return The same lookup table with a new `category` column.
#' @noRd
read3_attach_category <- function(read3_lkp_table, hier_table) {
  # V3hier.v3 lists one row per (child, parent) pair with `relationship_type`
  # as a per-pair sequence number (NOT a semantic label like "is a"). Every
  # row is a valid hierarchical edge for the chapter walk, so dedup on
  # (child, parent) rather than filtering by relationship_type.
  hier_edges <- hier_table |>
    dplyr::distinct(.data$child_code, .data$parent_code)

  # Root: code(s) that appear as a parent but never as a child.
  root_codes <- setdiff(
    unique(hier_edges$parent_code),
    unique(hier_edges$child_code)
  )
  # Chapters: children of root.
  chapter_codes <- unique(
    hier_edges$child_code[hier_edges$parent_code %in% root_codes]
  )

  # Preferred + current description per chapter / root code.
  category_descs <- read3_lkp_table |>
    dplyr::filter(
      .data$code %in% c(.env$chapter_codes, .env$root_codes),
      .data$desc_type == "P",
      .data$status == "C"
    ) |>
    dplyr::distinct(.data$code, .keep_all = TRUE) |>
    dplyr::select(category_code = "code", category_desc = "term")

  # Build the chapter descendant set by iteratively expanding downward
  # from each chapter until no new (chapter, descendant) pairs are added.
  descendants <- tibble::tibble(
    category_code = chapter_codes,
    descendant = chapter_codes
  )
  frontier <- descendants
  repeat {
    next_step <- frontier |>
      dplyr::inner_join(
        hier_edges,
        by = c("descendant" = "parent_code"),
        relationship = "many-to-many"
      ) |>
      dplyr::transmute(
        category_code = .data$category_code,
        descendant = .data$child_code
      ) |>
      dplyr::anti_join(descendants, by = c("category_code", "descendant"))

    if (nrow(next_step) == 0L) {
      break
    }
    descendants <- dplyr::bind_rows(descendants, next_step)
    frontier <- next_step
  }

  # Per-code category: pick the alphabetically-first chapter description
  # when a code is reachable from multiple chapters.
  code_to_category <- descendants |>
    dplyr::inner_join(category_descs, by = "category_code") |>
    dplyr::arrange(.data$descendant, .data$category_desc) |>
    dplyr::distinct(.data$descendant, .keep_all = TRUE) |>
    dplyr::select(code = "descendant", category = "category_desc")

  # The root code itself isn't a descendant of any chapter, so cover it
  # separately with its own description (matches the "top-level keeps own
  # description" convention).
  root_to_category <- category_descs |>
    dplyr::filter(.data$category_code %in% .env$root_codes) |>
    dplyr::select(code = "category_code", category = "category_desc")

  code_to_category <- dplyr::bind_rows(code_to_category, root_to_category)

  read3_lkp_table |>
    dplyr::left_join(code_to_category, by = "code") |>
    data.table::as.data.table()
}
