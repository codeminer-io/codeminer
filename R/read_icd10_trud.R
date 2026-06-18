#' Read ICD-10 coding files into R
#'
#' Reads the ICD-10 Edition 5 codes and descriptions from a local copy of the
#' TRUD release files.
#'
#' @param path Path to the ICD-10 release. Can be:
#'   * A **zip file** (e.g., from [get_icd10_trud()])
#'   * An **unzipped directory** containing the `Content` subdirectory
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with elements:
#'   * `icd10_lkp`: lookup table with `table` (data.table) and `metadata`
#'   * `icd10_relationship`: parent-child hierarchy derived from code structure
#'
#'   The lookup `table` has columns including `CODE`, `ALT_CODE` (dot-stripped,
#'   used as the primary code column), `DESCRIPTION`, and others from the source
#'   file. `DESCRIPTION` is augmented with `MODIFIER_4` or `MODIFIER_5` where
#'   present. The trailing `X` placeholder used to pad 3-char categories to a
#'   4-char width is stripped from `ALT_CODE` (e.g., `J46X` becomes `J46`); the
#'   original `CODE` column is left unchanged.
#'
#' @seealso [add_icd10_trud()], [get_icd10_trud()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_icd10_trud()
#' result <- read_icd10_trud(path)
#' result$icd10_lkp$lookup$table
#' }
read_icd10_trud <- function(
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

    cli::cli_inform("Extracting ICD-10 from zip file...")
    extract_dir <- file.path(tempdir(), "codeminer_icd10_extract")
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

  cli::cli_inform("Discovering ICD-10 codes file...")

  icd10_files <- list.files(
    path,
    pattern = "CodesAndTitlesAndMetadata",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(icd10_files) == 0) {
    cli::cli_abort(c(
      "x" = "Could not find ICD-10 codes file",
      "i" = "Expected a file matching 'CodesAndTitlesAndMetadata' under {.path {path}}"
    ))
  }

  if (length(icd10_files) > 1) {
    cli::cli_inform(c(
      "!" = "Multiple ICD-10 files found; using the first:",
      "i" = "{.file {basename(icd10_files[[1]])}}"
    ))
  }

  cli::cli_inform("Loading ICD-10 codes...")

  icd10_table <- data.table::fread(
    icd10_files[[1]],
    sep = "\t",
    colClasses = "character"
  )

  # Strip trailing `X` placeholder from 3-char categories (e.g. J46X -> J46) so
  # they are not orphaned in the prefix-derived relationship table.
  icd10_table <- icd10_table |>
    dplyr::mutate(ALT_CODE = strip_icd10_x_placeholder(.data$ALT_CODE))

  if (anyDuplicated(icd10_table$ALT_CODE)) {
    dups <- icd10_table$ALT_CODE[duplicated(icd10_table$ALT_CODE)]
    cli::cli_abort(c(
      "x" = "Duplicate {.field ALT_CODE} values after stripping the X placeholder",
      "i" = "Affected codes: {.val {unique(dups)}}"
    ))
  }

  # Augment DESCRIPTION with modifier columns where present
  icd10_table <- icd10_table |>
    dplyr::mutate(
      DESCRIPTION = dplyr::case_when(
        !is.na(.data$MODIFIER_4) & .data$MODIFIER_4 != "" ~
          paste(.data$DESCRIPTION, .data$MODIFIER_4),
        !is.na(.data$MODIFIER_5) & .data$MODIFIER_5 != "" ~
          paste(.data$DESCRIPTION, .data$MODIFIER_5),
        TRUE ~ .data$DESCRIPTION
      )
    )

  # Attach chapter as `category` by joining the static prefix -> chapter
  # lookup on the 3-character prefix of ALT_CODE. TRUD's TREE_DESCRIPTION is
  # a sparse leaf-level display label, not a chapter.
  icd10_table <- icd10_table |>
    dplyr::mutate(.prefix = stringr::str_sub(.data$ALT_CODE, 1, 3)) |>
    dplyr::left_join(
      icd10_chapter_lookup() |> dplyr::rename(category = "chapter"),
      by = c(".prefix" = "prefix")
    ) |>
    dplyr::select(-".prefix")

  icd10_metadata <- lookup_metadata(
    code_type = "ICD-10",
    lookup_version = version,
    lookup_code_col = "ALT_CODE",
    lookup_description_col = "DESCRIPTION",
    lookup_category_col = "category",
    lookup_source = source
  )

  # Derive parent-child hierarchy from code structure
  icd10_relationship <- build_prefix_hierarchy_len(icd10_table$ALT_CODE)

  list(
    icd10_lkp = list(
      lookup = list(
        table = icd10_table,
        metadata = icd10_metadata
      )
    ),
    icd10_relationship = list(
      relationship = list(
        table = icd10_relationship,
        metadata = relationship_metadata(
          code_type = "ICD-10",
          relationship_version = version,
          from_col = "from",
          to_col = "to",
          relationship_source = source
        )
      )
    )
  )
}
