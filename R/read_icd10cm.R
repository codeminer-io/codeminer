#' Read an ICD-10-CM release into lookup and relationship tables
#'
#' Reads the fixed-width ICD-10-CM "order file" published by the National
#' Center for Health Statistics (NCHS) — one row per code, including the
#' non-billable category and subcategory headers — and returns an ICD-10-CM
#' lookup table plus a parent-child relationship table for use with CodeMiner.
#'
#' @param path Path to an ICD-10-CM release: either the `.zip` published by
#'   NCHS (the order file is located inside it) or the order `.txt` file
#'   itself. Default uses [get_icd10cm()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   the 8-digit effective date is taken from the file name (as written by
#'   [get_icd10cm()]); an error is raised if none can be found.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with elements:
#'   * `icd10cm_lkp`: list with `lookup` -> `list(table, metadata)`
#'   * `icd10cm_relationship`: list with `relationship` -> `list(table, metadata)`
#'
#'   The lookup `table` has columns:
#'   * `icd10cm_code`: the code without its decimal point (e.g. `E1165`); this
#'     is the primary code column.
#'   * `icd10cm_code_dotted`: the code as conventionally written (`E11.65`).
#'   * `billable`: `"1"` for a billable (leaf) code, `"0"` for a category or
#'     subcategory header. Exposed as a column filter; both are included by
#'     default.
#'   * `short_description`, `description`: the short and long descriptions.
#'   * `category`: the ICD-10-CM chapter.
#'
#' @details
#' Codes are stored exactly as published: placeholder `X` characters (e.g.
#' `T07XXXA`) are part of the code and are retained. The hierarchy is derived
#' from code prefixes, so a code whose shorter prefixes are not themselves
#' codes attaches to the nearest prefix that is (`T07XXXA` -> `T07`).
#'
#' @section Attribution:
#' ICD-10-CM is published by NCHS/CDC and is available free of charge at
#' <https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files does
#' not imply endorsement by CDC, HHS or the US Government.
#'
#' @seealso [add_icd10cm()], [get_icd10cm()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_icd10cm()
#' result <- read_icd10cm(path)
#' result$icd10cm_lkp$lookup$table
#' result$icd10cm_relationship$relationship$table
#' }
read_icd10cm <- function(
  path = get_icd10cm(),
  version = NULL,
  source = "https://www.cdc.gov/nchs/icd/icd-10-cm/files.html"
) {
  if (!file.exists(path)) {
    cli::cli_abort("Path does not exist: {.path {path}}")
  }

  if (is.null(version)) {
    version <- icd10cm_version_from_path(path)
  }

  # A zip is unpacked to a scoped temp directory that is removed once the
  # order file has been read.
  extract_dir <- tempfile("codeminer_icd10cm_")
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  order_file <- icd10cm_order_file(path, extract_dir)

  cli::cli_inform(
    "Loading ICD-10-CM codes from {.file {basename(order_file)}}..."
  )

  # Fixed-width layout (1-based, inclusive): 1-5 order number, 7-13 code,
  # 15 billable flag, 17-76 short description, 78+ long description. The order
  # number is not carried over as it is not stable across releases.
  lines <- readLines(order_file, warn = FALSE)
  lines <- lines[lines != ""]

  icd10cm_table <- tibble::tibble(
    icd10cm_code = stringr::str_trim(stringr::str_sub(lines, 7, 13)),
    billable = stringr::str_sub(lines, 15, 15),
    short_description = stringr::str_trim(stringr::str_sub(lines, 17, 76)),
    description = stringr::str_trim(stringr::str_sub(lines, 78, -1))
  )

  check_icd10cm_order_table(icd10cm_table, lines)

  # Attach the chapter as `category` by joining on the 3-character prefix.
  chapter_lookup <- icd10cm_chapter_lookup(
    stringr::str_sub(icd10cm_table$icd10cm_code, 1, 3)
  ) |>
    dplyr::rename(category = "chapter")

  # Chapter ranges are carried in the package, so a category outside every
  # range most likely means a new release has outgrown them. Surface it rather
  # than silently storing NA.
  unmatched <- chapter_lookup$prefix[is.na(chapter_lookup$category)]
  if (length(unmatched) > 0) {
    codeminer_warn(c(
      "!" = "{length(unmatched)} ICD-10-CM categor{?y/ies} did not match any
             chapter range and will have an NA {.field category}:
             {.val {unmatched}}.",
      "i" = "The chapter ranges in {.fun icd10cm_chapters} may need updating
             for this release."
    ))
  }

  icd10cm_table <- icd10cm_table |>
    dplyr::mutate(
      icd10cm_code_dotted = stringr::str_replace(
        .data$icd10cm_code,
        "^(.{3})(.+)$",
        "\\1.\\2"
      ),
      .prefix = stringr::str_sub(.data$icd10cm_code, 1, 3)
    ) |>
    dplyr::left_join(chapter_lookup, by = c(".prefix" = "prefix")) |>
    dplyr::select(
      "icd10cm_code",
      "icd10cm_code_dotted",
      "billable",
      "short_description",
      "description",
      "category"
    )

  icd10cm_metadata <- lookup_metadata(
    code_type = "ICD-10-CM",
    lookup_version = version,
    lookup_code_col = "icd10cm_code",
    lookup_description_col = "description",
    lookup_category_col = "category",
    lookup_source = source,
    col_filters = list(
      # Headers (categories and subcategories) are the nodes that hierarchy
      # traversal passes through, so both values are included by default.
      billable = list(
        values = c("0", "1"),
        defaults = c("0", "1"),
        description = "Whether the code is billable (a leaf code) or a category/subcategory header. Defaults to both.",
        value_labels = c("1" = "Billable", "0" = "Header")
      )
    )
  )

  icd10cm_relationship <- build_prefix_hierarchy_len(icd10cm_table$icd10cm_code)

  list(
    icd10cm_lkp = list(
      lookup = list(
        table = icd10cm_table,
        metadata = icd10cm_metadata
      )
    ),
    icd10cm_relationship = list(
      relationship = list(
        table = icd10cm_relationship,
        metadata = relationship_metadata(
          code_type = "ICD-10-CM",
          relationship_version = version,
          from_col = "from",
          to_col = "to",
          relationship_source = source
        )
      )
    )
  )
}

# Derive the version label from an ICD-10-CM file name: the first run of
# exactly 8 digits (the release's effective date, as written by
# `get_icd10cm()`). Aborts if there is none, since a label that does not sort by
# date would break "latest" version resolution.
icd10cm_version_from_path <- function(path, call = rlang::caller_env()) {
  version <- stringr::str_extract(
    basename(path),
    "(?<![0-9])[0-9]{8}(?![0-9])"
  )
  if (is.na(version)) {
    cli::cli_abort(
      c(
        "x" = "Could not derive an ICD-10-CM version from the file name
               {.file {basename(path)}}.",
        "i" = "Supply {.arg version} as the release's 8-digit effective date,
               e.g. {.val 20261001}."
      ),
      call = call
    )
  }
  version
}

# Resolve `path` to the order `.txt` file. A `.txt` is returned as-is; a `.zip`
# is searched for a single file whose name contains "order" (excluding the
# addenda file published alongside it), which is extracted to `extract_dir`.
icd10cm_order_file <- function(path, extract_dir, call = rlang::caller_env()) {
  if (!grepl("\\.zip$", path, ignore.case = TRUE)) {
    return(path)
  }

  contents <- utils::unzip(path, list = TRUE)$Name
  order_files <- contents[
    grepl("order", basename(contents), ignore.case = TRUE) &
      !grepl("addenda", basename(contents), ignore.case = TRUE) &
      grepl("\\.txt$", contents, ignore.case = TRUE)
  ]

  if (length(order_files) != 1) {
    cli::cli_abort(
      c(
        "x" = "Expected exactly one ICD-10-CM order file in {.file {basename(path)}},
               found {length(order_files)}.",
        "i" = "Zip contents: {.file {contents}}"
      ),
      call = call
    )
  }

  dir.create(extract_dir, recursive = TRUE)
  utils::unzip(
    path,
    files = order_files,
    exdir = extract_dir,
    junkpaths = TRUE
  )
  file.path(extract_dir, basename(order_files))
}

# Abort clearly if the file does not have the expected fixed-width layout
# (every code must look like an ICD-10-CM code and the billable flag must be
# 0/1) or contains duplicate codes.
check_icd10cm_order_table <- function(tbl, lines, call = rlang::caller_env()) {
  bad <- !stringr::str_detect(tbl$icd10cm_code, "^[A-Z][0-9A-Z]{2,6}$") |
    !tbl$billable %in% c("0", "1")
  if (any(bad)) {
    cli::cli_abort(
      c(
        "x" = "The ICD-10-CM order file does not have the expected fixed-width
               layout ({sum(bad)} problem line{?s}).",
        "i" = "First problem line: {.val {lines[bad][[1]]}}"
      ),
      call = call
    )
  }

  if (anyDuplicated(tbl$icd10cm_code)) {
    dups <- unique(tbl$icd10cm_code[duplicated(tbl$icd10cm_code)])
    cli::cli_abort(
      c(
        "x" = "Duplicate ICD-10-CM codes in the order file.",
        "i" = "Affected codes: {.val {dups}}"
      ),
      call = call
    )
  }

  invisible(tbl)
}
