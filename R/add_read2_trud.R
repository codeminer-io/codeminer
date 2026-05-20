#' Add Read 2 tables to CodeMiner database
#'
#' Reads Read V2 lookup and relationship files from the NHS Read Browser
#' release (TRUD item 8) and adds them to the active CodeMiner database. This
#' is a convenience wrapper around [read_read2_trud()] that automatically
#' calls [add_lookup_table()] and [add_relationship_table()].
#'
#' The Read 2 ↔ CTV3 cross-mapping tables (`rctctv3map_uk`, `ctv3rctmap_uk`)
#' live in TRUD item 9 and are added via [add_nhs_data_migration()].
#'
#' @param path Path to the NHS Read Browser release (zip file or unzipped
#'   directory). Default uses [get_read2_trud()] to download the latest
#'   release.
#' @param tables Character vector of table names to add. See
#'   [read_read2_trud()] for available tables. By default, adds both
#'   tables.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_read2_trud()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_read2_trud()], [get_read2_trud()], [add_nhs_data_migration()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_read2_trud()
#' add_read2_trud(tables = "read2_lkp")
#' }
add_read2_trud <- function(
  path = get_read2_trud(),
  tables = c("read2_lkp", "read2_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/authenticated/filters/0/categories/9/items/8/releases"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  expected_names <- c(
    read2_lkp = paste("Read v2", version, sep = "_"),
    read2_relationship = paste("Read v2", "relationship", version, sep = "_")
  )
  expected_types <- c(
    read2_lkp = "lookup",
    read2_relationship = "relationship"
  )

  sel <- names(expected_names) %in% tables
  if (tables_all_exist(expected_names[sel], expected_types[sel])) {
    codeminer_inform(
      "All requested Read 2 tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  read2_data <- read_read2_trud(
    path = path,
    tables = tables,
    version = version,
    source = source
  )

  # Only add tables that don't already exist
  missing <- !purrr::map2_lgl(
    expected_names[sel],
    expected_types[sel],
    \(n, t) tables_all_exist(n, t)
  )
  read2_data <- read2_data[names(expected_names[sel])[missing]]

  add_tables_to_database(read2_data)

  invisible(read2_data)
}
