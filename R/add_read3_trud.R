#' Add Read 3 (CTV3) tables to CodeMiner database
#'
#' Reads Read Codes Version 3 (CTV3) files and adds lookup and relationship
#' tables to the active CodeMiner database. This is a convenience wrapper
#' around [read_read3_trud()] that automatically calls [add_lookup_table()]
#' and [add_relationship_table()].
#'
#' @param path Path to the Read 3 release (zip file or unzipped directory).
#'   Default uses [get_read3_trud()] to download the latest release.
#' @param tables Character vector of table names to add. See
#'   [read_read3_trud()] for available tables. By default, adds both tables.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_read3_trud()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_read3_trud()], [get_read3_trud()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_read3_trud()
#' add_read3_trud(tables = "read3_lkp")
#' }
add_read3_trud <- function(
  path = get_read3_trud(),
  tables = c("read3_lkp", "read3_relationship"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  expected_names <- c(
    read3_lkp = paste("read3", version, sep = "_"),
    read3_relationship = paste("read3", "relationship", version, sep = "_")
  )
  expected_types <- c(
    read3_lkp = "lookup",
    read3_relationship = "relationship"
  )

  sel <- names(expected_names) %in% tables
  if (tables_all_exist(expected_names[sel], expected_types[sel])) {
    codeminer_inform(
      "All requested Read 3 tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  read3_data <- read_read3_trud(
    path = path,
    tables = tables,
    version = version,
    source = source
  )

  add_tables_to_database(read3_data)

  invisible(read3_data)
}
