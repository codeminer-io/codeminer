#' Add Read 2 tables to CodeMiner database
#'
#' Reads Read V2 lookup and cross-mapping files from the NHS Data Migration
#' release and adds them to the active CodeMiner database. This is a
#' convenience wrapper around [read_read2_trud()] that automatically calls
#' [add_lookup_table()] and [add_mapping_table()].
#'
#' @param path Path to the NHS Data Migration release (zip file or unzipped
#'   directory). Default uses [get_read2_trud()] to download the latest
#'   release.
#' @param tables Character vector of table names to add. See
#'   [read_read2_trud()] for available tables. By default, adds all three
#'   tables.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_read2_trud()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_read2_trud()], [get_read2_trud()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_read2_trud()
#' add_read2_trud(tables = "read2_lkp")
#' }
add_read2_trud <- function(
  path = get_read2_trud(),
  tables = c("read2_lkp", "read2_ctv3", "ctv3_read2"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
) {
  read2_data <- read_read2_trud(
    path = path,
    tables = tables,
    version = version,
    source = source
  )

  add_tables_to_database(read2_data)

  invisible(read2_data)
}
