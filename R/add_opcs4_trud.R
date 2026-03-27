#' Add OPCS-4 lookup table to CodeMiner database
#'
#' Reads OPCS-4 files and adds the lookup and relationship tables to the active
#' CodeMiner database. This is a convenience wrapper around [read_opcs4_trud()]
#' that automatically calls [add_lookup_table()] and
#' [add_relationship_table()].
#'
#' @param path Path to the OPCS-4 release (zip file or unzipped directory).
#'   Default uses [get_opcs4_trud()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_opcs4_trud()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_opcs4_trud()], [get_opcs4_trud()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_opcs4_trud()
#' }
add_opcs4_trud <- function(
  path = get_opcs4_trud(),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  if (tables_all_exist(paste("OPCS4", version, sep = "_"), "lookup")) {
    codeminer_inform(
      "OPCS-4 lookup table already exists for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  opcs4_data <- read_opcs4_trud(
    path = path,
    version = version,
    source = source
  )

  add_tables_to_database(opcs4_data)

  invisible(opcs4_data)
}
