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

  expected_names <- c(
    opcs4_lkp = paste("OPCS4", version, sep = "_"),
    opcs4_relationship = paste("OPCS4", "relationship", version, sep = "_")
  )
  expected_types <- c(
    opcs4_lkp = "lookup",
    opcs4_relationship = "relationship"
  )

  if (tables_all_exist(expected_names, expected_types)) {
    codeminer_inform(
      "All OPCS-4 tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  opcs4_data <- read_opcs4_trud(
    path = path,
    version = version,
    source = source
  )

  # Only add tables that don't already exist
  missing <- !purrr::map2_lgl(expected_names, expected_types, \(n, t) {
    tables_all_exist(n, t)
  })
  opcs4_data <- opcs4_data[names(expected_names)[missing]]

  add_tables_to_database(opcs4_data)

  invisible(opcs4_data)
}
