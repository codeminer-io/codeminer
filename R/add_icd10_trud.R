#' Add ICD-10 lookup table to CodeMiner database
#'
#' Reads ICD-10 Edition 5 files and adds the lookup and relationship tables to
#' the active CodeMiner database. This is a convenience wrapper around
#' [read_icd10_trud()] that automatically calls [add_lookup_table()] and
#' [add_relationship_table()].
#'
#' @param path Path to the ICD-10 release (zip file or unzipped directory).
#'   Default uses [get_icd10_trud()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_icd10_trud()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_icd10_trud()], [get_icd10_trud()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_icd10_trud()
#' }
add_icd10_trud <- function(
  path = get_icd10_trud(),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  if (tables_all_exist(paste("ICD-10", version, sep = "_"), "lookup")) {
    codeminer_inform(
      "ICD-10 lookup table already exists for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  icd10_data <- read_icd10_trud(
    path = path,
    version = version,
    source = source
  )

  add_tables_to_database(icd10_data)

  invisible(icd10_data)
}
