#' Add ICD-10-CM to CodeMiner database
#'
#' Reads an ICD-10-CM release and adds the lookup and relationship tables to the
#' active CodeMiner database. This is a convenience wrapper around
#' [read_icd10cm()] that automatically calls [add_lookup_table()] and
#' [add_relationship_table()].
#'
#' @param path Path to an ICD-10-CM release (`.zip` or order `.txt` file).
#'   Default uses [get_icd10cm()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   the 8-digit effective date is taken from the file name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_icd10cm()] (a named list of
#'   tables with metadata), or `NULL` if all tables already exist.
#'
#' @section Attribution:
#' ICD-10-CM is published by NCHS/CDC and is available free of charge at
#' <https://www.cdc.gov/nchs/icd/icd-10-cm/files.html>. Use of these files does
#' not imply endorsement by CDC, HHS or the US Government.
#'
#' @seealso [read_icd10cm()], [get_icd10cm()]
#' @export
#' @examples
#' \dontrun{
#' build_database()
#' add_icd10cm()
#'
#' # A specific release
#' add_icd10cm(get_icd10cm(release = "20250401"))
#' }
add_icd10cm <- function(
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

  expected_names <- c(
    icd10cm_lkp = paste("ICD-10-CM", version, sep = "_"),
    icd10cm_relationship = paste(
      "ICD-10-CM",
      "relationship",
      version,
      sep = "_"
    )
  )
  expected_types <- c(
    icd10cm_lkp = "lookup",
    icd10cm_relationship = "relationship"
  )

  if (tables_all_exist(expected_names, expected_types)) {
    codeminer_inform(
      "All ICD-10-CM tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  icd10cm_data <- read_icd10cm(
    path = path,
    version = version,
    source = source
  )

  # Only add tables that don't already exist
  missing <- !purrr::map2_lgl(expected_names, expected_types, \(n, t) {
    tables_all_exist(n, t)
  })
  icd10cm_data <- icd10cm_data[names(expected_names)[missing]]

  add_tables_to_database(icd10cm_data)

  invisible(icd10cm_data)
}
