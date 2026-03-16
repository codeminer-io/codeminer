#' Add Phecode 1.2 tables to CodeMiner database
#'
#' Reads Phecode 1.2 definitions and/or ICD mapping files and adds them to the
#' active CodeMiner database. This is a convenience wrapper around
#' [read_phecode()] that automatically calls [add_lookup_table()] and
#' [add_mapping_table()] for each table.
#'
#' @param lkp_path Path to the Phecode 1.2 definitions CSV. If `NULL`, skipped.
#' @param icd10_map_path Path to the Phecode 1.2 to ICD-10 mapping CSV. If
#'   `NULL`, skipped.
#' @param icd9_map_path Path to the Phecode 1.2 to ICD-9 mapping `.rda` file.
#'   If `NULL`, skipped.
#' @param version Character string for the version label (default: `"v1.2"`).
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_phecode()] (a named list of
#'   tables with metadata).
#'
#' @seealso [read_phecode()], [get_phecode()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#'
#' # Download and add all phecode tables
#' paths <- get_phecode()
#' add_phecode(
#'   lkp_path = paths$lkp,
#'   icd10_map_path = paths$icd10_map,
#'   icd9_map_path = paths$icd9_map
#' )
#' }
#'
#' # Example with dummy data
#' \dontrun{
#' build_database(db_path = tempfile(fileext = ".db"))
#' add_phecode(
#'   lkp_path = dummy_phecode_lkp_path(),
#'   icd10_map_path = dummy_icd10_phecode_map_path()
#' )
#' }
add_phecode <- function(
  lkp_path = NULL,
  icd10_map_path = NULL,
  icd9_map_path = NULL,
  version = "v1.2",
  source = "https://phewascatalog.org/phecodes"
) {
  phecode_data <- read_phecode(
    lkp_path = lkp_path,
    icd10_map_path = icd10_map_path,
    icd9_map_path = icd9_map_path,
    version = version,
    source = source
  )

  add_tables_to_database(phecode_data)

  invisible(phecode_data)
}
