#' Add NHS Data Migration mapping tables to CodeMiner database
#'
#' Reads the clinically assured Read V2 and CTV3 to SNOMED CT mapping tables
#' from the NHS Data Migration release and adds them to the active CodeMiner
#' database. This is a convenience wrapper around [read_nhs_data_migration()]
#' that automatically calls [add_mapping_table()] for each table.
#'
#' @param path Path to the NHS Data Migration release (zip file or unzipped
#'   directory). Default uses [get_nhs_data_migration()] to download the file.
#' @param tables Character vector of table names to add. See
#'   [read_nhs_data_migration()] for available tables. By default, adds both
#'   tables.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the zip file or directory name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_nhs_data_migration()] (a
#'   named list of tables with metadata).
#'
#' @seealso [read_nhs_data_migration()], [get_nhs_data_migration()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_nhs_data_migration()
#' add_nhs_data_migration(tables = "ctv3sctmap2")
#' }
add_nhs_data_migration <- function(
  path = get_nhs_data_migration(),
  tables = c("ctv3sctmap2", "rcsctmap2"),
  version = NULL,
  source = "https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases"
) {
  if (is.null(version)) {
    version <- basename(path)
  }

  expected_names <- c(
    ctv3sctmap2 = paste("read3", "sct", version, sep = "_"),
    rcsctmap2 = paste("read2", "sct", version, sep = "_")
  )
  expected_types <- c(
    ctv3sctmap2 = "mapping",
    rcsctmap2 = "mapping"
  )

  sel <- names(expected_names) %in% tables
  if (tables_all_exist(expected_names[sel], expected_types[sel])) {
    codeminer_inform(
      "All requested NHS Data Migration tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  migration_data <- read_nhs_data_migration(
    path = path,
    tables = tables,
    version = version,
    source = source
  )

  add_tables_to_database(migration_data)

  invisible(migration_data)
}
