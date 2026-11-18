#' Add CPRD Aurum code browser tables to CodeMiner database
#'
#' Reads a CPRD Aurum code browser release and adds the medcode and prodcode
#' lookup tables, and their mappings to SNOMED CT and Read v2, to the active
#' CodeMiner database. This is a convenience wrapper around
#' [read_cprd_aurum()] that calls [add_lookup_table()] and
#' [add_mapping_table()]. Tables that already exist for the version are
#' skipped.
#'
#' CPRD supplies the code browser to researchers with access to its data, so
#' `path` is required. Check your CPRD data access agreement before sharing a
#' database that contains these tables.
#'
#' @inheritParams read_cprd_aurum
#'
#' @returns Invisibly returns the result from [read_cprd_aurum()] for the
#'   tables that were added (a named list of tables with metadata), or `NULL`
#'   if they all existed already.
#'
#' @seealso [read_cprd_aurum()]
#' @export
#' @examples
#' \dontrun{
#' build_database()
#' add_cprd_aurum("path/to/CPRD_CodeBrowser_YYYYMM_Aurum")
#' }
add_cprd_aurum <- function(
  path,
  tables = cprd_aurum_tables(),
  version = NULL,
  source = "CPRD Aurum code browser"
) {
  tables <- rlang::arg_match(tables, cprd_aurum_tables(), multiple = TRUE)

  if (!dir.exists(path)) {
    codeminer_abort("Path does not exist or is not a directory: {.path {path}}")
  }

  if (is.null(version)) {
    version <- cprd_aurum_version(path)
  }

  medcode <- "CPRD Aurum medcode"
  prodcode <- "CPRD Aurum prodcode"
  expected_names <- c(
    cprd_aurum_medcode_lkp = paste(medcode, version, sep = "_"),
    cprd_aurum_prodcode_lkp = paste(prodcode, version, sep = "_"),
    cprd_aurum_medcode_snomed = paste(medcode, "SNOMED CT", version, sep = "_"),
    cprd_aurum_medcode_read2 = paste(medcode, "Read v2", version, sep = "_"),
    cprd_aurum_prodcode_snomed = paste(
      prodcode,
      "SNOMED CT",
      version,
      sep = "_"
    )
  )[tables]
  expected_types <- c(
    cprd_aurum_medcode_lkp = "lookup",
    cprd_aurum_prodcode_lkp = "lookup",
    cprd_aurum_medcode_snomed = "mapping",
    cprd_aurum_medcode_read2 = "mapping",
    cprd_aurum_prodcode_snomed = "mapping"
  )[tables]

  missing <- !purrr::map2_lgl(expected_names, expected_types, \(n, t) {
    tables_all_exist(n, t)
  })

  if (!any(missing)) {
    codeminer_inform(
      "All requested CPRD Aurum tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  cprd_data <- read_cprd_aurum(
    path = path,
    tables = names(expected_names)[missing],
    version = version,
    source = source
  )

  add_tables_to_database(cprd_data)

  invisible(cprd_data)
}
