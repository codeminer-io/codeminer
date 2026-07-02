#' Add NHS BSA BNF code information (pseudo-BNF) to CodeMiner database
#'
#' Reads the NHS Business Services Authority (NHSBSA) "BNF Code Information" CSV
#' and adds the BNF lookup and relationship tables to the active CodeMiner
#' database. This is a convenience wrapper around [read_pseudobnf()] that
#' automatically calls [add_lookup_table()] and [add_relationship_table()].
#'
#' @param path Path to the NHS BSA BNF code information CSV. Default uses
#'   [get_pseudobnf()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the file name.
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_pseudobnf()] (a named list of
#'   tables with metadata), or `NULL` if all tables already exist.
#'
#' @seealso [read_pseudobnf()], [get_pseudobnf()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_pseudobnf()
#' }
add_pseudobnf <- function(
  path = get_pseudobnf(),
  version = NULL,
  source = "https://opendata.nhsbsa.net/dataset/bnf-code-information-current-year"
) {
  if (is.null(version)) {
    # Full resource file name (incl. extension), matching the NHS BSA portal.
    version <- basename(path)
  }

  expected_names <- c(
    bnf_lkp = paste("BNF", version, sep = "_"),
    bnf_relationship = paste("BNF", "relationship", version, sep = "_")
  )
  expected_types <- c(
    bnf_lkp = "lookup",
    bnf_relationship = "relationship"
  )

  if (tables_all_exist(expected_names, expected_types)) {
    codeminer_inform(
      "All BNF tables already exist for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  bnf_data <- read_pseudobnf(
    path = path,
    version = version,
    source = source
  )

  add_tables_to_database(bnf_data)

  invisible(bnf_data)
}
