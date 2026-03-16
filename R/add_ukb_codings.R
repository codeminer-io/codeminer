#' Add UK Biobank codings tables to CodeMiner database
#'
#' Reads the UK Biobank `Codings.tsv` file and adds lookup tables for OPCS-4
#' and UKB self-reported code types to the active CodeMiner database. This is a
#' convenience wrapper around [read_ukb_codings()] that automatically calls
#' [add_lookup_table()] for each table.
#'
#' @param path Path to the `Codings.tsv` file. Default uses [get_ukb_codings()]
#'   to download the file.
#' @param tables Character vector of table names to add. See
#'   [read_ukb_codings()] for available table names. By default, adds all five
#'   tables.
#' @param version Character string for the version label (default: `"v0"`).
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_ukb_codings()] (a named
#'   list of tables with metadata).
#'
#' @seealso [read_ukb_codings()], [get_ukb_codings()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_ukb_codings()
#' add_ukb_codings(tables = c("opcs4_lkp", "data_coding_4"))
#' }
#'
#' # Example with dummy data
#' \dontrun{
#' build_database(db_path = tempfile(fileext = ".db"))
#' add_ukb_codings(
#'   path = dummy_ukb_codings_path(),
#'   tables = "opcs4_lkp"
#' )
#' }
add_ukb_codings <- function(
  path = get_ukb_codings(),
  tables = c(
    "opcs4_lkp",
    "data_coding_3",
    "data_coding_4",
    "data_coding_5",
    "data_coding_6"
  ),
  version = "v0",
  source = "https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide"
) {
  codings_data <- read_ukb_codings(
    path = path,
    tables = tables,
    version = version,
    source = source
  )

  add_tables_to_database(codings_data)

  invisible(codings_data)
}
