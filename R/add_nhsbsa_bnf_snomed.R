#' Add NHSBSA BNF-SNOMED mapping table to CodeMiner database
#'
#' Reads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D mapping
#' file and adds it to the active CodeMiner database. This is a convenience
#' wrapper around [read_nhsbsa_bnf_snomed()] that automatically calls
#' [add_mapping_table()].
#'
#' @param path Path to the NHSBSA BNF-SNOMED mapping zip file. Default uses
#'   [get_nhsbsa_bnf_snomed()] to download the file.
#' @param version Character string for the version label (default: `"v0"`).
#' @param source Character string for the data source URL or description.
#'
#' @returns Invisibly returns the result from [read_nhsbsa_bnf_snomed()] (a
#'   named list of tables with metadata).
#'
#' @seealso [read_nhsbsa_bnf_snomed()], [get_nhsbsa_bnf_snomed()]
#' @export
#' @examples
#' \dontrun{
#' build_database(db_path = "my_codes.db")
#' add_nhsbsa_bnf_snomed()
#' }
add_nhsbsa_bnf_snomed <- function(
  path = get_nhsbsa_bnf_snomed(),
  version = "v0",
  source = "https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping"
) {
  if (tables_all_exist(paste("bnf", "dmd", version, sep = "_"), "mapping")) {
    codeminer_inform(
      "NHSBSA BNF-SNOMED mapping table already exists for version {.val {version}}, skipping."
    )
    return(invisible(NULL))
  }

  nhsbsa_data <- read_nhsbsa_bnf_snomed(
    path = path,
    version = version,
    source = source
  )

  add_tables_to_database(nhsbsa_data)

  invisible(nhsbsa_data)
}
