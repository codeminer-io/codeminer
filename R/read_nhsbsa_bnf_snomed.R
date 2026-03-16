#' Read the NHSBSA BNF-SNOMED mapping file into a mapping table
#'
#' Reads the NHS Business Services Authority (NHSBSA) BNF-SNOMED/DM+D mapping
#' zip file and returns a structured mapping table for use with CodeMiner.
#'
#' @param path Path to the NHSBSA BNF-SNOMED mapping zip file. Default uses
#'   [get_nhsbsa_bnf_snomed()] to download the file.
#' @param version Character string for the version label (default: `"v0"`).
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with element `bnf_dmd`, containing:
#'   * `mapping`: a list with `table` (data frame) and `metadata` (list)
#'
#' @seealso [add_nhsbsa_bnf_snomed()], [get_nhsbsa_bnf_snomed()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_nhsbsa_bnf_snomed()
#' result <- read_nhsbsa_bnf_snomed(path)
#' result$bnf_dmd$mapping$table
#' }
read_nhsbsa_bnf_snomed <- function(
  path = get_nhsbsa_bnf_snomed(),
  version = "v0",
  source = "https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping"
) {
  # Extract into a temp directory
  extract_dir <- file.path(tempdir(), "codeminer_nhsbsa_extract")
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
  }

  utils::unzip(path, exdir = extract_dir, overwrite = TRUE)

  extracted_files <- list.files(extract_dir, full.names = TRUE)
  xlsx_files <- extracted_files[grepl(
    "\\.xlsx?$",
    extracted_files,
    ignore.case = TRUE
  )]

  if (length(xlsx_files) == 0) {
    cli::cli_abort(c(
      "x" = "No Excel file found in the NHSBSA BNF-SNOMED zip archive.",
      "i" = "Expected a single .xlsx file."
    ))
  }

  if (length(xlsx_files) > 1) {
    cli::cli_abort(c(
      "x" = "Multiple Excel files found in the NHSBSA BNF-SNOMED zip archive.",
      "i" = "Found: {.file {basename(xlsx_files)}}"
    ))
  }

  bnf_dmd <- readxl::read_excel(xlsx_files[[1]])

  # Normalise column names: lower case, remove special characters, strip "plus_"
  col_names <- names(bnf_dmd)
  col_names <- tolower(col_names)
  col_names <- stringr::str_replace_all(col_names, "[^a-z0-9_]", "_")
  col_names <- stringr::str_replace_all(col_names, "_+", "_")
  col_names <- stringr::str_remove_all(col_names, "^_|_$")
  col_names <- stringr::str_remove_all(col_names, "plus_")
  names(bnf_dmd) <- col_names

  meta <- mapping_metadata(
    from_code_type = "bnf",
    to_code_type = "dmd",
    map_version = version,
    map_source = source
  )

  list(
    bnf_dmd = list(
      mapping = list(
        table = bnf_dmd,
        metadata = meta
      )
    )
  )
}
