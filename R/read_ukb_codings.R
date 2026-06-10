#' Read UK Biobank codings file into lookup tables
#'
#' Reads the UK Biobank codings file (`Codings.tsv`) and returns lookup tables
#' for OPCS-4 and UKB self-reported condition/medication/operation code types.
#'
#' @param path Path to the `Codings.tsv` file. Default uses
#'   [get_ukb_codings()] to download the file.
#' @param tables Character vector of table names to read. Available tables:
#'   * `"opcs4_lkp"` — OPCS-4 procedure lookup
#'   * `"data_coding_3"` — self-reported cancer (UKB field coding 3)
#'   * `"data_coding_4"` — self-reported medications (UKB field coding 4)
#'   * `"data_coding_5"` — self-reported operations (UKB field coding 5)
#'   * `"data_coding_6"` — self-reported non-cancer illnesses (UKB field coding 6)
#'
#'   By default, all five tables are returned.
#' @param version Character string for the version label (default: `"v0"`).
#' @param source Character string for the data source URL or description.
#'
#' @return A named list where each element corresponds to a requested table,
#'   containing:
#'   * `lookup`: a list with `table` (data frame) and `metadata` (list)
#'
#' @examples
#' result <- read_ukb_codings(
#'   path = dummy_ukb_codings_path(),
#'   tables = "opcs4_lkp"
#' )
#' result$opcs4_lkp$lookup$table
#'
#' @seealso [add_ukb_codings()], [get_ukb_codings()]
#' @export
read_ukb_codings <- function(
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
  rlang::arg_match(
    tables,
    values = c(
      "opcs4_lkp",
      "data_coding_3",
      "data_coding_4",
      "data_coding_5",
      "data_coding_6"
    ),
    multiple = TRUE
  )

  ukb_codings <- readr::read_tsv(
    path,
    progress = FALSE,
    col_types = readr::cols(.default = "c")
  )

  # Map of table name -> list(Coding, code_col, code_type)
  table_specs <- list(
    opcs4_lkp = list(
      coding = "240",
      code_col = "opcs4_code",
      code_type = "OPCS-4"
    ),
    data_coding_3 = list(
      coding = "3",
      code_col = "data_coding_3",
      code_type = "data_coding_3"
    ),
    data_coding_4 = list(
      coding = "4",
      code_col = "data_coding_4",
      code_type = "data_coding_4"
    ),
    data_coding_5 = list(
      coding = "5",
      code_col = "data_coding_5",
      code_type = "data_coding_5"
    ),
    data_coding_6 = list(
      coding = "6",
      code_col = "data_coding_6",
      code_type = "data_coding_6"
    )
  )

  result <- list()

  for (tbl in tables) {
    spec <- table_specs[[tbl]]

    lkp_table <- make_lkp_from_ukb_codings(
      ukb_codings = ukb_codings,
      Coding = spec$coding,
      Value_col_new_name = spec$code_col
    )

    meta <- lookup_metadata(
      code_type = spec$code_type,
      lookup_version = version,
      lookup_source = source,
      lookup_code_col = spec$code_col
    )

    result[[tbl]] <- list(
      lookup = list(
        table = lkp_table,
        metadata = meta
      )
    )
  }

  result
}
