
# CONSTANTS ---------------------------------------------------------------

# UKB CODE MAPPINGS -------------------------------------------------------

# used by functions in clinical_codes.R

# NOTES: if editing this, always refer to `ukb_code_mappings_sheet_names` and
# `ukb_code_mappings_code_types` first - these should contain all possible code
# types and sheets from the UKB excel file (resource 592). Tests in
# test_data_raw_constants.R reply on the accuracy of these.

# names of excel spreadsheets in ukb_code_mappings (UKB resource 592) -----

ALL_LKPS_MAPS_TABLE_NAMES <- c(
  "bnf_lkp",
  "dmd_lkp",
  "icd9_lkp",
  "icd10_lkp",
  "icd9_icd10",
  "read_v2_lkp",
  "read_v2_drugs_lkp",
  "read_v2_drugs_bnf",
  "read_v2_icd9",
  "read_v2_icd10",
  "read_v2_opcs4",
  "read_v2_read_ctv3",
  "read_ctv3_lkp",
  "read_ctv3_icd9",
  "read_ctv3_icd10",
  "read_ctv3_opcs4",
  "read_ctv3_read_v2",
  "bnf_dmd",
  "opcs4_lkp",
  "ctv3sctmap2",
  "rcsctmap2",
  "phecode_lkp"
)

# TODO - unhash this
# assertthat::assert_that(
#   all(sort(ALL_LKPS_MAPS_TABLE_NAMES) == sort(names(all_lkps_maps))),
#   msg = "`ALL_LKPS_MAPS_TABLE_NAMES` does not match the expected names"
# )

# TO DELETE?
# colnames for each excel spreadsheet in resource 592 ---------------------
# colnames_for_ukb_code_mappings_sheet_names <- ukb_code_mappings_sheet_names %>%
#   purrr::set_names() %>%
#   purrr::map(~ names(ukb_code_mappings[[.x]]))

# clinical code system to lookup table map --------------------------------
# mappings note, BNF - 'description_col' is for chemical substances only (TODO
# amend this?)

# TODO - category col for tables?

CODE_TYPE_TO_LKP_TABLE_MAP <- tibble::tribble(
  ~code, ~code_label, ~lkp_table, ~code_col, ~description_col, ~preferred_synonym_col, ~preferred_code, ~grouping_col, ~filter_cols,
  "icd10", "ICD-10", "icd10_lkp", "ALT_CODE", "DESCRIPTION", NA, NA, "category", NA,
  "icd9", "ICD-9", "icd9_lkp", "ICD9", "DESCRIPTION_ICD9", NA, NA, "category", NA,
  "read3", "Read 3", "read_ctv3_lkp", "read_code", "term_description", "description_type", "P", NA, NA,
  "read2", "Read 2", "read_v2_lkp", "read_code", "term_description", "term_code", "00", NA, NA,
  "sct", "SNOMED CT", "sct_description", "conceptId", "term_description", "typeId_description", "900000000000003001", NA, list(list(active_concept = c("*0*", "*1*"), active_description = c("0", "*1*"))),
  "opcs4", "OPCS4", "opcs4_lkp", "opcs4_code", "description", NA, NA, "category", NA,
  "phecode", "Phecode", "phecode_lkp", "phecode", "phenotype", NA, NA, "category", NA,
  "read2_drugs", "Read 2, drugs", "read_v2_drugs_lkp", "read_code", "term_description", NA, NA, NA, NA,
  "bnf", "BNF", "bnf_lkp", "BNF_Code", "Description", NA, NA, "BNF_Chemical_Substance", NA,
  # "dmd", "dmd_lkp", "concept_id", "term", NA, NA,
  "dmd", "DMD", "bnf_dmd", "snomed_code", "dm_d_product_description", NA, NA, "dm_d_product_description", NA,
  "data_coding_3", "Self-reported cancer (dc-3)", "self_report_cancer", "data_coding_3", "description", NA, NA, "category", NA,
  "data_coding_4", "Self-reported medications (dc-4)", "self_report_medication", "data_coding_4", "description", NA, NA, "category", NA,
  "data_coding_5", "Self-reported operations (dc-5)", "self_report_operation", "data_coding_5", "description", NA, NA, "category", NA,
  "data_coding_6", "Self-reported non-cancer (dc-6)", "self_report_non_cancer", "data_coding_6", "description", NA, NA, "category", NA,
)

# CLINICAL_CODE_MAPPINGS_MAP ----------------------------------------------

# used by `map_codes()`
# 'from' and 'to' cols: possible mapping combinations
# 'mapping_table': the appropriate mapping table to use for a 'from'/'to' combination
# 'from_col' and 'to_col': the columns to use when mapping
# Note, `preferred_synonym_col` and `preferred_code` refer to `to_col`
CLINICAL_CODE_MAPPINGS_MAP <- tibble::tribble(
  ~from, ~to, ~mapping_table, ~from_col, ~to_col, ~preferred_synonym_col, ~preferred_code, ~filter_cols,
  "icd9", "icd10", "icd9_icd10", "ICD9", "ICD10", NA, NA, NA,
  "read2_drugs", "bnf", "read_v2_drugs_bnf", "read_code", "bnf_code", NA, NA, NA,
  "read2", "icd9", "read_v2_icd9", "read_code", "icd9_code", NA, NA, NA,
  "read2", "icd10", "read_v2_icd10", "read_code", "icd10_code", NA, NA, list(list(icd10_code_def = c("*1*", "*15*", "*3*", "*5*", "*7*", "*8*", "2"))),
  "read2", "opcs4", "read_v2_opcs4", "read_code", "opcs_4.2_code", NA, NA, NA,
  "read2", "read3", "read_v2_read_ctv3", "READV2_CODE", "READV3_CODE", "TERMV3_TYPE", "P", list(list(IS_ASSURED = "*1*")),
  "read3", "icd9", "read_ctv3_icd9", "read_code", "icd9_code", NA, NA, NA,
  "read3", "icd10", "read_ctv3_icd10", "read_code", "icd10_code", NA, NA, list(list(mapping_status = c("*E*", "*G*", "*D*", "R", "A", "U"), refine_flag = c("*C*", "*P*", "M"), element_num = c("*0*", as.character(1:3)), block_num = c("*0*", as.character(1:14)))),
  "read3", "opcs4", "read_ctv3_opcs4", "read_code", "opcs4_code", NA, NA, NA,
  "read3", "read2", "read_ctv3_read_v2", "READV3_CODE", "READV2_CODE", "TERMV2_TYPE", "P", list(list(IS_ASSURED = "*1*")),
  "bnf", "sct", "bnf_dmd", "bnf_code", "snomed_code", NA, NA, NA,
  "icd10", "phecode", "icd10_phecode", "ALT_CODE", "PHECODE", NA, NA, NA,
  "icd9", "phecode", "icd9_phecode", "icd9", "phecode", NA, NA, NA,
  "sct", "icd10", "sct_icd10", "referencedComponentId", "mapTarget", NA, NA, NA,
  "sct", "opcs4", "sct_opcs4", "referencedComponentId", "mapTarget", NA, NA, NA,
  "read2", "sct", "rcsctmap2", "ReadCode", "ConceptId", NA, NA, list(list(IS_ASSURED = "*1*", MapStatus = "*1*")),
  "read3", "sct", "ctv3sctmap2", "CTV3_CONCEPTID", "SCT_CONCEPTID", NA, NA, list(list(IS_ASSURED = "*1*", MAPSTATUS = "*1*"))
)

# PUBLIC ------------------------------------------------------------------

#' Metadata
#'
#' Returns a named list of tibbles which show the metadata used for (i) lookup
#' tables and (ii) mapping tables.
#'
#' @return A named list.
#' @export
#'
#' @examples
#' codemapper_metadata()
codemapper_metadata <- function() {
  list(
    lookup_tables = CODE_TYPE_TO_LKP_TABLE_MAP,
    mapping_tables = CLINICAL_CODE_MAPPINGS_MAP
  )
}

#' Get metadata from `all_lkps_maps` database
#'
#' Retrieves metadata for all tables as a single data frame.
#'
#' @inheritParams lookup_codes
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' get_all_lkps_maps_db_metadata()
#' }
get_all_lkps_maps_db_metadata <- function(all_lkps_maps = NULL) {
  # connect to database file path if `all_lkps_maps` is a string, or `NULL`
  if (is.character(all_lkps_maps)) {
    con <- check_all_lkps_maps_path(all_lkps_maps)
    all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  } else if (is.null(all_lkps_maps)) {
    if (Sys.getenv("ALL_LKPS_MAPS_DB") != "") {
      # message(paste0("Attempting to connect to ", Sys.getenv("ALL_LKPS_MAPS_DB")))
      con <-
        check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"))
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    } else if (file.exists("all_lkps_maps.db")) {
      # message("Attempting to connect to all_lkps_maps.db in current working directory")
      con <- check_all_lkps_maps_path("all_lkps_maps.db")
      all_lkps_maps <- ukbwranglr::db_tables_to_list(con)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    } else {
      stop(
        "No/invalid path supplied to `all_lkps_maps` and no file called 'all_lkps_maps.db' found in current working directory. See `?all_lkps_maps_to_db()`"
      )
    }
  }

  validate_all_lkps_maps()

  all_lkps_maps[subset(names(all_lkps_maps),
                       stringr::str_starts(names(all_lkps_maps),
                                           "metadata_"))] %>%
    purrr::map(dplyr::collect) %>%
    dplyr::bind_rows(.id = "source")
}
