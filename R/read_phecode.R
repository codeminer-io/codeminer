#' Read Phecode 1.2 files into lookup and mapping tables
#'
#' Reads the Phecode 1.2 definitions file and/or ICD mapping files and returns
#' structured lookup and mapping tables for use with CodeMiner.
#'
#' @param lkp_path Path to the Phecode 1.2 definitions CSV file. If `NULL`,
#'   this table is skipped.
#' @param icd10_map_path Path to the Phecode 1.2 to ICD-10 (beta) mapping CSV
#'   file. If `NULL`, this table is skipped.
#' @param icd9_map_path Path to the Phecode 1.2 to ICD-9 mapping `.rda` file.
#'   If `NULL`, this table is skipped.
#' @param version Character string for the version label (default: `"v1.2"`).
#' @param source Character string for the data source URL or description.
#'
#' @return A named list containing any of the following elements (depending on
#'   which paths are provided):
#'   * `phecode_lkp`: list with `lookup` → `list(table, metadata)`
#'   * `icd10_phecode`: list with `mapping` → `list(table, metadata)`
#'   * `icd9_phecode`: list with `mapping` → `list(table, metadata)`
#'
#' @examples
#' result <- read_phecode(
#'   lkp_path = dummy_phecode_lkp_path(),
#'   icd10_map_path = dummy_icd10_phecode_map_path()
#' )
#' result$phecode_lkp$lookup$table
#' result$icd10_phecode$mapping$table
#'
#' @seealso [add_phecode()], [get_phecode()]
#' @export
read_phecode <- function(
  lkp_path = NULL,
  icd10_map_path = NULL,
  icd9_map_path = NULL,
  version = "v1.2",
  source = "https://phewascatalog.org/phecodes"
) {
  if (is.null(lkp_path) && is.null(icd10_map_path) && is.null(icd9_map_path)) {
    cli::cli_abort(
      "At least one of {.arg lkp_path}, {.arg icd10_map_path}, or {.arg icd9_map_path} must be provided."
    )
  }

  result <- list()

  if (!is.null(lkp_path)) {
    phecode_lkp <- readr::read_csv(
      lkp_path,
      progress = FALSE,
      col_types = readr::cols(.default = "c")
    )

    meta <- lookup_metadata(
      code_type = "phecode",
      lookup_version = version,
      lookup_code_col = "phecode",
      lookup_description_col = "phenotype",
      lookup_source = source,
      col_filters = NULL
    )

    result$phecode_lkp <- list(
      lookup = list(
        table = phecode_lkp,
        metadata = meta
      )
    )
  }

  if (!is.null(icd10_map_path)) {
    icd10_phecode <- readr::read_csv(
      icd10_map_path,
      progress = FALSE,
      col_types = readr::cols(.default = "c")
    )

    meta <- mapping_metadata(
      from_code_type = "icd10",
      to_code_type = "phecode",
      map_version = version,
      map_source = source
    )

    result$icd10_phecode <- list(
      mapping = list(
        table = icd10_phecode,
        metadata = meta
      )
    )
  }

  if (!is.null(icd9_map_path)) {
    icd9_phecode_env <- new.env()
    load(icd9_map_path, envir = icd9_phecode_env)
    icd9_phecode_var <- ls(icd9_phecode_env)[[1]]
    icd9_phecode <- icd9_phecode_env[[icd9_phecode_var]]
    icd9_phecode$icd9 <- stringr::str_remove(icd9_phecode$icd9, pattern = "\\.")

    meta <- mapping_metadata(
      from_code_type = "icd9",
      to_code_type = "phecode",
      map_version = version,
      map_source = "https://github.com/PheWAS/PheWAS"
    )

    result$icd9_phecode <- list(
      mapping = list(
        table = icd9_phecode,
        metadata = meta
      )
    )
  }

  result
}
