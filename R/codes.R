#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup
#' @param code_type character. Type of clinical code system to be searched. One
#'   of `r knitr::combine_words(CODE_TYPE_TO_LKP_TABLE_MAP$code, and = "or ")`.
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns from the relevant look up table.
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised, then either an error or warning will be raised.
#' @param .return_unrecognised_codes If `TRUE`, return a vector of unrecognised
#'   codes only.
#' @param col_filters A named list where each name in the list refers to the
#'   name of a lookup or mapping table. Each item is also a named list, where
#'   the names refer to column names in the corresponding table, and the items
#'   are vectors of values to filter for. For example, `list(my_lookup_table =
#'   list(colA = c("A", "B"))` will result in `my_lookup_table` being filtered
#'   for rows where `colA` is either 'A' or 'B'. Uses `default_col_filters()` by
#'   default. Set to `NULL` to remove all filters.
#' @param preferred_description_only If `TRUE` (default), return only preferred
#'   descriptions for clinical codes with synonyms. Will only apply if
#'   \code{standardise_output} is also \code{TRUE}.
#' @param all_lkps_maps Either a named list of lookup and mapping tables
#'   (either data frames or `tbl_dbi` objects), or the path to a Duckdb database
#'   containing these tables. If `NULL`, will attempt to connect to a Duckdb
#'   database named 'all_lkps_maps.db' in the current working directory, or to
#'   a a Duckdb database specified by an environmental variable named
#'   'ALL_LKPS_MAPS_DB' (see
#'   [here](https://resources.numbat.space/using-rprofile-and-renviron.html#renviron)
#'   for how to set environment variables using a `.Renviron` file). The latter
#'   method will be used in preference.
#'
#' @return data frame
#' @export
#' @name CODES
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # look up ICD10 codes
#' CODES(
#'   codes = c("E10", "E11"),
#'   code_type = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
CODES <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  all_lkps_maps = NULL,
  preferred_description_only = TRUE,
  standardise_output = TRUE,
  unrecognised_codes = getOption("codeminer.unrecognised_codes_lookup"),
  col_filters = getOption("codeminer.col_filters"),
  .return_unrecognised_codes = FALSE
) {
  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    code_type <- codes$code_type[1]
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  stopifnot(!is.null(code_type))

  match.arg(
    arg = code_type,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  check_table_exists_in_all_lkps_maps(
    all_lkps_maps = all_lkps_maps,
    table_name = lkp_table
  )

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  # determine description column for lookup sheet
  description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "description_col"
    )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # lookup - filter lookup sheet for codes
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(.data[[code_col]] %in% codes) %>%
    dplyr::collect()

  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = lkp_table,
      col_filters = col_filters
    )
  }

  # check for unrecognised codes
  missing_codes <- subset(codes, !codes %in% result[[code_col]])

  if (.return_unrecognised_codes) {
    # optionally return vector of unrecognised codes only
    message(paste0(
      "Returning unrecognised codes only. N unrecognised: ",
      length(missing_codes)
    ))
    return(missing_codes)
  }

  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = lkp_table,
    code_type = code_type
  )

  # filter for preferred code descriptions only if requested
  if (
    preferred_description_only &
      !is.na(preferred_description_col)
  ) {
    result <- result %>%
      dplyr::filter(
        .data[[preferred_description_col]] == preferred_description_code
      )
  }

  # standardise output if requested
  if (standardise_output) {
    result <- standardise_output_fn(
      result,
      lkp_table = lkp_table,
      code_col = code_col,
      description_col = description_col,
      code_type = code_type
    )
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found")
    return(result)
  } else {
    # return either unique codes only, or df including code descriptions
    return(result)
  }
}
