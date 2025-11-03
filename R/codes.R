#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup
#' @param code_type character. Type of clinical code system to be searched.
#'   Depends on what is available in the lookup tables. See [add_lookup_table()]
#'   on how to add new lookup tables. This can also be configured through the `codeminer.code_type` option.
#'
#' @return A `data.frame` containing the codes and their descriptions
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # look up ICD10 codes
#' CODES(
#'   codes = c("E10", "E11"),
#'   code_type = "icd10"
#' )
CODES <- function(codes, code_type = getOption("codeminer.code_type")) {
  if (is.data.frame(codes)) {
    code_type <- codes$code_type[1]
    codes <- codes$code
  }

  check_codes(codes)
  check_code_type(code_type)

  lookup_metadata <- get_lookup_metadata()
  if (!(code_type %in% lookup_metadata$code_type)) {
    cli::cli_abort(c(
      "Code type '{code_type}' not found in lookup metadata.",
      "i" = "Did you add the lookup table with {.fun codeminer::add_lookup_table}?"
    ))
  }

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

  result <- standardise_output_fn(
    result,
    lkp_table = lkp_table,
    code_col = code_col,
    description_col = description_col,
    code_type = code_type
  )

  return(result)
}

# Argument validation helpers
check_codes <- function(codes) {
  if (!is.character(codes)) {
    cli::cli_abort(
      "{.arg codes} must be a character vector, not {typeof(codes)}"
    )
  }
}

check_code_type <- function(code_type) {
  if (!is.character(code_type)) {
    cli::cli_abort(
      "{.arg code_type} must be a character vector, not {typeof(code_type)}"
    )
  }
  if (length(code_type) != 1) {
    cli::cli_abort(
      "{.arg code_type} must have length 1, not {length(code_type)}"
    )
  }
}
