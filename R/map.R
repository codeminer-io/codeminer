#' Map clinical codes from one coding system to another
#'
#' Uses the code mapping file provided by UK Biobank
#' (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}).
#'
#' The values for arguments \code{from} and \code{to} must be one of
#' \code{read2}, \code{read3}, \code{icd9}, \code{icd10}, \code{bnf},
#' \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#'
#' @param codes A character vector of codes to be mapped.
#' @param from Coding system that \code{codes} belong to.
#' @param to Coding system to map \code{codes} to.
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised for the coding system being mapped from, then
#'   either an error or warning will be raised.
#' @param reverse_mapping If 'error' (default), an error raised if attempting to
#'   map between coding systems for which a mapping table does not exist. If
#'   'warning', will raise a warning and attempt to use an existing mapping
#'   table in the opposite direction (for example, a mapping from ICD10 to Read
#'   3 would be attempted using the Read 3-to-ICD10 mapping table).
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns from the relevant mapping table. Note
#'   that this may or may not include code descriptions.
#' @inheritParams CHILDREN
#' @inheritParams CODES
#'
#' @name MAP
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # map codes from Read 2 to ICD10
#' MAP(
#'   codes = "G20..",
#'   from = "read2",
#'   to = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
MAP <- function(
  codes,
  to = getOption("codeminer.map_to"),
  from = getOption("codeminer.map_from"),
  all_lkps_maps = NULL,
  codes_only = FALSE,
  standardise_output = TRUE,
  unrecognised_codes = getOption("codeminer.unrecognised_codes_mapped"),
  preferred_description_only = TRUE,
  reverse_mapping = getOption("codeminer.reverse_mapping"),
  col_filters = getOption("codeminer.col_filters")
) {
  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    from <- unique(codes$code_type)
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  assertthat::assert_that(
    is.logical(codes_only),
    msg = "`code_only` must be either 'TRUE' or 'FALSE'"
  )

  assertthat::assert_that(
    !(codes_only & standardise_output),
    msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`"
  )

  # check mapping args and get required details - mapping_table, from_col and
  # to_col
  mapping_params <- check_mapping_args(
    from = from,
    to = to,
    reverse_mapping = reverse_mapping
  )

  from_col <- mapping_params$from_col
  to_col <- mapping_params$to_col
  mapping_table <- mapping_params$mapping_table

  check_table_exists_in_all_lkps_maps(
    all_lkps_maps = all_lkps_maps,
    table_name = mapping_table
  )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_value_for_mapping_sheet(
      mapping_table = mapping_table,
      value = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "preferred_code"
      )
  }

  # do mapping
  result <- all_lkps_maps[[mapping_table]] %>%
    dplyr::filter(.data[[from_col]] %in% codes) %>%
    dplyr::filter(!is.na(.data[[to_col]])) %>%
    dplyr::collect()

  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = mapping_table,
      col_filters = col_filters
    )
  }

  # check for unrecognised codes
  missing_codes <- subset(codes, !codes %in% result[[from_col]])

  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = mapping_table,
    code_type = from
  )

  # return result
  if (nrow(result) == 0) {
    message("No codes found after mapping.")
    return(result)
  } else {
    # return either unique codes only, or df including descriptions
    if (codes_only) {
      result <- unique(result[[to_col]])

      return(result)
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `CODES` if
      # `standardise_output` is `TRUE`

      codes <- unique(result[[to_col]])

      return(
        CODES(
          codes = codes,
          code_type = to,
          all_lkps_maps = all_lkps_maps,
          preferred_description_only = preferred_description_only,
          unrecognised_codes = unrecognised_codes
        )
      )
    } else {
      return(result)
    }
  }
}

#' Get the mapping table for the given from and to types in standardised format
#'
#' @param con A database connection.
#' @param from The source code type to map from
#' @param to The target code type to map to
#' @param version The version of the mapping table.
#' @param call The calling environment. Passed to [cli::cli_abort].
#'
#' @return A data frame containing the lookup table with two columns: `from` and `to`.
#' @keywords internal
get_mapping_table <- function(
  con,
  from,
  to,
  version,
  call = rlang::caller_env()
) {
  this_meta <- get_meta_for_mapping(con, from, to, version, call = call)
  tbl_name <- this_meta$mapping_table_name
  tbl <- get_table_from_db(con, tbl_name)

  tbl <- dplyr::select(tbl, from = this_meta$from_col, to = this_meta$to_col)

  return(tbl)
}

get_meta_for_mapping <- function(
  con,
  from,
  to,
  version,
  call = rlang::caller_env()
) {
  all_meta <- get_mapping_metadata(con = con)

  # Check if we need to swap the from and to types
  if (!(from %in% all_meta$from_code_type) && from %in% all_meta$to_code_type) {
    old_from <- from
    from <- to
    to <- old_from
  }

  this_meta <- dplyr::filter(
    all_meta,
    .data$from_code_type == from,
    .data$to_code_type == to
  )

  if (nrow(this_meta) == 0) {
    cli::cli_abort(
      c(
        "No mapping table found for from type '{from}' and to type '{to}'.",
        "i" = "Did you add the mapping table with {.fun codeminer::add_mapping_table}?"
      ),
      call = call
    )
  }

  available_versions <- this_meta$lookup_version

  if (!(version %in% available_versions)) {
    cli::cli_abort(c(
      "No metadata found for '{code_type}' version '{version}'",
      "i" = "Available versions for '{code_type}': {available_versions}"
    ))
  }

  this_meta <- dplyr::filter(this_meta, .data$mapping_version == version)
  stopifnot(nrow(this_meta) == 1) # expect a unique mapping table

  return(this_meta)
}
