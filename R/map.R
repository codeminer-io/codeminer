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
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  version = "v0"
) {
  check_mapping_args(from = from, to = to, version = version)

  con <- connect_to_db()
  check_database(con)

  mapping_table <- get_mapping_table(con, from, to, version)

  mapping <- dplyr::filter(
    mapping_table,
    .data$from %in% codes,
    !is.na(.data$to)
  ) |>
    dplyr::collect()

  missing_codes <- setdiff(codes, mapping$from)
  if (length(missing_codes) > 0) {
    cli::cli_warn(
      "The following codes were not found in the mapping table: {.code {missing_codes}}"
    )
  }
  mapped_codes <- unique(mapping$to)
  return(CODES(mapped_codes, code_type = to, version = version))
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
    .data$to_code_type == to,
    .data$mapping_version == version
  )

  if (nrow(this_meta) == 0) {
    cli::cli_abort(
      c(
        "No mapping table found for from type '{from}' and to type '{to}', version '{version}'.",
        "i" = "Did you add the mapping table with {.fun codeminer::add_mapping_table}?"
      ),
      call = call
    )
  }

  stopifnot(nrow(this_meta) == 1) # expect a unique mapping table
  return(this_meta)
}

check_mapping_args <- function(
  from,
  to,
  version,
  call = rlang::caller_env()
) {
  check_version(version)

  if (!is.character(from)) {
    cli::cli_abort(
      "{.arg from} must be of type character, not {typeof(from)}",
      call = call
    )
  }
  if (length(from) != 1) {
    cli::cli_abort(
      "{.arg from} must have length 1, not {length(from)}",
      call = call
    )
  }
  if (!is.character(to)) {
    cli::cli_abort(
      "{.arg to} must be of type character, not {typeof(to)}",
      call = call
    )
  }
  if (length(to) != 1) {
    cli::cli_abort(
      "{.arg to} must have length 1, not {length(to)}",
      call = call
    )
  }
}
