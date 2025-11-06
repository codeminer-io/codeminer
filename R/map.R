#' Map clinical codes from one coding system to another
#'
#' @param codes A character vector of codes to be mapped. If passing `"all"`, all mapped codes will be returned.
#' @param from Coding system that `codes` belong to.
#' @param to Coding system to map `codes` to.
#' @param version Version of the mapping table to use.
#'
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' MAP("X40J4", from = "read3", to = "icd10")
MAP <- function(
  codes,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  version = "latest"
) {
  check_mapping_args(from = from, to = to, version = version)

  con <- connect_to_db()
  check_database(con)

  mapping_table <- get_mapping_table(con, from, to, version)

  if (length(codes) == 1 && codes == "all") {
    all_mapped_codes <- unique(dplyr::pull(mapping_table, .data$to))
    return(CODES(all_mapped_codes, code_type = to, version = version))
  }

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
  swap <- !(from %in% all_meta$from_code_type) &&
    from %in% all_meta$to_code_type
  if (swap) {
    cli::cli_warn(
      c(
        "No explicit mapping table found for '{from} -> {to}', but found '{to} -> {from}'.",
        "Using the reverse of '{to} -> {from}' instead.",
        "i" = "You can add a new mapping table with {.fun codeminer::add_mapping_table}."
      ),
      call = call
    )
    old_from <- from
    from <- to
    to <- old_from
  }

  all_version_meta <- dplyr::filter(
    all_meta,
    .data$from_code_type == from,
    .data$to_code_type == to
  )
  if (version == "latest") {
    version <- get_latest_version(all_version_meta$mapping_version)
  }
  this_meta <- dplyr::filter(all_version_meta, .data$mapping_version == version)

  if (nrow(this_meta) == 0) {
    cli::cli_abort(
      c(
        "No mapping table found for from type '{from}' and to type '{to}', version '{version}'.",
        "i" = "Did you add the mapping table with {.fun codeminer::add_mapping_table}?"
      ),
      call = call
    )
  }

  if (swap) {
    # Swap the from and to column names if necessary
    old_from_col <- this_meta$from_col
    this_meta$from_col <- this_meta$to_col
    this_meta$to_col <- old_from_col
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
  if (length(from) != 1) {
    cli::cli_abort(
      "{.arg from} must have length 1, not {length(from)}",
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
