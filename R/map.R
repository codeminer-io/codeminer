#' Map clinical codes from one coding system to another
#'
#' @param codes Character vector, `||` separated string, or data frame with codes to be mapped.
#'   Special value: `"all"` returns all mapped codes.
#' @param from Coding system that `codes` belong to. Optional if `codes` is a codelist with code_type.
#' @param to Coding system to map `codes` to.
#' @param map_version Version of the mapping table to use.
#' @inheritParams CODES
#'
#' @details
#' If no mapping table matching the `from -> to` direction is found, but there is a table for `to -> from`,
#' `MAP()` will return the reverse mapping with a warning. Note that this is not guaranteed to be correct,
#' as most mapping tables only work one way.
#'
#' @return A `codeminer_codelist` of the mapped codes with their descriptions.
#'
#'   If using `codes = "all"`, returns the mapping table as a `data.frame` with columns:
#'   - `from`: the codes from the source code system
#'   - `to`: the mapped codes from the destination system
#'
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_mapping_table()] for adding new mapping tables to the codeminer database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # Character vector
#' MAP("X40J4", from = "Read 3", to = "ICD-10")
#'
#' # || separated
#' MAP("X40J4 || X40J5", from = "Read 3", to = "ICD-10")
#'
#' # Data frame input (from is optional)
#' df <- data.frame(
#'   code = c("X40J4", "X40J5"),
#'   description = c("Desc 1", "Desc 2"),
#'   code_type = c("Read 3", "Read 3")
#' )
#' MAP(df, to = "ICD-10")
#'
#' # Return the mapping table itself
#' MAP("all", from = "Read 3", to = "ICD-10")
MAP <- function(
  codes,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  map_version = getOption("codeminer.map_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest")
) {
  # Handle special case: "all"
  if (is.character(codes) && length(codes) == 1 && identical(codes, "all")) {
    check_code_type(from, call = rlang::caller_env())
    check_code_type(to, call = rlang::caller_env())
    check_version(map_version, call = rlang::caller_env())

    if (identical(from, to)) {
      codeminer_abort(
        "{.arg from} and {.arg to} must be different code types",
        call = rlang::caller_env()
      )
    }

    con <- connect_to_db()
    check_database(con)
    mapping_table <- get_mapping_table(con, from, to, map_version)
    return(dplyr::collect(mapping_table))
  }

  # Prepare input (handles character/||/codelist, extracts code_type if available)
  prepared <- prepare_codes_input(codes, from, arg_name = "codes")
  codes_vec <- prepared$codes

  # If input was codelist, use its code_type as from (unless from was explicitly provided)
  if (!is.null(prepared$code_type)) {
    from <- prepared$code_type
  }

  # Validate parameters
  check_code_type(from, call = rlang::caller_env())
  check_code_type(to, call = rlang::caller_env())
  check_version(map_version, call = rlang::caller_env())

  if (identical(from, to)) {
    codeminer_abort(
      "{.arg from} and {.arg to} must be different code types",
      call = rlang::caller_env()
    )
  }

  con <- connect_to_db()
  check_database(con)

  mapping_table <- get_mapping_table(con, from, to, map_version)

  mapping <- dplyr::filter(
    mapping_table,
    .data$from %in% .env$codes_vec,
    !is.na(.data$to)
  ) |>
    dplyr::collect()

  missing_codes <- setdiff(codes_vec, mapping$from)
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "mapping",
      table_meta = get_metadata_for_mapping(con, from, to, map_version)
    )
  }
  mapped_codes <- unique(mapping$to)
  return(CODES(mapped_codes, code_type = to, lookup_version = lookup_version))
}

#' Get the mapping table for the given from and to types in standardised format
#'
#' @param con A database connection.
#' @param from The source code type to map from
#' @param to The target code type to map to
#' @param map_version The version of the mapping table.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A data frame containing the lookup table with two columns: `from` and `to`.
#' @keywords internal
get_mapping_table <- function(
  con,
  from,
  to,
  map_version,
  call = rlang::caller_env()
) {
  this_meta <- get_metadata_for_mapping(con, from, to, map_version, call = call)
  tbl_name <- this_meta$mapping_table_name
  tbl <- dplyr::tbl(con, tbl_name)

  tbl <- dplyr::rename(tbl, from = this_meta$from_col, to = this_meta$to_col)
  return(tbl)
}

get_metadata_for_mapping <- function(
  con,
  from,
  to,
  map_version,
  call = rlang::caller_env()
) {
  all_meta <- get_mapping_metadata(con = con)

  # Check if we need to swap the from and to types
  swap <- !(from %in% all_meta$from_code_type) &&
    from %in% all_meta$to_code_type
  if (swap) {
    codeminer_warn(
      c(
        "!" = "No explicit mapping table found for '{from} -> {to}', but found '{to} -> {from}'.",
        ">" = "Using the reverse of '{to} -> {from}' instead.",
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
  if (map_version == "latest") {
    map_version <- get_latest_version(all_version_meta$map_version)
  }
  this_meta <- dplyr::filter(
    all_version_meta,
    .data$map_version == .env$map_version
  )

  if (nrow(this_meta) == 0) {
    codeminer_abort(
      c(
        "No mapping table found for from type '{from}' and to type '{to}', version '{map_version}'.",
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
