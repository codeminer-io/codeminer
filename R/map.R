#' Map clinical codes from one coding system to another
#'
#' @param ... Codes to map. Supports flexible input like [CODES()]. Special
#'   value: `"all"` returns all mapped codes.
#' @param from Coding system that `...` codes belong to. Optional if input is a
#'   codelist with code_type.
#' @param to Coding system to map codes to.
#' @param map_version Version of the mapping table to use.
#' @param col_filters Column filters to apply to the **mapping table**. One of:
#'   - `"default"` (default): apply session-pinned or metadata-defined default
#'     filters
#'   - `NULL`: no filtering (return all rows)
#'   - A named list of `column_name = c(values)` pairs for explicit filtering
#'
#'   Note: this controls filtering of the mapping table, not the target lookup
#'   table (which uses its own default col_filters).
#' @inheritParams CODES
#'
#' @details If no mapping table matching the `from -> to` direction is found,
#' but there is a table for `to -> from`, `MAP()` will return the reverse
#' mapping with a warning. Note that this is not guaranteed to be correct, as
#' most mapping tables only work one way.
#'
#' @return A `codeminer_codelist` of the mapped codes with their descriptions.
#'
#'   If using `codes = "all"`, returns the mapping table as a `data.frame` with
#'   columns:
#'   - `from`: the codes from the source code system
#'   - `to`: the mapped codes from the destination system
#'
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_mapping_table()] for adding new mapping tables to the codeminer
#'   database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # Single code
#' MAP("X40J4", from = "Read v3", to = "ICD-10")
#'
#' # Multiple codes
#' MAP("X40J4", "X40J5", from = "Read v3", to = "ICD-10")
#'
#' # || separated
#' MAP("X40J4 || X40J5", from = "Read v3", to = "ICD-10")
#'
#' # Data frame input (from is optional)
#' df <- data.frame(
#'   code = c("X40J4", "X40J5"),
#'   description = c("Desc 1", "Desc 2"),
#'   code_type = c("Read v3", "Read v3")
#' )
#' MAP(df, to = "ICD-10")
#'
#' # Return the mapping table itself
#' MAP("all", from = "Read v3", to = "ICD-10")
MAP <- function(
  ...,
  from = getOption("codeminer.map_from"),
  to = getOption("codeminer.map_to"),
  map_version = getOption("codeminer.map_version", default = "latest"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  col_filters = "default"
) {
  # Collect and validate input
  collected <- collect_codes_input(
    ...,
    type = from,
    call = rlang::current_env()
  )
  codes_vec <- collected$codes

  # If input was codelist, use its code_type as from
  if (!is.null(collected$code_type)) {
    from <- collected$code_type
  }

  # Handle special case: "all"
  if (length(codes_vec) == 1 && identical(codes_vec, "all")) {
    check_code_type(from, call = rlang::current_env())
    check_code_type(to, call = rlang::current_env())
    check_version(map_version, call = rlang::current_env())

    if (identical(from, to)) {
      codeminer_abort(
        "{.arg from} and {.arg to} must be different code types",
        call = rlang::current_env()
      )
    }

    con <- get_db_con()
    mapping_table <- get_mapping_table(
      from,
      to,
      map_version,
      col_filters = col_filters,
      con = con
    )
    return(dplyr::collect(mapping_table))
  }

  # Validate parameters
  check_code_type(from, call = rlang::current_env())
  check_code_type(to, call = rlang::current_env())
  check_version(map_version, call = rlang::current_env())

  if (identical(from, to)) {
    codeminer_abort(
      "{.arg from} and {.arg to} must be different code types",
      call = rlang::current_env()
    )
  }

  con <- get_db_con()

  mapping_table <- get_mapping_table(
    from,
    to,
    map_version,
    col_filters = col_filters,
    con = con
  )

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
  # Mapped target codes are looked up for descriptions; if any are absent from
  # the target lookup, enrich the resulting warning with the likely cause.
  return(with_lookup_miss_hint(
    CODES(mapped_codes, type = to, lookup_version = lookup_version),
    hint = c(
      "i" = paste0(
        "These codes are in the {from} to {to} mapping table but absent from the ",
        "{to} lookup table - check the mapping and lookup versions were built ",
        "together."
      )
    )
  ))
}

#' Get the full mapping table for a pair of code types
#'
#' Returns a lazy `dplyr::tbl()` containing the mapping table with standardised
#' column names (`from`, `to`) plus all additional columns from the underlying
#' database table. Call [dplyr::collect()] to materialise the result.
#'
#' This is useful for inspecting columns beyond the standard codelist output
#' returned by [MAP()].
#'
#' @param from The source code type to map from.
#' @param to The target code type to map to.
#' @param map_version The version to retrieve. Defaults to `"latest"`.
#' @param col_filters Column filters to apply. See [CODES()] for details.
#' @param con Optional DBI connection. If `NULL` (default), uses the
#'   workbench connection.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A lazy `dplyr::tbl()` with standardised columns (`from`, `to`)
#'   plus all other columns from the underlying table.
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [MAP()] for standardised codelist output,
#'   [get_codeminer_metadata()] for discovering available tables.
#' @examples
#' create_dummy_database()
#'
#' # Get the full Read 3 to ICD-10 mapping table
#' get_mapping_table("Read v3", "ICD-10") |> dplyr::collect()
get_mapping_table <- function(
  from,
  to,
  map_version = "latest",
  col_filters = "default",
  con = NULL,
  call = rlang::caller_env()
) {
  con <- get_db_con(con)
  this_meta <- get_metadata_for_mapping(con, from, to, map_version, call = call)
  tbl_name <- this_meta$mapping_table_name
  tbl <- dplyr::tbl(con, tbl_name)

  # Apply col_filters BEFORE column renaming (filter on original column names)
  pin_key <- paste(from, ">", to)
  resolved <- resolve_col_filters(
    col_filters,
    this_meta$col_filters,
    pin_type = "mapping",
    pin_key = pin_key
  )
  tbl <- apply_col_filters(tbl, resolved, tbl_name = tbl_name, call = call)

  check_meta_columns_exist(
    tbl,
    cols = c(from_col = this_meta$from_col, to_col = this_meta$to_col),
    tbl_name = tbl_name,
    metadata_type = "mapping",
    call = call
  )
  tbl <- dplyr::select(
    tbl,
    from = .env$this_meta$from_col,
    to = .env$this_meta$to_col,
    dplyr::everything()
  )
  return(tbl)
}

# Pre-check the columns named by mapping / lookup / relationship metadata
# exist on the underlying table. Raises a friendly error instead of letting
# the downstream `dplyr::select()` blow up with the generic
# "Column `from` doesn't exist" message.
check_meta_columns_exist <- function(
  tbl,
  cols,
  tbl_name,
  metadata_type,
  call = rlang::caller_env()
) {
  available <- colnames(tbl)
  missing_idx <- !cols %in% available
  if (!any(missing_idx)) {
    return(invisible(NULL))
  }
  problems <- cols[missing_idx]
  problem_lines <- vapply(
    seq_along(problems),
    function(i) {
      sprintf(
        "{.arg %s} = {.val %s}",
        names(problems)[i],
        unname(problems[i])
      )
    },
    character(1)
  )
  codeminer_abort(
    c(
      "{metadata_type} metadata for {.field {tbl_name}} names columns that do not exist on the underlying table.",
      "x" = paste(problem_lines, collapse = ", "),
      "i" = "Available columns: {.val {available}}.",
      "i" = "Rebuild the table - the read_* function's stored metadata no longer matches what's on disk."
    ),
    class = "codeminer_metadata_col_missing",
    call = call
  )
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

  # Filter to from/to pair, then use shared version resolution
  pair_meta <- all_meta[
    all_meta$from_code_type == from & all_meta$to_code_type == to,
  ]
  pin_key <- paste(from, ">", to)
  this_meta <- resolve_versioned_metadata(
    pair_meta,
    code_type_val = from,
    version_val = map_version,
    code_type_col = "from_code_type",
    version_col = "map_version",
    pin_type = "mapping",
    pin_key = pin_key,
    type_label = "mapping",
    add_fun_name = "codeminer::add_mapping_table",
    call = call
  )

  if (swap) {
    # Swap the from and to column names if necessary
    old_from_col <- this_meta$from_col
    this_meta$from_col <- this_meta$to_col
    this_meta$to_col <- old_from_col
  }

  return(this_meta)
}
