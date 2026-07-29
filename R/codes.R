#' Look up descriptions for clinical codes
#'
#' Returns a codelist with descriptions for the codes of interest. Supports
#' flexible input: character vectors, `||` separated strings, or data frames.
#'
#' @param ... Codes to look up. Can be:
#'   - Character vectors: `CODES("E10", "E11", type = "ICD-10")`
#'   - `||` separated strings: `CODES("E10 || E11", type = "ICD-10")`
#'   - Data frame with code/description/code_type columns: `CODES(my_df)`
#'   - Mixed: `CODES("E10", my_vector, "E13 || E14", type = "ICD-10")`
#'
#'   Special values: `"all"` returns all codes; empty input returns empty
#'   codelist.
#'
#'   Comments can be added with `<< >>` syntax: `"E10 << Type 1 diabetes >>"`.
#' @param type character. Type of clinical code system to be searched. Optional
#'   if input is a data frame with code_type column. Depends on what is
#'   available in the lookup tables. See [add_lookup_table()] on how to add new
#'   lookup tables. This can also be configured through the
#'   `codeminer.code_type` option.
#' @param lookup_version character. Version of the lookup table to use. Default:
#'   `"latest"`. Can be configured through the `codeminer.lookup_version`
#'   option.
#' @param preferred_description_only logical. If `TRUE`, only returns the
#'   preferred description for each code. Default: `FALSE`.
#' @param col_filters Column filters for the tables this query touches. One
#'   of:
#'   - `"default"` (default): apply session-pinned filters
#'     ([codeminer_set_col_filters()]) where set, else the metadata-defined
#'     default filters.
#'   - `NULL` (or `NA`): no filtering for any table this query touches.
#'   - A table-keyed list — the shape returned by [get_col_filters()] — with
#'     top-level names `lookup` / `relationship` / `mapping`, keyed by code
#'     type (or `"from > to"` pair for mappings), e.g.
#'     `list(lookup = list("SNOMED CT" = list(active_concept = "1")))`.
#'     Each table entry *replaces* that table's pinned/default filters
#'     wholesale; tables the list does not name keep their pins/defaults.
#'     `NA` as a table entry un-filters that one table. The filters reach
#'     every table the query touches (e.g. both the mapping table and the
#'     target lookup in [MAP()]).
#'
#'   To tweak one column while keeping a table's other default filters,
#'   amend [get_col_filters()] output and pass it back. Entries that match
#'   no registered table or column trigger a warning.
#'
#' @return A `codeminer_codelist` object (tibble) containing the codes and their
#'   descriptions
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [add_lookup_table()] for adding new lookup tables to the database.
#' @examples
#' # Set up a temporary dummy database
#' temp_db <- tempfile(fileext = ".duckdb")
#' create_dummy_database(temp_db)
#'
#' # Multiple arguments
#' CODES("E10", "E11", type = "ICD-10")
#'
#' # With comments
#' CODES("E10 << Type 1 diabetes >>", type = "ICD-10")
#'
#' # || separated string
#' CODES("E10 || E11", type = "ICD-10")
#'
#' # Splice operator
#' my_codes <- c("E10", "E11")
#' CODES(!!!my_codes, type = "ICD-10")
#'
#' # Data frame input
#' df <- data.frame(
#'   code = c("E10", "E11"),
#'   description = c("Type 1", "Type 2"),
#'   code_type = c("ICD-10", "ICD-10")
#' )
#' CODES(df)
CODES <- function(
  ...,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  # Apply the col_filters argument as a call-scoped overlay: every table read
  # this query makes (directly or via internal helpers) resolves against it.
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  # Validate logical parameter
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  # Special handling: if single data frame input that's already a codelist, return as-is
  args <- rlang::list2(...)
  if (
    length(args) == 1 &&
      is.data.frame(args[[1]]) &&
      inherits(args[[1]], "codeminer_codelist")
  ) {
    df <- args[[1]]

    # Validate type matches if provided
    df_code_type <- unique(df$code_type)
    type_missing <- is.null(type) || identical(type, "")

    if (!type_missing && !is.na(df_code_type) && df_code_type != type) {
      codeminer_abort(
        c(
          "Conflicting {.arg type} values.",
          "x" = "Data frame has: {.val {df_code_type}}",
          "x" = "Argument specifies: {.val {type}}",
          "i" = "Both must match, or omit the {.arg type} argument to use the data frame value."
        ),
        call = rlang::current_env()
      )
    }

    return(df)
  }

  # Use helper to collect and validate all other inputs
  collected <- collect_codes_input(
    ...,
    type = type,
    call = rlang::current_env()
  )
  codes_vec <- collected$codes

  # Use codelist code_type if provided
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  # Empty after parsing
  if (length(codes_vec) == 0) {
    empty_cols <- stats::setNames(
      replicate(3, character(), simplify = FALSE),
      codelist_cols()
    )
    return(as_codelist(tibble::as_tibble(empty_cols)))
  }

  # Handle special case: "all"
  if (length(codes_vec) == 1 && identical(codes_vec, "all")) {
    check_code_type(type)
    check_version(lookup_version)

    con <- get_db_con()
    lookup_table <- get_lookup_table(
      type,
      lookup_version = lookup_version,
      con = con
    )
    result <- dplyr::collect(lookup_table)

    if (preferred_description_only) {
      result <- dplyr::filter(result, .data$preferred_description)
    }

    # Select only standard columns
    result <- dplyr::select(result, dplyr::all_of(codelist_cols()))

    return(as_codelist(result))
  }

  # Validate type parameter
  check_code_type(type)
  check_version(lookup_version)

  con <- get_db_con()

  # Resolve the lookup metadata once and reuse it for both the table lookup and
  # the missing-codes warning, rather than resolving it twice per call.
  this_meta <- get_metadata_for_lookup(con, type, lookup_version)

  lookup_table <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    con = con,
    meta = this_meta
  )

  result <- lookup_table |>
    dplyr::filter(.data[["code"]] %in% .env$codes_vec) |>
    dplyr::collect()

  missing_codes <- setdiff(codes_vec, result[["code"]])
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "lookup",
      table_meta = this_meta,
      extra = active_col_filters_hint(this_meta$col_filters, "lookup", type)
    )
  }

  if (preferred_description_only) {
    result <- dplyr::filter(result, .data$preferred_description)
  }

  # Select only standard columns
  result <- dplyr::select(result, dplyr::all_of(codelist_cols()))

  return(as_codelist(result))
}

#' Chunked, bounded "all codes of a type" fetch
#'
#' Bounded variant of `CODES("all", ...)` for callers that need to keep any
#' single call's work small (e.g. to stay under a network-layer request
#' timeout), rather than materialising the whole code type in one unbounded
#' scan. See [DESCRIPTION_CHUNK()] for the equivalent for a description
#' search - this is simpler, since there's no search predicate: each chunk is
#' just the `rowid`-bounded slice of the lookup table, read directly.
#'
#' @inheritParams DESCRIPTION_CHUNK
#' @param preferred_description_only Logical. If `TRUE`, return only the
#'   preferred description for each code.
#'
#' @return A list with `result`, `next_cursor`, `total_rows`, and `exhausted`
#'   - see [DESCRIPTION_CHUNK()] for the shape.
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' create_dummy_database()
#' chunk <- CODES_ALL_CHUNK(type = "ICD-10", batch_size = 100)
#' chunk$result
#' chunk$exhausted
CODES_ALL_CHUNK <- function(
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 200000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()
  assert_chunking_supported()

  this_meta <- get_metadata_for_lookup(con, type, lookup_version)

  # Raw physical row count - see DESCRIPTION_CHUNK() for why this bypasses
  # get_lookup_table()/col_filters.
  if (is.null(total_rows)) {
    total_rows <- dplyr::tbl(con, this_meta$lookup_table_name) |>
      dplyr::tally() |>
      dplyr::collect() |>
      dplyr::pull("n") |>
      as.integer()
  }

  cursor <- as.integer(cursor)
  batch_size <- as.integer(batch_size)
  to <- min(cursor + batch_size, total_rows)

  chunk_tbl <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    con = con,
    meta = this_meta,
    rowid_range = c(cursor, to)
  )
  result <- dplyr::collect(chunk_tbl)

  if (preferred_description_only) {
    result <- dplyr::filter(result, .data$preferred_description)
  }
  result <- dplyr::select(result, dplyr::all_of(codelist_cols()))
  result <- as_codelist(result)

  abort_if_leaf_rows_exceeded(accumulated_so_far + nrow(result), max_rows)

  list(
    result = result,
    next_cursor = to,
    total_rows = total_rows,
    exhausted = to >= total_rows
  )
}

# Argument validation helpers
check_code_type <- function(
  code_type,
  arg = rlang::caller_arg(code_type),
  call = rlang::caller_env()
) {
  if (is.null(code_type)) {
    codeminer_abort(
      c(
        "{.arg {arg}} is required but not provided.",
        "i" = "Either set the option: {.code options(codeminer.code_type = \"ICD-10\")}",
        "i" = "Or provide {.arg {arg}} explicitly in your function call."
      ),
      call = call
    )
  }

  if (!rlang::is_string(code_type)) {
    codeminer_abort(
      "{.arg {arg}} must be a string, not {typeof(code_type)} with length {length(code_type)}",
      call = call
    )
  }
}

check_version <- function(version, call = rlang::caller_env()) {
  version_expr <- rlang::enquo(version)

  # nolint next: object_usage_linter.
  version_name <- rlang::as_label(version_expr)

  if (length(version) != 1) {
    codeminer_abort(
      c(
        "x" = "{.arg {version_name}} must have length 1, not {length(version)}"
      ),
      ,
      call = call
    )
  }
}

check_logical_scalar <- function(arg, arg_name, call = rlang::caller_env()) {
  if (!rlang::is_scalar_logical(arg)) {
    codeminer_abort(
      "{.arg {arg_name}} must be TRUE or FALSE, not {typeof(arg)} with length {length(arg)}",
      call = call
    )
  }
}

check_depth <- function(depth, call = rlang::caller_env()) {
  if (!rlang::is_scalar_integerish(depth) && !identical(depth, Inf)) {
    codeminer_abort(
      "{.arg depth} must be a positive integer or Inf, not {typeof(depth)}",
      call = call
    )
  }
  if (is.finite(depth) && depth < 1) {
    codeminer_abort(
      "{.arg depth} must be at least 1, not {depth}",
      call = call
    )
  }
}


check_pattern <- function(pattern, call = rlang::caller_env()) {
  if (!rlang::is_string(pattern)) {
    codeminer_abort(
      "{.arg pattern} must be a length 1 string, not {typeof(pattern)} with length {length(pattern)}",
      call = call
    )
  }
}

# Confirm `pattern` compiles as a regular expression by asking the backend's own
# engine (RE2), rather than R's, which accepts a different dialect. A table-less
# scalar query is enough to force compilation without scanning anything.
check_pattern_valid_regex <- function(
  pattern,
  con,
  call = rlang::caller_env()
) {
  compile_error <- tryCatch(
    {
      DBI::dbGetQuery(
        con,
        "SELECT regexp_matches('', ?)",
        params = list(pattern)
      )
      NULL
    },
    error = function(cnd) cnd
  )
  if (!is.null(compile_error)) {
    codeminer_abort(
      c(
        "x" = "{.arg pattern} is not a valid regular expression: {.val {pattern}}.",
        "i" = "Searches use regular expressions. To match any text, use {.code .*}."
      ),
      class = "codeminer_invalid_pattern",
      call = call
    )
  }
}

#' @param pattern a regular expression to search for
#'
#' @details `CODES_LIKE` searches for codes that match a given regular
#' expression. The matching is case-insensitive.
#'
#' @export
#' @rdname CODES
#' @examples
#' CODES_LIKE("^E1", type = "ICD-10")
CODES_LIKE <- function(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  check_pattern(pattern)
  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()
  check_pattern_valid_regex(pattern, con)

  lookup_table <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    con = con
  )
  like_codes <- dplyr::filter(
    lookup_table,
    stringr::str_detect(.data$code, pattern)
  ) |>
    dplyr::pull("code")

  result <- CODES(
    unique(like_codes),
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )
  return(result)
}

#' Chunked, bounded CODES_LIKE search
#'
#' Bounded variant of [CODES_LIKE()] for callers that need to keep any single
#' call's work small (e.g. to stay under a network-layer request timeout),
#' rather than materialising every match in one unbounded scan. Same shape as
#' [DESCRIPTION_CHUNK()], matching on `code` instead of `description`.
#'
#' @inheritParams DESCRIPTION_CHUNK
#'
#' @return A list with `result`, `next_cursor`, `total_rows`, and `exhausted`
#'   - see [DESCRIPTION_CHUNK()] for the shape.
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' create_dummy_database()
#' chunk <- CODES_LIKE_CHUNK("^E1", type = "ICD-10", batch_size = 100)
#' chunk$result
#' chunk$exhausted
CODES_LIKE_CHUNK <- function(
  pattern,
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 200000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  max_chunk_matches = getOption(
    "codeminer.max_chunk_matches",
    default = 20000L
  ),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  check_pattern(pattern)
  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()
  check_pattern_valid_regex(pattern, con)
  assert_chunking_supported()

  this_meta <- get_metadata_for_lookup(con, type, lookup_version)

  if (is.null(total_rows)) {
    total_rows <- dplyr::tbl(con, this_meta$lookup_table_name) |>
      dplyr::tally() |>
      dplyr::collect() |>
      dplyr::pull("n") |>
      as.integer()
  }

  cursor <- as.integer(cursor)
  batch_size <- as.integer(batch_size)
  to <- min(cursor + batch_size, total_rows)

  chunk_tbl <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    con = con,
    meta = this_meta,
    rowid_range = c(cursor, to)
  )
  like_codes <- dplyr::filter(
    chunk_tbl,
    stringr::str_detect(.data$code, pattern)
  ) |>
    dplyr::pull("code")
  like_codes <- unique(like_codes)

  abort_if_chunk_match_limit_exceeded(length(like_codes), max_chunk_matches)

  result <- if (length(like_codes) == 0) {
    empty_cols <- stats::setNames(
      replicate(3, character(), simplify = FALSE),
      codelist_cols()
    )
    as_codelist(tibble::as_tibble(empty_cols))
  } else {
    CODES(
      like_codes,
      type = type,
      lookup_version = lookup_version,
      preferred_description_only = preferred_description_only
    )
  }

  abort_if_leaf_rows_exceeded(accumulated_so_far + nrow(result), max_rows)

  list(
    result = result,
    next_cursor = to,
    total_rows = total_rows,
    exhausted = to >= total_rows
  )
}

#' Which of `codes` are present in a code type's lookup table
#'
#' Existence check used to classify codes that are absent from a relationship
#' table: those that *are* in the lookup point to a lookup/relationship version
#' mismatch, while those absent from both are likely invalid codes. Runs without
#' `col_filters`: a code filtered out by a `col_filter` still exists.
#'
#' @param codes Character vector of codes to check.
#' @param code_type The code type.
#' @param lookup_version The lookup version.
#' @param con Database connection.
#' @return The subset of `codes` present in the lookup table.
#' @keywords internal
#' @noRd
codes_present_in_lookup <- function(codes, code_type, lookup_version, con) {
  get_lookup_table(
    code_type,
    codes = codes,
    lookup_version = lookup_version,
    col_filters = NULL,
    con = con
  ) |>
    dplyr::distinct(.data$code) |>
    dplyr::pull(.data$code)
}

#' Get the full lookup table for a code type
#'
#' Returns a lazy `dplyr::tbl()` containing the lookup table with standardised
#' column names (`code`, `description`, `code_type`, `preferred_description`,
#' `category`) plus all additional columns from the underlying database table.
#' Call [dplyr::collect()] to materialise the result.
#'
#' This is useful for inspecting columns beyond the standard codelist output
#' returned by [CODES()] and [DESCRIPTION()].
#'
#' @param type The code type for which to retrieve the lookup table.
#' @param codes Optional character vector of codes to filter the result to.
#'   If `NULL` (default), all rows are returned. No warning is emitted for
#'   codes that are not present in the table; use [CODES()] for missing-code
#'   warnings.
#' @param lookup_version The version to retrieve. Defaults to `"latest"`.
#' @param col_filters Column filters to apply. See [CODES()] for details.
#' @param con Optional DBI connection. If `NULL` (default), uses the
#'   workbench connection.
#' @param meta Optional pre-resolved lookup metadata row (as returned by the
#'   internal metadata resolver). When supplied, the metadata is not resolved
#'   again - callers that have already resolved it pass it through to avoid
#'   repeating the resolution. Defaults to `NULL`, which resolves the metadata
#'   from `type`/`lookup_version`.
#' @param rowid_range Optional length-2 integer vector `c(from, to)` bounding
#'   the scan to `rowid >= from & rowid < to` on the underlying table, applied
#'   before any other filter so DuckDB can row-group-prune rather than
#'   scanning the whole table. Used by the `*_CHUNK()` functions to bound work
#'   per call; `NULL` (default) scans the whole table as before. Only
#'   supported when the "main" database is `duckdb_file`-backed (see
#'   `backend_kind()`) - `rowid` is not addressable through the views the
#'   other two backend shapes use.
#' @param call The calling environment. Passed to [codeminer_abort].
#'
#' @return A lazy `dplyr::tbl()` with standardised columns (`code`,
#'   `description`, `code_type`, `preferred_description`, `category`) plus all
#'   other columns from the underlying table. `category` is `NA` for code
#'   systems without a populated category source.
#' @export
#' @family Clinical code lookups and mappings
#' @seealso [CODES()] for standardised codelist output,
#'   [get_codeminer_metadata()] for discovering available tables.
#' @examples
#' create_dummy_database()
#'
#' # Get the full ICD-10 lookup table
#' get_lookup_table("ICD-10") |> dplyr::collect()
#'
#' # Filter to specific codes
#' get_lookup_table("ICD-10", codes = c("E10", "E11")) |> dplyr::collect()
get_lookup_table <- function(
  type,
  codes = NULL,
  lookup_version = "latest",
  col_filters = "default",
  con = NULL,
  meta = NULL,
  rowid_range = NULL,
  call = rlang::caller_env()
) {
  old_cf <- push_col_filters(col_filters, call = call)
  on.exit(pop_col_filters(old_cf), add = TRUE)

  con <- get_db_con(con)
  this_meta <- if (is.null(meta)) {
    get_metadata_for_lookup(con, type, lookup_version, call)
  } else {
    meta
  }

  tbl_name <- this_meta$lookup_table_name
  tbl <- dplyr::tbl(con, tbl_name)

  if (!is.null(rowid_range)) {
    assert_chunking_supported(call = call)
    from <- as.integer(rowid_range[[1]])
    to <- as.integer(rowid_range[[2]])
    tbl <- dplyr::filter(tbl, .data$rowid >= .env$from, .data$rowid < .env$to)
  }

  # Apply col_filters BEFORE column renaming (filter on original column names)
  resolved <- resolve_col_filters(
    this_meta$col_filters,
    pin_type = "lookup",
    pin_key = type
  )
  tbl <- apply_col_filters(tbl, resolved, tbl_name = tbl_name, call = call)

  # Do every column rename in a single innermost SELECT. dplyr::rename() chained
  # after a mutate forces dbplyr to enumerate columns in the outer projection,
  # which in DuckDB triggers case-folding of unquoted identifiers — so a source
  # column like `CODE` collapses with the `ALT_CODE AS code` alias and tibble
  # rejects the result. Keeping all renames in one SELECT lets the outer
  # wrappers stay as `q01.*`, and the backend auto-disambiguates duplicates
  # with `_1` suffixes.
  # NULL on `lookup_category_col` covers pre-migration databases (the metadata
  # column did not exist before #124).
  category_col <- this_meta$lookup_category_col
  renames <- c(
    code = this_meta$lookup_code_col,
    description = this_meta$lookup_description_col
  )
  if (!is.na(this_meta$preferred_description_col)) {
    renames <- c(
      renames,
      preferred_description = this_meta$preferred_description_col
    )
  }
  if (!is.null(category_col) && !is.na(category_col)) {
    renames <- c(renames, category = category_col)
  }

  check_meta_columns_exist(
    tbl,
    cols = renames,
    tbl_name = tbl_name,
    metadata_type = "lookup",
    call = call
  )
  tbl <- dplyr::select(tbl, !!!renames, dplyr::everything()) |>
    dplyr::mutate(code_type = .env$type)

  if (is.na(this_meta$preferred_description_col)) {
    tbl <- dplyr::mutate(tbl, preferred_description = TRUE)
  }
  if (is.null(category_col) || is.na(category_col)) {
    # `as.character(NA)` is dbplyr-translated to the backend's typed cast
    # (e.g. DuckDB `TRY_CAST(NULL AS TEXT)`) so the column comes back `<chr>`
    # rather than being inferred as integer from an untyped NULL.
    tbl <- dplyr::mutate(tbl, category = as.character(NA))
  }

  if (!is.null(codes)) {
    tbl <- dplyr::filter(tbl, .data$code %in% .env$codes)
  }

  return(tbl)
}

# Chunked reads bound the scan via the `rowid` pseudo-column, which is only
# addressable against a real base table. `codeminer_folder`/`parquet_folder`
# expose data tables as views (see `backend.R`), where `rowid` fails with a
# clean Binder Error rather than silently returning wrong data - so this
# checks the shape upfront and aborts with a clear message instead of letting
# that DuckDB error surface directly.
assert_chunking_supported <- function(call = rlang::caller_env()) {
  main_path <- .codeminer_env$db_paths$main
  if (is.null(main_path)) {
    return(invisible(TRUE))
  }
  kind <- backend_kind(main_path)
  if (!identical(kind, "duckdb_file")) {
    codeminer_abort(
      c(
        "Chunked queries require a {.val duckdb_file}-backed database, not {.val {kind}}.",
        "i" = "The {.arg rowid}-based chunking mechanism only works against a real base table, not the views {.val codeminer_folder}/{.val parquet_folder} expose."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Shared guard for the `*_CHUNK()` functions' row-count ceiling, mirroring
# `get_relationship_tree()`'s `max_codes` pattern (same mechanics, different
# semantic class since this guards a search-result size, not a tree
# expansion).
abort_if_leaf_rows_exceeded <- function(
  n,
  max_rows,
  call = rlang::caller_env()
) {
  if (n > max_rows) {
    codeminer_abort(
      c(
        "Your query has matched {n} rows so far, which exceeds the allowed maximum of {max_rows} rows.",
        "i" = "Try narrowing your search (e.g. a more specific pattern, or extra filters), then run it again."
      ),
      class = "codeminer_max_leaf_rows_exceeded",
      call = call
    )
  }
  invisible(TRUE)
}

# Guards the cost of the CODES() expansion step within a single chunk.
# Matched codes drive CODES()'s cost (confirmed empirically: ~20s for ~18,000
# matched codes), not rows scanned - so a chunk that happens to match densely
# can still risk the network-layer timeout even though its `batch_size` (rows
# scanned) is itself small. This aborts before paying that cost, rather than
# discovering it mid-CODES()-call.
abort_if_chunk_match_limit_exceeded <- function(
  n,
  max_matches,
  call = rlang::caller_env()
) {
  if (n > max_matches) {
    codeminer_abort(
      c(
        "This search matched {n} codes, more than the {max_matches} that can be processed at once.",
        "i" = "Try a more specific search pattern to narrow the results, then run it again."
      ),
      class = "codeminer_chunk_match_limit_exceeded",
      call = call
    )
  }
  invisible(TRUE)
}

get_metadata_for_lookup <- function(
  con,
  code_type,
  lookup_version,
  call = rlang::caller_env()
) {
  meta <- get_lookup_metadata(con = con)
  resolve_versioned_metadata(
    meta,
    code_type_val = code_type,
    version_val = lookup_version,
    version_col = "lookup_version",
    pin_type = "lookup",
    type_label = "lookup",
    add_fun_name = "codeminer::add_lookup_table",
    call = call
  )
}

# NOTE: this helper has a strong assumption that the versions have some numeric component (e.g. "v42")
# that can be extracted and used to sort, in order to get the latest one
# This is is not something we enforce, so this may not always return the correct result.
#
# `type_label` ("mapping"/"lookup"/"relationship") and `key` (the code type, or
# "from > to" pair for mappings) qualify the message so it is clear which table
# the resolved version applies to. A single MAP() call resolves both a mapping
# version and a lookup version, so an unqualified message reads as a confusing
# duplicate. The message is emitted even when only one version exists, so users
# who have not explicitly pinned versions can see which defaults were chosen.
get_latest_version <- function(versions, type_label = NULL, key = NULL) {
  versions_numeric <- as.numeric(stringr::str_extract(versions, "\\d+"))
  if (any(is.na(versions_numeric))) {
    # resort to alphabetic ordering
    latest_version <- max(versions)
  } else {
    latest_version <- versions[which.max(versions_numeric)]
  }
  if (!is.null(type_label) && !is.null(key)) {
    codeminer_inform(c(
      "i" = "Using {.val {latest_version}} as the latest {type_label} version for {.val {key}}."
    ))
  } else {
    codeminer_inform(c("i" = "Using {.val {latest_version}} as latest version"))
  }
  return(latest_version)
}
