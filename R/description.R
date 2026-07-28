#' Search for codes that match a description
#'
#' Returns a codelist with clinical codes that match the provided description
#' pattern.
#'
#' @param pattern The description to search for. See [stringr::str_detect()] for
#'   details.
#' @param ignore_case If `TRUE` (default), ignore case in `description`.
#' @param preferred_description_only `logical`. If `TRUE` (default), return only
#'   preferred descriptions.
#' @param col_filters Column filters to apply. See [CODES()] for details.
#' @inheritParams CODES
#'
#' @return A `codeminer_codelist` with codes that match the description.
#'
#' @export
#' @examples
#' # build dummy database
#' create_dummy_database()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' DESCRIPTION("cyst", type = "ICD-10")
DESCRIPTION <- function(
  pattern,
  type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  check_pattern(pattern)
  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(ignore_case, "ignore_case")
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()
  check_pattern_valid_regex(pattern, con)

  lkp_table <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    con = con
  )
  code_col <- "code"
  description_col <- "description"

  # Note - it isn't possible to specify `ignore_case` when using dbplyr, so use `tolower()`
  if (ignore_case) {
    filtered <- dplyr::filter(
      lkp_table,
      stringr::str_detect(
        string = tolower(.data[[description_col]]),
        pattern = tolower(pattern)
      )
    )
  } else {
    filtered <- dplyr::filter(
      lkp_table,
      stringr::str_detect(
        string = .data[[description_col]],
        pattern = pattern
      )
    )
  }
  filtered <- dplyr::collect(filtered)

  ## then expand to include both primary and secondary descriptions
  codes <- unique(filtered[[code_col]])
  codes <- codes[!is.na(codes)]

  result <- CODES(
    codes,
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  return(result)
}

#' Chunked, bounded DESCRIPTION search
#'
#' Bounded variant of [DESCRIPTION()] for callers that need to keep any single
#' call's work small (e.g. to stay under a network-layer request timeout),
#' rather than materialising every match in one unbounded scan.
#'
#' Each call scans only the `rowid` range `[cursor, cursor + batch_size)` of
#' the underlying lookup table for matches, expands those matches to full
#' rows via [CODES()], and returns a cursor for the next chunk. Callers
#' should keep calling with the returned `next_cursor` (and the same
#' `total_rows` and `accumulated_so_far`, updated each time) until
#' `exhausted` is `TRUE`.
#'
#' @inheritParams DESCRIPTION
#' @param cursor Integer. `rowid` to start this chunk's scan from. `0` for
#'   the first call.
#' @param batch_size Integer. Number of `rowid`s to scan in this call.
#'   Defaults to `getOption("codeminer.chunk_batch_size", default = 2000)`.
#' @param total_rows Integer or `NULL`. The underlying table's total row
#'   count. Pass `NULL` on the first call (it will be resolved and returned);
#'   pass the previously-returned value on subsequent calls to skip
#'   re-resolving it.
#' @param accumulated_so_far Integer. Total matched rows found by this leaf's
#'   previous chunks (before this one). `0` for the first call. Used to check
#'   `max_rows` cumulatively across the whole chunked fetch, not just this one
#'   call.
#' @param max_rows Integer. Ceiling on this leaf's cumulative matched rows
#'   across all chunks. Aborts with class `codeminer_max_leaf_rows_exceeded`
#'   if exceeded. Defaults to
#'   `getOption("codeminer.max_leaf_rows", default = 30000)`.
#'
#' @return A list with `result` (a `codeminer_codelist` of this chunk's
#'   matches), `next_cursor` (integer, pass back as `cursor` next call),
#'   `total_rows` (integer, pass back as `total_rows` next call), and
#'   `exhausted` (logical — `TRUE` once `next_cursor` has covered the whole
#'   table).
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' create_dummy_database()
#' chunk <- DESCRIPTION_CHUNK("cyst", type = "ICD-10", batch_size = 100)
#' chunk$result
#' chunk$exhausted
DESCRIPTION_CHUNK <- function(
  pattern,
  type = getOption("codeminer.code_type"),
  cursor = 0L,
  batch_size = getOption("codeminer.chunk_batch_size", default = 2000L),
  total_rows = NULL,
  accumulated_so_far = 0L,
  max_rows = getOption("codeminer.max_leaf_rows", default = 30000L),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  old_cf <- push_col_filters(col_filters, call = rlang::current_env())
  on.exit(pop_col_filters(old_cf), add = TRUE)

  check_pattern(pattern)
  check_code_type(type)
  check_version(lookup_version)
  check_logical_scalar(ignore_case, "ignore_case")
  check_logical_scalar(preferred_description_only, "preferred_description_only")

  con <- get_db_con()
  check_pattern_valid_regex(pattern, con)
  assert_chunking_supported()

  this_meta <- get_metadata_for_lookup(con, type, lookup_version)

  # Raw physical row count of the base table - deliberately NOT run through
  # get_lookup_table()/col_filters, since the rowid cursor has to cover the
  # whole physical table regardless of which rows col_filters would exclude.
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

  description_col <- "description"
  filtered <- if (ignore_case) {
    dplyr::filter(
      chunk_tbl,
      stringr::str_detect(
        string = tolower(.data[[description_col]]),
        pattern = tolower(pattern)
      )
    )
  } else {
    dplyr::filter(
      chunk_tbl,
      stringr::str_detect(
        string = .data[[description_col]],
        pattern = pattern
      )
    )
  }
  filtered <- dplyr::collect(filtered)

  codes <- unique(filtered[["code"]])
  codes <- codes[!is.na(codes)]

  result <- if (length(codes) == 0) {
    empty_cols <- stats::setNames(
      replicate(3, character(), simplify = FALSE),
      codelist_cols()
    )
    as_codelist(tibble::as_tibble(empty_cols))
  } else {
    CODES(
      codes,
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
