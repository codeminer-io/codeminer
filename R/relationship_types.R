#' Relationship types
#'
#' Return relationship types as a `codeminer_codelist`, with each
#' type's description looked up from the lookup table where available.
#'
#' - `RELATIONSHIP_TYPES_FROM()` - types originating from the supplied codes.
#' - `RELATIONSHIP_TYPES_TO()` - types pointing to the supplied codes.
#' - `RELATIONSHIP_TYPES()` - all types in a code type's relationship table,
#'   optionally filtered by a description `pattern` (like [DESCRIPTION()]).
#'
#' Relationship types are themselves codes (e.g. a SNOMED CT relationship type
#' is a concept id), so they are described via the lookup table. A type with no
#' matching lookup entry falls back to using its own value as the description.
#'
#' These functions are not applicable to purely hierarchical relationship
#' tables (those with no type column); they error in that case. Use [CHILDREN()]
#' / [PARENTS()] to traverse a hierarchy.
#'
#' @inheritParams CHILDREN
#' @param pattern Optional description pattern to filter the relationship types
#'   by. See [stringr::str_detect()] for details. `NULL` (default) returns all
#'   types.
#' @param ignore_case If `TRUE` (default), ignore case when matching `pattern`.
#' @return A `codeminer_codelist` of relationship types with their
#'   descriptions.
#' @family Code relationships
#' @name relationship_types
#' @examples
#' create_dummy_database()
#' # RELATIONSHIP_TYPES_FROM() returns types originating from codes
#' # RELATIONSHIP_TYPES_TO() returns types pointing to codes
#' # RELATIONSHIP_TYPES() lists / searches all types for a code type
NULL

#' An empty `codeminer_codelist`
#' @return A zero-row `codeminer_codelist`.
#' @keywords internal
#' @noRd
empty_codelist <- function() {
  as_codelist(tibble::tibble(
    code = character(),
    description = character(),
    code_type = character()
  ))
}

#' Describe relationship-type codes via the lookup table
#'
#' Looks up descriptions for a set of relationship-type values. Reuses [CODES()]
#' (so real descriptions are returned where the type is a code in the lookup),
#' but a type absent from the lookup is expected for non-ontology systems, so
#' the "not found" warning is muffled and such types fall back to using their
#' own value as the description.
#'
#' @param type_values Character vector of distinct relationship-type values.
#' @param type The code type.
#' @param lookup_version The lookup version.
#' @param preferred_description_only Passed to [CODES()].
#' @param col_filters Column filters applied when looking up descriptions (the
#'   caller's `col_filters`). Honouring these keeps the description to one row
#'   per type (e.g. SNOMED's default `active_description` filter), instead of
#'   surfacing inactive descriptions as duplicate rows.
#' @return A `codeminer_codelist`.
#' @keywords internal
#' @noRd
describe_relationship_types <- function(
  type_values,
  type,
  lookup_version,
  preferred_description_only,
  col_filters
) {
  if (length(type_values) == 0) {
    return(empty_codelist())
  }

  # A relationship type that is not a code in the lookup is expected (only
  # ontologies like SNOMED CT model types as concepts), so muffle the otherwise
  # confusing "not found in the lookup table" warning and fall back below.
  described <- withCallingHandlers(
    CODES(
      type_values,
      type = type,
      lookup_version = lookup_version,
      preferred_description_only = preferred_description_only,
      col_filters = col_filters
    ),
    codeminer_missing_codes = function(w) invokeRestart("muffleWarning")
  )

  missing <- setdiff(type_values, described$code)
  if (length(missing) > 0) {
    described <- as_codelist(dplyr::bind_rows(
      tibble::as_tibble(described),
      tibble::tibble(
        code = missing,
        description = missing,
        code_type = type
      )
    ))
  }

  described
}

#' Distinct relationship types for codes, in one direction
#'
#' Shared engine for [RELATIONSHIP_TYPES_FROM()] / [RELATIONSHIP_TYPES_TO()].
#'
#' @param codes_vec Character vector of codes.
#' @param type The code type.
#' @param direction `"from"` (codes in the `from` column) or `"to"`.
#' @param relationship_version,lookup_version Versions to use.
#' @param preferred_description_only Passed to [CODES()].
#' @param col_filters Column filters applied to the relationship table.
#' @param fn Calling function name, for the not-applicable guard.
#' @param empty_warning Warning when no types are found.
#' @param call Calling environment.
#' @return A `codeminer_codelist`.
#' @keywords internal
#' @noRd
relationship_types_for_codes <- function(
  codes_vec,
  type,
  direction,
  relationship_version,
  lookup_version,
  preferred_description_only,
  col_filters,
  fn,
  empty_warning,
  call = rlang::caller_env()
) {
  check_code_type(type, call = call)
  check_version(relationship_version, call = call)
  check_version(lookup_version, call = call)

  con <- get_db_con()
  meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call = call
  )
  abort_if_no_relationship_types(meta, fn = fn, call = call)

  rel_table <- dplyr::tbl(con, meta$relationship_table_name)
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  rel_table <- apply_col_filters(
    rel_table,
    resolved,
    tbl_name = meta$relationship_table_name,
    call = call
  )

  filter_col <- if (direction == "from") meta$from_col else meta$to_col
  matched <- dplyr::filter(rel_table, .data[[filter_col]] %in% .env$codes_vec)

  type_values <- matched |>
    dplyr::select(dplyr::all_of(meta$type_col)) |>
    dplyr::distinct() |>
    dplyr::pull()

  available_codes <- matched |>
    dplyr::select(dplyr::all_of(filter_col)) |>
    dplyr::distinct() |>
    dplyr::pull()

  missing_codes <- setdiff(codes_vec, available_codes)
  if (length(missing_codes) > 0) {
    missing_codes_warning(
      missing_codes,
      table_type = "relationship",
      table_meta = meta
    )
  }

  if (length(type_values) == 0) {
    codeminer_warn(empty_warning)
    return(empty_codelist())
  }

  describe_relationship_types(
    type_values,
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )
}

#' @rdname relationship_types
#' @export
RELATIONSHIP_TYPES_FROM <- function(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  collected <- collect_codes_input(..., type = type)
  codes_vec <- collected$codes
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  relationship_types_for_codes(
    codes_vec,
    type = type,
    direction = "from",
    relationship_version = relationship_version,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters,
    fn = "RELATIONSHIP_TYPES_FROM",
    empty_warning = "No relationship types found originating from the specified codes."
  )
}

#' @rdname relationship_types
#' @export
RELATIONSHIP_TYPES_TO <- function(
  ...,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  collected <- collect_codes_input(..., type = type)
  codes_vec <- collected$codes
  if (!is.null(collected$code_type)) {
    type <- collected$code_type
  }

  relationship_types_for_codes(
    codes_vec,
    type = type,
    direction = "to",
    relationship_version = relationship_version,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters,
    fn = "RELATIONSHIP_TYPES_TO",
    empty_warning = "No relationship types found pointing to the specified codes."
  )
}

#' @rdname relationship_types
#' @export
RELATIONSHIP_TYPES <- function(
  pattern = NULL,
  type = getOption("codeminer.code_type"),
  relationship_version = getOption(
    "codeminer.relationship_version",
    default = "latest"
  ),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  preferred_description_only = TRUE,
  col_filters = "default"
) {
  check_code_type(type)
  check_version(relationship_version)
  check_version(lookup_version)
  check_logical_scalar(ignore_case, "ignore_case")
  check_logical_scalar(preferred_description_only, "preferred_description_only")
  if (!is.null(pattern)) {
    check_pattern(pattern)
  }

  con <- get_db_con()
  meta <- get_metadata_for_relationship(
    con,
    type,
    relationship_version,
    call = rlang::caller_env()
  )
  abort_if_no_relationship_types(meta, fn = "RELATIONSHIP_TYPES")

  rel_table <- dplyr::tbl(con, meta$relationship_table_name)
  resolved <- resolve_col_filters(
    col_filters,
    meta$col_filters,
    pin_type = "relationship",
    pin_key = type
  )
  rel_table <- apply_col_filters(
    rel_table,
    resolved,
    tbl_name = meta$relationship_table_name
  )

  # The set of relationship types = the distinct values of the type column.
  type_values <- rel_table |>
    dplyr::select(dplyr::all_of(meta$type_col)) |>
    dplyr::distinct() |>
    dplyr::pull()

  if (length(type_values) == 0) {
    codeminer_warn("No relationship types found for {.val {type}}.")
    return(empty_codelist())
  }

  if (is.null(pattern)) {
    return(describe_relationship_types(
      type_values,
      type = type,
      lookup_version = lookup_version,
      preferred_description_only = preferred_description_only,
      col_filters = col_filters
    ))
  }

  # With a pattern, restrict to the type codes whose lookup description matches
  # (synonyms included, like DESCRIPTION()) - a single-table lookup query
  # combined in R, never an in-SQL join across the relationship and lookup
  # tables (each may live in a separate backend file).
  lkp <- get_lookup_table(
    type,
    lookup_version = lookup_version,
    col_filters = col_filters,
    con = con
  )
  lkp <- dplyr::filter(lkp, .data$code %in% .env$type_values)
  lkp <- if (ignore_case) {
    dplyr::filter(
      lkp,
      stringr::str_detect(tolower(.data$description), tolower(.env$pattern))
    )
  } else {
    dplyr::filter(lkp, stringr::str_detect(.data$description, .env$pattern))
  }
  matched_codes <- lkp |>
    dplyr::distinct(.data$code) |>
    dplyr::pull(.data$code)

  describe_relationship_types(
    matched_codes,
    type = type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )
}
