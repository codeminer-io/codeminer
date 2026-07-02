#' Add a relationship table to the database
#'
#' Add a relationship table to the database together with its metadata. Note
#' that it is not possible to overwrite an existing relationship table.
#'
#' Relationship tables are indexed by their `code_type` and
#' `relationship_version`, specified in [relationship_metadata()]. This index
#' needs to be unique and is used to identify the relationship table in the
#' database. If a relationship table with the same `code_type` and
#' `relationship_version` already exists, the function will emit a warning and
#' return `FALSE` (invisibly) without any effect. Use a different
#' `relationship_version` to add a new version of the relationship table for the
#' given `code_type`.
#'
#' @param table The relationship table to add, should be coercible to a
#'   `data.frame`
#' @param metadata The relationship metadata, as specified by
#'   [relationship_metadata()].
#'
#' @return `TRUE` invisibly if successful, `FALSE` invisibly if the relationship
#'   table already exists.
#'
#' @seealso [relationship_metadata()] for the specification of the metadata.
#' @export
#' @examples
#' relationship_table <- data.frame(
#'   source = c("A", "B", "C"),
#'   target = c("B", "C", "D"),
#'   type = "child"
#' )
#' relationship_table
#'
#' # Using a temporary database
#' Sys.setenv(CODEMINER_DB_PATH = tempfile())
#' build_database()
#' add_relationship_table(
#'   relationship_table,
#'   relationship_metadata(
#'     "test",
#'     relationship_version = "v1",
#'     from_col = "source",
#'     to_col = "target",
#'     type_col = "type",
#'     child_parent_relationship_code = "child"
#'   )
#' )
add_relationship_table <- function(table, metadata) {
  validate_relationship_metadata(metadata, arg = rlang::caller_arg(metadata))

  # Validate col_filters column names exist in the data table
  cf <- deserialise_col_filters(metadata$col_filters)
  validate_col_filters_columns(
    cf,
    table_cols = names(as.data.frame(table)),
    table_name = metadata$relationship_table_name
  )

  table_name <- metadata$relationship_table_name
  if (length(table_name) != 1) {
    codeminer_abort(
      "`metadata$relationship_table_name` must have length 1, not {length(table_name)}."
    )
  }

  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  target_path <- db_path()
  check_database(target_path)
  acquire_writable_workbench(target_path)

  added <- backend_add_table(
    target_path,
    type = "relationship",
    metadata_row = metadata,
    data_df = table
  )
  if (isFALSE(added)) {
    codeminer_warn(
      c(
        "!" = paste0(
          "The relationship table ",
          "{.field {metadata$relationship_table_name}} already exists."
        ),
        "i" = paste0(
          "Use a different {.arg code_type} or ",
          "{.arg relationship_version} in {.arg metadata} to add a ",
          "new relationship table."
        )
      )
    )
    return(invisible(FALSE))
  }

  codeminer_inform(
    c(
      "v" = "Relationship table {.field {metadata$relationship_table_name}} added successfully."
    )
  )
  cached <- .codeminer_env$active_versions[["relationship"]][[
    metadata$code_type
  ]]
  if (
    !is.null(cached) &&
      cached != metadata$relationship_version
  ) {
    codeminer_inform(c(
      "i" = paste0(
        "Currently using version {.val {cached}} for ",
        "{.val {metadata$code_type}} relationships."
      ),
      "i" = paste0(
        "Use {.fun codeminer_clear_versions} or ",
        "{.fun codeminer_set_version} to switch."
      )
    ))
  }
  return(invisible(TRUE))
}

#' Remove a relationship table from the database
#'
#' Removes a relationship table and its metadata entry from the database.
#'
#' @param code_type The coding system type (e.g. `"ICD-10"`).
#' @param relationship_version The version to remove (e.g. `"UKB v4"`).
#'
#' @return `TRUE` invisibly if successful.
#'
#' @seealso [add_relationship_table()], [relationship_metadata()]
#' @export
remove_relationship_table <- function(code_type, relationship_version) {
  table_name <- paste(
    code_type,
    "relationship",
    relationship_version,
    sep = "_"
  )

  target_path <- db_path()
  check_database(target_path)
  acquire_writable_workbench(target_path)

  meta <- backend_read_metadata(target_path, "relationship")
  if (!table_name %in% meta$relationship_table_name) {
    codeminer_abort(
      "No relationship table found for {.val {code_type}} version {.val {relationship_version}}."
    )
  }

  backend_remove_table(target_path, "relationship", table_name)
  cached <- .codeminer_env$active_versions[["relationship"]][[
    code_type
  ]]
  if (!is.null(cached) && cached == relationship_version) {
    .codeminer_env$active_versions[["relationship"]][[
      code_type
    ]] <- NULL
  }
  codeminer_inform(c(
    "v" = "Relationship table {.field {table_name}} removed."
  ))
  invisible(TRUE)
}

#' Update relationship table metadata
#'
#' Updates metadata fields for an existing relationship table without re-adding
#' the data. Currently supports updating `col_filters`.
#'
#' @param code_type The coding system type (e.g. `"SNOMED CT"`).
#' @param relationship_version The version to update. Use `"latest"` (default)
#'   to update the most recent version.
#' @inheritParams rlang::args_dots_empty
#' @param col_filters Column filter specification to set. See
#'   [relationship_metadata()] for the format. Use `NULL` to clear existing
#'   filters.
#'
#' @return `TRUE` invisibly if successful.
#' @export
#' @family Database management
#' @seealso [relationship_metadata()], [add_relationship_table()]
update_relationship_metadata <- function(
  code_type,
  relationship_version = "latest",
  ...,
  col_filters = NULL
) {
  rlang::check_dots_empty()

  target_path <- db_path()
  check_database(target_path)

  meta <- backend_read_metadata(target_path, "relationship")
  resolved <- resolve_versioned_metadata(
    meta,
    code_type_val = code_type,
    version_val = relationship_version,
    version_col = "relationship_version",
    pin_type = "relationship",
    type_label = "relationship",
    add_fun_name = "codeminer::add_relationship_table"
  )
  table_name <- resolved$relationship_table_name

  cf_json <- serialise_col_filters(col_filters)
  if (!is.na(cf_json)) {
    table_cols <- DBI::dbListFields(get_db_con(), table_name)
    validate_col_filters_columns(
      col_filters,
      table_cols = table_cols,
      table_name = table_name
    )
  }

  acquire_writable_workbench(target_path)
  backend_update_metadata(
    target_path,
    "relationship",
    table_name = table_name,
    col = "col_filters",
    value = cf_json
  )

  codeminer_inform(c(
    "v" = "Updated metadata for relationship table {.field {table_name}}."
  ))
  invisible(TRUE)
}

#' Create relationship metadata
#'
#' Generate the required metadata for a relationship table. This is mainly used
#' to generate the necessary metadata when adding a new relationship table to
#' the database with [add_relationship_table()].
#'
#' @param code_type The type of coding system (e.g., ICD-10, SNOMED-CT)
#' @param relationship_version The version of the relationship metadata (default: "v0")
#' @inheritParams rlang::args_dots_empty
#' @param from_col The column name for the source code in the relationship (default: "from")
#' @param to_col The column name for the target code in the relationship (default: "to")
#' @param type_col The column name for the relationship type, or `NA` (default)
#'   when the relationship table is purely hierarchical (every edge is
#'   child-parent, so there is no type column and no type filtering).
#' @param child_parent_relationship_code The value in `type_col` that indicates a
#'   child-parent (is-a) relationship, or `NA` (default) for a purely
#'   hierarchical table. Must be `NA` if and only if `type_col` is `NA`.
#' @param relationship_source The source of the relationship metadata (default: `NA_character_`)
#' @param col_filters Optional column filter specification. A named list where
#'   each element is a list with `values` (all valid values) and `defaults`
#'   (default filter values), plus the optional `description` (single string)
#'   and `value_labels` (named character vector, names a subset of `values`)
#'   documentation fields. See [lookup_metadata()] for the full format. `NULL`
#'   (default) means no column filters.
#'
#' @return A list containing the relationship metadata
#'
#' @seealso [add_relationship_table()]
#' @export
#' @examples
#' # Purely hierarchical table (no type column): `type_col` and
#' # `child_parent_relationship_code` both default to `NA`.
#' relationship_metadata("ICD-10", relationship_version = "2023")
#'
#' # Multi-type table: name the type column and the value selecting is-a edges.
#' relationship_metadata(
#'   "SNOMED-CT",
#'   relationship_version = "2023",
#'   type_col = "typeId",
#'   child_parent_relationship_code = "116680003"
#' )
relationship_metadata <- function(
  code_type,
  relationship_version = "v0",
  ...,
  from_col = "from",
  to_col = "to",
  type_col = NA_character_,
  child_parent_relationship_code = NA_character_,
  relationship_source = NA_character_,
  col_filters = NULL
) {
  rlang::check_dots_empty()
  validate_type_col_pairing(type_col, child_parent_relationship_code)

  relationship_table_name <- paste(
    code_type,
    "relationship",
    relationship_version,
    sep = "_"
  )
  return(list(
    relationship_table_name = relationship_table_name,
    code_type = code_type,
    relationship_version = relationship_version,
    from_col = from_col,
    to_col = to_col,
    type_col = type_col,
    child_parent_relationship_code = child_parent_relationship_code,
    relationship_source = relationship_source,
    col_filters = serialise_col_filters(col_filters)
  ))
}

#' Validate relationship metadata
#'
#' @param metadata A list containing the relationship metadata.
#' @param arg The argument name. Used to construct error message.
#' @param call The calling environment. Used to construct error message.
#' @return A logical value, invisibly, indicating whether the metadata is valid.
#' @keywords internal
#' @noRd
validate_relationship_metadata <- function(
  metadata,
  arg = rlang::caller_arg(metadata),
  call = rlang::caller_env()
) {
  required <- required_relationship_metadata_columns()
  missing <- setdiff(required, names(metadata))

  if (length(missing) > 0) {
    codeminer_abort(
      c(
        "The metadata in {.arg {arg}} is incomplete.",
        "x" = "The following entries are missing: {.field {missing}}",
        "i" = "Use {.fun codeminer::relationship_metadata} to construct valid metadata."
      ),
      call = call
    )
  }

  validate_type_col_pairing(
    metadata$type_col,
    metadata$child_parent_relationship_code,
    call = call
  )

  return(invisible(metadata))
}

#' Validate the `type_col` / `child_parent_relationship_code` pairing
#'
#' A relationship table either has a type column (so both `type_col` and the
#' `child_parent_relationship_code` that selects hierarchical edges within it are
#' set) or is purely hierarchical (no type column, so both are `NA`). One set and
#' the other `NA` is contradictory.
#'
#' @param type_col The relationship type column name, or `NA`.
#' @param child_parent_relationship_code The hierarchical type value, or `NA`.
#' @param call The calling environment. Used to construct error message.
#' @return `TRUE` invisibly if valid; otherwise aborts.
#' @keywords internal
#' @noRd
validate_type_col_pairing <- function(
  type_col,
  child_parent_relationship_code,
  call = rlang::caller_env()
) {
  type_col_na <- length(type_col) == 0 || is.na(type_col)
  cpr_na <- length(child_parent_relationship_code) == 0 ||
    is.na(child_parent_relationship_code)

  if (type_col_na != cpr_na) {
    codeminer_abort(
      c(
        paste0(
          "{.arg type_col} and {.arg child_parent_relationship_code} ",
          "must both be set or both be {.code NA}."
        ),
        "x" = paste0(
          "Got {.arg type_col} = {.val {type_col}} and ",
          "{.arg child_parent_relationship_code} = ",
          "{.val {child_parent_relationship_code}}."
        ),
        "i" = paste0(
          "Use {.code NA} for both when a relationship table is purely ",
          "hierarchical (child-parent only, with no type column)."
        )
      ),
      call = call
    )
  }

  return(invisible(TRUE))
}
