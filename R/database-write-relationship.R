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

  table_name <- metadata$relationship_table_name
  if (length(table_name) != 1) {
    codeminer_abort(
      "`metadata$relationship_table_name` must have length 1, not {length(table_name)}."
    )
  }

  table <- as.data.frame(table)
  metadata <- as.data.frame(metadata)

  con <- connect_to_db(read_only = FALSE)
  check_database(con)

  meta_added <- add_metadata_table(con, metadata, type = "relationship")
  if (!meta_added) {
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

  success <- DBI::dbWriteTable(
    con,
    name = table_name,
    value = table,
    overwrite = FALSE
  )
  if (success) {
    codeminer_inform(
      c(
        "v" = "Relationship table {.field {metadata$relationship_table_name}} added successfully."
      )
    )
  }
  return(invisible(success))
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
  table_name <- paste(code_type, "relationship", relationship_version, sep = "_")

  con <- connect_to_db(read_only = FALSE)
  check_database(con)

  meta <- read_table_from_db(con, codeminer_metadata_table_names$relationship)
  if (!table_name %in% meta$relationship_table_name) {
    codeminer_abort(
      "No relationship table found for {.val {code_type}} version {.val {relationship_version}}."
    )
  }

  remove_table_entry(con, "relationship", table_name)
  codeminer_inform(c(
    "v" = "Relationship table {.field {table_name}} removed."
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
#' @param type_col The column name for the relationship type (default: "type")
#' @param child_parent_relationship_code The code value that indicates a
#'   child-parent (is-a) relationship in the `type_col` column (default: "is a")
#' @param relationship_source The source of the relationship metadata (default: `NA_character_`)
#'
#' @return A list containing the relationship metadata
#'
#' @seealso [add_relationship_table()]
#' @export
#' @examples
#' relationship_metadata("SNOMED-CT", relationship_version = "2023")
relationship_metadata <- function(
  code_type,
  relationship_version = "v0",
  ...,
  from_col = "from",
  to_col = "to",
  type_col = "type",
  child_parent_relationship_code = "is a",
  relationship_source = NA_character_
) {
  rlang::check_dots_empty()

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
    relationship_source = relationship_source
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

  return(invisible(metadata))
}
