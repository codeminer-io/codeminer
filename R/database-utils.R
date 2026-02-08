#' Get codeminer metadata
#'
#' Returns metadata about the lookup, mapping, and relationship tables in the
#' codeminer database.
#'
#' @param type The type of metadata to return. By default returns a list
#'   containing all metadata types. Otherwise, returns a data frame for the
#'   specified type. Must be one of "lookup", "mapping" or "relationship".
#' @param con Optional DBI connection. If `NULL` (default), uses the
#'   workbench connection.
#' @return If a single type is requested, a data frame. If multiple types are
#'   requested, a named list of data frames.
#' @export
#' @examples
#' create_dummy_database()
#' get_codeminer_metadata()
#' get_codeminer_metadata("lookup")
#' get_codeminer_metadata(c("lookup", "mapping"))
get_codeminer_metadata <- function(
  type = c("lookup", "mapping", "relationship"),
  con = NULL
) {
  type <- rlang::arg_match(
    type,
    sort(names(codeminer_metadata_table_names)),
    multiple = TRUE
  )

  con <- get_db_con(con)

  result <- list()
  if ("lookup" %in% type) {
    result$lookup <- get_lookup_metadata(con)
  }
  if ("mapping" %in% type) {
    result$mapping <- get_mapping_metadata(con)
  }
  if ("relationship" %in% type) {
    result$relationship <- get_relationship_metadata(con)
  }

  if (length(result) == 1L) {
    return(result[[1L]])
  }
  result
}

# Return the lookup metadata table as a data frame.
# Uses cached metadata from the workbench when available.
#' @param con A database connection object. If `NULL`, uses the workbench cache.
#' @return A data frame containing the lookup metadata.
#' @noRd
#' @keywords internal
get_lookup_metadata <- function(con = NULL) {
  if (is.null(con) && !is.null(.codeminer_env$metadata$lookup)) {
    return(.codeminer_env$metadata$lookup)
  }
  con <- get_db_con(con)
  tbl_name <- codeminer_metadata_table_names$lookup
  read_table_from_db(con, tbl_name)
}

# Return the mapping metadata table as a data frame.
# Uses cached metadata from the workbench when available.
#' @param con A database connection object. If `NULL`, uses the workbench cache.
#' @return A data frame containing the mapping metadata.
#' @noRd
#' @keywords internal
get_mapping_metadata <- function(con = NULL) {
  if (is.null(con) && !is.null(.codeminer_env$metadata$mapping)) {
    return(.codeminer_env$metadata$mapping)
  }
  con <- get_db_con(con)
  tbl_name <- codeminer_metadata_table_names$mapping
  read_table_from_db(con, tbl_name)
}

# Return the relationship metadata table as a data frame.
# Uses cached metadata from the workbench when available.
#' @param con A database connection object. If `NULL`, uses the workbench cache.
#' @return A data frame containing the relationship metadata.
#' @noRd
#' @keywords internal
get_relationship_metadata <- function(con = NULL) {
  if (is.null(con) && !is.null(.codeminer_env$metadata$relationship)) {
    return(.codeminer_env$metadata$relationship)
  }
  con <- get_db_con(con)
  tbl_name <- codeminer_metadata_table_names$relationship
  read_table_from_db(con, tbl_name)
}

#' Resolve a single-row metadata entry by code_type + version
#'
#' Shared helper for `get_metadata_for_lookup()`, `get_metadata_for_mapping()`,
#' and `get_metadata_for_relationship()`. Handles "latest" resolution (with
#' pinned version override via `.codeminer_env$active_versions`) and error
#' messaging.
#'
#' @param meta Data frame of metadata rows.
#' @param code_type_val Value to match in `code_type_col`.
#' @param version_val Version string (or "latest").
#' @param code_type_col Column name containing the code type.
#' @param version_col Column name containing the version.
#' @param pin_type Key into `.codeminer_env$active_versions` for pinned versions
#'   (one of "lookup", "relationship", "mapping", or NULL to skip).
#' @param pin_key Key to look up in the pinned versions list. Defaults to
#'   `code_type_val`.
#' @param type_label Human-readable label for error messages (e.g. "lookup").
#' @param add_fun_name Qualified function name for the "did you add..." hint.
#' @param call Calling environment for error messages.
#' @return A single-row data frame.
#' @noRd
#' @keywords internal
resolve_versioned_metadata <- function(
  meta,
  code_type_val,
  version_val,
  code_type_col = "code_type",
  version_col,
  pin_type = NULL,
  pin_key = code_type_val,
  type_label = pin_type,
  add_fun_name = NULL,
  call = rlang::caller_env()
) {
  # Check code_type exists
  if (!(code_type_val %in% meta[[code_type_col]])) {
    hint <- if (!is.null(add_fun_name)) {
      c("i" = "Did you add the {type_label} table with {.fun {add_fun_name}}?")
    }
    codeminer_abort(
      c(
        "Code type '{code_type_val}' not found in {type_label} metadata.",
        hint
      ),
      call = call
    )
  }

  # Filter to code_type
  filtered <- meta[meta[[code_type_col]] == code_type_val, ]

  # Resolve "latest" — check pinned version first
  if (identical(version_val, "latest")) {
    pinned <- .codeminer_env$active_versions[[pin_type]][[pin_key]]
    if (!is.null(pinned)) {
      version_val <- pinned
    } else {
      version_val <- get_latest_version(filtered[[version_col]])
    }
  }

  # Filter to version
  this_meta <- filtered[filtered[[version_col]] == version_val, ]

  if (nrow(this_meta) == 0) {
    codeminer_abort(
      "No {type_label} metadata found for '{code_type_val}' version '{version_val}'",
      call = call
    )
  }
  stopifnot(nrow(this_meta) == 1)
  return(this_meta)
}

# Helper to read a table from the database as a data.frame
read_table_from_db <- function(con, tbl_name) {
  DBI::dbReadTable(con, tbl_name)
}

table_exists <- function(con, tbl_name) {
  existing_tables <- DBI::dbListTables(con)
  return(tbl_name %in% existing_tables)
}

# Helper to check if the database is valid or throw an error
check_database <- function(con) {
  error_msg <- c(
    "The database is not initialised.",
    "i" = "You may need to build the database first with {.fun codeminer::build_database}"
  )
  has_lookup_meta <- table_exists(con, codeminer_metadata_table_names$lookup)
  has_mapping_meta <- table_exists(con, codeminer_metadata_table_names$mapping)
  has_relationship_meta <- table_exists(
    con,
    codeminer_metadata_table_names$relationship
  )

  if (!has_lookup_meta) {
    codeminer_abort(c(
      error_msg,
      "x" = "The lookup metadata table does not exist in the database."
    ))
  }
  if (!has_mapping_meta) {
    codeminer_abort(c(
      error_msg,
      "x" = "The mapping metadata table does not exist in the database."
    ))
  }
  if (!has_relationship_meta) {
    codeminer_abort(c(
      error_msg,
      "x" = "The relationship metadata table does not exist in the database."
    ))
  }
  return(invisible(TRUE))
}

#' Add metadata to the database
#'
#' @param con A database connection object.
#' @param metadata A data frame containing the metadata to be added.
#' @param type The type of metadata to be added. Can be one of "lookup", "mapping", or "relationship".
#' @return If successful, returns the number of rows added to the metadata
#'   table, invisibly. If not, returns `FALSE` invisibly.
#' @noRd
#' @keywords internal
add_metadata_table <- function(
  con,
  metadata,
  type = c("lookup", "mapping", "relationship")
) {
  type <- rlang::arg_match(type)
  tbl_name <- codeminer_metadata_table_names[[type]]
  stopifnot(!is.null(tbl_name)) # sanity check, not expected to ever fail

  id_col <- switch(
    type,
    lookup = "lookup_table_name",
    mapping = "mapping_table_name",
    relationship = "relationship_table_name",
    codeminer_abort("Invalid metadata type: {type}.")
  )
  ids <- metadata[[id_col]]
  if (is.null(ids)) {
    codeminer_abort("Missing field {id_col} in metadata.")
  }

  # "latest" is reserved — it's the sentinel for automatic version resolution
  version_col <- switch(
    type,
    lookup = "lookup_version",
    mapping = "map_version",
    relationship = "relationship_version"
  )
  versions <- metadata[[version_col]]
  if (!is.null(versions) && any(versions == "latest")) {
    codeminer_abort(
      "{.val latest} is a reserved version name and cannot be used."
    )
  }

  # ">" is reserved in code_type values — used as a separator in mapping keys
  code_type_cols <- switch(
    type,
    lookup = "code_type",
    mapping = c("from_code_type", "to_code_type"),
    relationship = "code_type"
  )
  for (col in code_type_cols) {
    vals <- metadata[[col]]
    if (!is.null(vals) && any(grepl(">", vals, fixed = TRUE))) {
      codeminer_abort(
        "Code type values must not contain {.val >} (found in column {.field {col}})."
      )
    }
  }

  current_metadata <- read_table_from_db(con, tbl_name)
  exists <- any(ids %in% current_metadata[[id_col]])

  # Don't allow overwriting existing metadata
  if (exists) {
    return(invisible(FALSE))
  }

  meta_df <- as.data.frame(metadata)
  rows_added <- DBI::dbAppendTable(con, tbl_name, meta_df)
  return(invisible(rows_added))
}

# Remove a data table and its metadata row from the database.
#
# Shared helper for `remove_lookup_table()`, `remove_mapping_table()`,
# and `remove_relationship_table()`.
#
# @param con A database connection object (write access required).
# @param type One of "lookup", "mapping", or "relationship".
# @param table_name The internal table name (primary key in the metadata table).
# @return `TRUE` invisibly.
# @noRd
# @keywords internal
remove_table_entry <- function(con, type, table_name) {
  meta_tbl <- codeminer_metadata_table_names[[type]]
  id_col <- switch(
    type,
    lookup = "lookup_table_name",
    mapping = "mapping_table_name",
    relationship = "relationship_table_name"
  )

  # Delete metadata row
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "DELETE FROM {`meta_tbl`} WHERE {`id_col`} = {table_name}",
      .con = con
    )
  )

  # Drop data table
  if (table_exists(con, table_name)) {
    DBI::dbRemoveTable(con, table_name)
  }

  invisible(TRUE)
}
