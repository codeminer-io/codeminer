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
  # `_db_metadata` is a single-row DB-level stamp, not a per-table index — it
  # is intentionally excluded from `get_codeminer_metadata()`. Inspect it via
  # `codeminer_status()` or by reading the table directly.
  type <- rlang::arg_match(
    type,
    sort(setdiff(names(codeminer_metadata_table_names), "db")),
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
    available <- unique(meta[[code_type_col]])

    if (length(available) == 0) {
      # Database has no types at all — hint about adding tables
      hint <- if (!is.null(add_fun_name)) {
        c(
          "i" = "Did you add the {type_label} table with {.fun {add_fun_name}}?"
        )
      }
    } else {
      # Show available types
      if (length(available) > 6) {
        n_more <- length(available) - 6
        hint <- c(
          "i" = "Available code types: {.val {available[1:6]}}, and {n_more} more",
          "i" = "Check available types with {.code get_codeminer_metadata(\"{type_label}\")}"
        )
      } else {
        hint <- c(
          "i" = "Available code types: {.val {available}}",
          "i" = "Check available types with {.code get_codeminer_metadata(\"{type_label}\")}"
        )
      }
    }

    codeminer_abort(
      c(
        "Code type {.val {code_type_val}} not found in {type_label} metadata.",
        hint
      ),
      call = call
    )
  }

  # Filter to code_type
  filtered <- meta[meta[[code_type_col]] == code_type_val, ]

  # Resolve "latest" — check pinned/cached version first, then resolve fresh.
  # Resolved versions are cached in active_versions so the info message from

  # get_latest_version() only appears once per code_type per session.
  # Cache is invalidated by add/remove table functions and codeminer_refresh_cache().
  if (identical(version_val, "latest")) {
    pinned <- .codeminer_env$active_versions[[pin_type]][[pin_key]]
    if (!is.null(pinned)) {
      version_val <- pinned
    } else {
      version_val <- get_latest_version(filtered[[version_col]])
      if (!is.null(pin_type)) {
        if (is.null(.codeminer_env$active_versions[[pin_type]])) {
          .codeminer_env$active_versions[[pin_type]] <- list()
        }
        .codeminer_env$active_versions[[pin_type]][[pin_key]] <- version_val
      }
    }
  }

  # Filter to version
  this_meta <- filtered[filtered[[version_col]] == version_val, ]

  if (nrow(this_meta) == 0) {
    available_versions <- unique(filtered[[version_col]])
    codeminer_abort(
      c(
        "No {type_label} metadata found for {.val {code_type_val}} version {.val {version_val}}",
        "i" = "Available versions for {.val {code_type_val}}: {.val {available_versions}}"
      ),
      call = call
    )
  }
  stopifnot(nrow(this_meta) == 1)
  return(this_meta)
}

# --- col_filters serialisation helpers ----------------------------------------

#' Serialise col_filters to JSON
#'
#' Converts a named list of column filter specifications to a JSON string for
#' storage in the database. Validates the structure before serialising.
#'
#' @param col_filters A named list where each element is a list with `values`
#'   (character vector of all valid values) and `defaults` (character vector of
#'   default values, must be a subset of `values`). `NULL` is allowed and
#'   returns `NA_character_`.
#' @param call The calling environment for error messages.
#' @return A single JSON string, or `NA_character_` if `col_filters` is `NULL`.
#' @noRd
serialise_col_filters <- function(col_filters, call = rlang::caller_env()) {
  if (is.null(col_filters)) {
    return(NA_character_)
  }

  validate_col_filters_structure(col_filters, call = call)

  jsonlite::toJSON(col_filters, auto_unbox = FALSE)
}

#' Deserialise col_filters from JSON
#'
#' Converts a JSON string from the database back to a named list of column
#' filter specifications.
#'
#' @param json_string A JSON string, or `NA` / `NULL`.
#' @return A named list of column filter specs, or `NULL` if input is `NA` /
#'   `NULL` / empty string.
#' @noRd
deserialise_col_filters <- function(json_string) {
  if (is.null(json_string) || is.na(json_string) || json_string == "") {
    return(NULL)
  }

  result <- jsonlite::fromJSON(json_string, simplifyVector = TRUE)

  # jsonlite may return a data.frame or nested list — normalise to named list
  # of lists with character vectors
  lapply(result, function(entry) {
    list(
      values = as.character(entry$values),
      defaults = as.character(entry$defaults)
    )
  })
}

#' Validate col_filters structure
#'
#' Checks that a col_filters list has the correct structure: a named list where
#' each element contains `values` and `defaults` character vectors, with
#' `defaults` being a subset of `values`.
#'
#' @param col_filters The col_filters list to validate.
#' @param call The calling environment for error messages.
#' @return `col_filters` invisibly if valid.
#' @noRd
validate_col_filters_structure <- function(
  col_filters,
  call = rlang::caller_env()
) {
  if (!is.list(col_filters)) {
    codeminer_abort(
      "{.arg col_filters} must be a named list, not {.cls {class(col_filters)}}.",
      call = call
    )
  }

  nms <- names(col_filters)
  if (is.null(nms) || any(nms == "")) {
    codeminer_abort(
      "{.arg col_filters} must be a named list (all elements must have names).",
      call = call
    )
  }

  if (anyDuplicated(nms)) {
    dups <- nms[duplicated(nms)]
    codeminer_abort(
      "Duplicate column names in {.arg col_filters}: {.field {dups}}.",
      call = call
    )
  }

  for (nm in nms) {
    entry <- col_filters[[nm]]

    if (!is.list(entry) || !all(c("values", "defaults") %in% names(entry))) {
      codeminer_abort(
        c(
          "Each entry in {.arg col_filters} must be a list with {.field values} and {.field defaults}.",
          "x" = "Entry {.field {nm}} is invalid."
        ),
        call = call
      )
    }

    values <- as.character(entry$values)
    defaults <- as.character(entry$defaults)

    if (length(values) == 0) {
      codeminer_abort(
        "{.field values} for column {.field {nm}} must not be empty.",
        call = call
      )
    }

    if (anyDuplicated(values)) {
      dups <- values[duplicated(values)]
      codeminer_abort(
        "Duplicate values in {.arg col_filters} for column {.field {nm}}: {.val {dups}}.",
        call = call
      )
    }

    if (anyDuplicated(defaults)) {
      dups <- defaults[duplicated(defaults)]
      codeminer_abort(
        "Duplicate defaults in {.arg col_filters} for column {.field {nm}}: {.val {dups}}.",
        call = call
      )
    }

    bad <- setdiff(defaults, values)
    if (length(bad) > 0) {
      codeminer_abort(
        c(
          "{.field defaults} must be a subset of {.field values} for column {.field {nm}}.",
          "x" = "Not found in values: {.val {bad}}."
        ),
        call = call
      )
    }
  }

  invisible(col_filters)
}

#' Validate col_filters column names against a data table
#'
#' Checks that all column names referenced in col_filters actually exist in the
#' given data table.
#'
#' @param col_filters A col_filters list (already validated structurally).
#' @param table_cols Character vector of column names in the data table.
#' @param table_name Name of the data table (for error messages).
#' @param call The calling environment for error messages.
#' @return `col_filters` invisibly if valid.
#' @noRd
validate_col_filters_columns <- function(
  col_filters,
  table_cols,
  table_name,
  call = rlang::caller_env()
) {
  if (is.null(col_filters)) {
    return(invisible(col_filters))
  }

  filter_cols <- names(col_filters)
  missing_cols <- setdiff(filter_cols, table_cols)

  if (length(missing_cols) > 0) {
    codeminer_abort(
      c(
        "{.arg col_filters} references columns not found in table {.field {table_name}}.",
        "x" = "Missing columns: {.field {missing_cols}}.",
        "i" = "Available columns: {.field {table_cols}}."
      ),
      call = call
    )
  }

  invisible(col_filters)
}

# --- col_filters resolution and application -----------------------------------

#' Resolve col_filters for a query
#'
#' Determines the effective col_filters to apply, following the resolution order:
#' explicit list > session pin > metadata defaults > NULL (no filtering).
#'
#' @param col_filters The col_filters argument from the calling function:
#'   `"default"` to resolve from pins/metadata, `NULL` for no filtering, or a
#'   named list for explicit override.
#' @param metadata_col_filters The serialised JSON col_filters string from
#'   the metadata table.
#' @param pin_type Key into `.codeminer_env$active_col_filters` (one of
#'   "lookup", "mapping", or "relationship").
#' @param pin_key Key to look up in the pinned col_filters list (e.g.
#'   code_type or "from > to" mapping key).
#' @return A named list of `col_name = c(values)` pairs for filtering, or
#'   `NULL` if no filtering should be applied.
#' @noRd
resolve_col_filters <- function(
  col_filters,
  metadata_col_filters,
  pin_type,
  pin_key
) {
  # Explicit NULL → no filtering
  if (is.null(col_filters)) {
    return(NULL)
  }

  # Explicit list → use directly
  if (is.list(col_filters)) {
    return(col_filters)
  }

  # Must be "default" at this point
  if (!identical(col_filters, "default")) {
    codeminer_abort(
      '{.arg col_filters} must be "default", NULL, or a named list.'
    )
  }

  # Check session pin first
  pinned <- .codeminer_env$active_col_filters[[pin_type]][[pin_key]]
  if (!is.null(pinned)) {
    return(pinned)
  }

  # Fall back to metadata defaults
  full_spec <- deserialise_col_filters(metadata_col_filters)
  if (is.null(full_spec)) {
    return(NULL)
  }

  # Extract just the defaults from each entry
  defaults <- lapply(full_spec, function(entry) entry$defaults)
  # Drop entries with empty defaults (no default filtering for that column)
  defaults <- Filter(function(x) length(x) > 0, defaults)

  if (length(defaults) == 0) {
    return(NULL)
  }
  defaults
}

#' Apply resolved col_filters to a lazy dplyr table
#'
#' For each column name → values pair, applies a `dplyr::filter()` to keep only
#' rows where the column value is in the specified values.
#'
#' @param tbl A lazy dplyr table (from `dplyr::tbl()`).
#' @param col_filters A named list of `col_name = c(values)` pairs, or `NULL`.
#' @param tbl_name The table name (for warning messages).
#' @param call The calling environment for warning messages.
#' @return The filtered lazy dplyr table.
#' @noRd
apply_col_filters <- function(
  tbl,
  col_filters,
  tbl_name = "unknown",
  call = rlang::caller_env()
) {
  if (is.null(col_filters) || length(col_filters) == 0) {
    return(tbl)
  }

  tbl_cols <- colnames(tbl)

  for (col_name in names(col_filters)) {
    if (!col_name %in% tbl_cols) {
      codeminer_warn(
        c(
          "Column {.field {col_name}} not found in table {.field {tbl_name}}.",
          "i" = "Skipping this col_filter."
        ),
        call = call
      )
      next
    }

    values <- col_filters[[col_name]]
    tbl <- dplyr::filter(tbl, .data[[col_name]] %in% values)
  }

  tbl
}

#' Extract column filters from database metadata
#'
#' Reads `col_filters` from all metadata tables in the connected database.
#' Returns a nested list keyed by table type and table key (code type or
#' mapping pair).
#'
#' @param defaults_only Logical. If `TRUE` (default), return only the default
#'   filter values. If `FALSE`, return the full specification including all
#'   available values (useful for Shiny UI checkboxes).
#'
#' @return A named list with entries for `lookup`, `mapping`, and
#'   `relationship`. Each entry is a named list keyed by code type (or
#'   `"from > to"` for mappings), containing either:
#'   - If `defaults_only = TRUE`: a flat `list(col = c(default_values))`
#'   - If `defaults_only = FALSE`: a full `list(col = list(values = ..., defaults = ...))`
#'
#'   Returns an empty list if no database is connected.
#' @export
#' @family Workbench management
get_col_filters <- function(defaults_only = TRUE) {
  check_logical_scalar(defaults_only, "defaults_only")

  result <- list()

  # Safely access metadata — return empty list if no connection
  tryCatch(
    {
      for (type in c("lookup", "mapping", "relationship")) {
        meta <- .codeminer_env$metadata[[type]]
        if (is.null(meta) || nrow(meta) == 0) {
          next
        }

        # Determine the key column(s) for each type
        key_fn <- switch(
          type,
          lookup = function(row) row$code_type,
          mapping = function(row) {
            paste(row$from_code_type, ">", row$to_code_type)
          },
          relationship = function(row) row$code_type
        )

        type_result <- list()
        for (i in seq_len(nrow(meta))) {
          row <- meta[i, ]
          cf <- deserialise_col_filters(row$col_filters)
          if (is.null(cf)) {
            next
          }

          key <- key_fn(row)
          if (defaults_only) {
            type_result[[key]] <- lapply(cf, function(entry) entry$defaults)
          } else {
            type_result[[key]] <- cf
          }
        }

        if (length(type_result) > 0) {
          result[[type]] <- type_result
        }
      }
    },
    error = function(e) {
      # No database connected — return empty list
    }
  )

  result
}

#' Default column filters
#'
#' Returns the default filtering parameters for all registered lookup, mapping,
#' and relationship tables. Equivalent to `get_col_filters(defaults_only = TRUE)`.
#'
#' @return A named list. See [get_col_filters()] for details.
#' @export
#' @family Workbench management
default_col_filters <- function() {
  get_col_filters(defaults_only = TRUE)
}

# Helper to read a table from the database as a data.frame
read_table_from_db <- function(con, tbl_name) {
  DBI::dbReadTable(con, tbl_name)
}

table_exists <- function(con, tbl_name) {
  existing_tables <- DBI::dbListTables(con)
  return(tbl_name %in% existing_tables)
}

# Check whether all specified tables already exist in the database metadata.
# Returns TRUE if every table is found, FALSE otherwise.
# Used by add_*_trud() wrappers to skip expensive read operations.
tables_all_exist <- function(table_names, types) {
  db <- db_path()
  if (!file.exists(db)) {
    return(FALSE)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  for (i in seq_along(table_names)) {
    meta_tbl <- codeminer_metadata_table_names[[types[[i]]]]
    if (!table_exists(con, meta_tbl)) {
      return(FALSE)
    }

    id_col <- switch(
      types[[i]],
      lookup = "lookup_table_name",
      mapping = "mapping_table_name",
      relationship = "relationship_table_name"
    )

    query <- glue::glue_sql(
      "SELECT 1 FROM {`meta_tbl`} WHERE {`id_col`} = {table_names[[i]]} LIMIT 1",
      .con = con
    )
    if (nrow(DBI::dbGetQuery(con, query)) == 0) {
      return(FALSE)
    }
  }

  TRUE
}

# Helper to check if the database is valid or throw an error.
#
# Accepts either a path (the modern call style used by the write
# functions and the backend abstraction) or a DBI connection (for
# back-compat with older tests). For paths, dispatches through the
# backend so both duckdb_file and parquet_folder DBs are checked
# consistently.
check_database <- function(con_or_path) {
  error_msg <- c(
    "The database is not initialised.",
    "i" = "You may need to build the database first with {.fun codeminer::build_database}"
  )
  if (is.character(con_or_path)) {
    path <- con_or_path
    has_lookup_meta <- backend_table_exists(
      path,
      codeminer_metadata_table_names$lookup
    )
    has_mapping_meta <- backend_table_exists(
      path,
      codeminer_metadata_table_names$mapping
    )
    has_relationship_meta <- backend_table_exists(
      path,
      codeminer_metadata_table_names$relationship
    )
  } else {
    con <- con_or_path
    has_lookup_meta <- table_exists(con, codeminer_metadata_table_names$lookup)
    has_mapping_meta <- table_exists(
      con,
      codeminer_metadata_table_names$mapping
    )
    has_relationship_meta <- table_exists(
      con,
      codeminer_metadata_table_names$relationship
    )
  }

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
