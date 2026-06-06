# DuckDB ATTACH aliases.
# "main" is reserved in DuckDB (it's the default schema for any connection),
# so we use "core" for the read-only ontology database and "user_db" for the
# user's private read-write database.
CODEMINER_ALIAS_MAIN <- "core"
CODEMINER_ALIAS_EXTRA <- "user_db"

#' Connect to the codeminer workbench
#'
#' Creates an in-memory DuckDB connection and ATTACHes one or more database
#' files. The `main` database is attached as read-only (immutable ontologies).
#' The optional `extra` database is attached as read-write (user-defined
#' tables). A search path is set so that tables in `extra` shadow those
#' in `main`.
#'
#' If called with no arguments, uses the default database path from
#' the `CODEMINER_DB_PATH` environment variable (or `rappdirs` default).
#'
#' @param main Path to the main (read-only) DuckDB database file.
#'   Defaults to the path from `CODEMINER_DB_PATH` env var or
#'   `rappdirs::user_data_dir("codeminer")`.
#' @param extra Optional path to an extra (read-write) DuckDB database
#'   file. If the file does not exist, it will be created with empty
#'   metadata tables.
#'
#' @return The DBI connection object, invisibly.
#' @export
#' @family Workbench management
#' @seealso [codeminer_disconnect()], [codeminer_status()]
codeminer_connect <- function(main = NULL, extra = NULL) {
  main_was_explicit <- !is.null(main)
  if (is.null(main)) {
    main <- db_path()
  }

  if (!main_was_explicit) {
    codeminer_inform(c(
      "i" = "Using database at {.file {main}}",
      "i" = paste(
        "Set {.envvar CODEMINER_DB_PATH} or use",
        "{.fun codeminer_connect} to change this."
      )
    ))
  }

  # Tear down existing workbench if one exists
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }

  # Create in-memory workbench
  .codeminer_env$con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  .codeminer_env$db_paths <- list()

  # Attach main (read-only)
  if (main_was_explicit && !file.exists(main)) {
    codeminer_abort(c(
      "Database file not found at {.file {main}}.",
      "i" = paste(
        "To create a new database, use",
        "{.code Sys.setenv(CODEMINER_DB_PATH = ...)} then",
        "{.fun build_database}."
      )
    ))
  }
  if (file.exists(main)) {
    # Gate the schema version BEFORE the read-only ATTACH so the migration
    # path can open its own write connection without lock contention.
    enforce_schema_gate(main)
    DBI::dbExecute(
      .codeminer_env$con,
      glue::glue_sql(
        "ATTACH {main} AS {`CODEMINER_ALIAS_MAIN`} (READ_ONLY)",
        .con = .codeminer_env$con
      )
    )
    .codeminer_env$db_paths$main <- main
  }

  # Attach extra (read-write) if provided
  if (!is.null(extra)) {
    if (!file.exists(extra)) {
      codeminer_init_extra(extra)
    }
    DBI::dbExecute(
      .codeminer_env$con,
      glue::glue_sql(
        "ATTACH {extra} AS {`CODEMINER_ALIAS_EXTRA`}",
        .con = .codeminer_env$con
      )
    )
    .codeminer_env$db_paths$extra <- extra
  }

  # Set search path: extra first so user tables shadow core ontologies
  codeminer_set_search_path()

  # Clear cached version selections — they may refer to a different database
  .codeminer_env$active_versions <- list()

  # Cache metadata from attached databases
  codeminer_refresh_cache()

  invisible(.codeminer_env$con)
}

#' Disconnect the codeminer workbench
#'
#' Tears down the in-memory DuckDB connection and clears cached metadata.
#' Non-connection state (e.g. extracted file paths) is preserved.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
codeminer_disconnect <- function() {
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }
  for (field in c(
    "con",
    "db_paths",
    "metadata",
    "active_versions",
    "active_col_filters"
  )) {
    if (exists(field, envir = .codeminer_env)) {
      rm(list = field, envir = .codeminer_env)
    }
  }
  invisible()
}

#' Show workbench status
#'
#' Prints the current state of the workbench connection, including
#' which databases are attached.
#'
#' @return A named list of attached database paths, invisibly.
#' @export
#' @family Workbench management
codeminer_status <- function() {
  if (
    !exists("con", envir = .codeminer_env) ||
      !DBI::dbIsValid(.codeminer_env$con)
  ) {
    cli::cli_inform("No active workbench connection.")
    return(invisible(list()))
  }
  main_path <- .codeminer_env$db_paths$main %||% "not attached"
  extra_path <- .codeminer_env$db_paths$extra %||% "not attached"
  msgs <- c(
    "i" = "Workbench active",
    " " = "Main:  {.file {main_path}}",
    " " = "Extra: {.file {extra_path}}"
  )

  # Show pinned versions if any
  pins <- .codeminer_env$active_versions
  if (length(pins) > 0) {
    msgs <- c(msgs, "i" = "Active versions:")
    for (type in names(pins)) {
      for (key in names(pins[[type]])) {
        msgs <- c(
          msgs,
          " " = "  {type}: {.val {key}} = {.val {pins[[type]][[key]]}}"
        )
      }
    }
  }

  # Show pinned col_filters if any
  cf_pins <- .codeminer_env$active_col_filters
  if (length(cf_pins) > 0) {
    msgs <- c(msgs, "i" = "Pinned column filters:")
    for (type in names(cf_pins)) {
      for (key in names(cf_pins[[type]])) {
        filter_desc <- paste(
          vapply(
            names(cf_pins[[type]][[key]]),
            function(col) {
              vals <- cf_pins[[type]][[key]][[col]]
              paste0(col, " = [", paste(vals, collapse = ", "), "]")
            },
            character(1)
          ),
          collapse = "; "
        )
        msgs <- c(
          msgs,
          " " = "  {type}: {.val {key}} -> {filter_desc}"
        )
      }
    }
  }

  cli::cli_inform(msgs)
  invisible(.codeminer_env$db_paths)
}

#' Refresh the metadata cache
#'
#' Re-reads metadata tables from all attached databases and updates
#' the internal cache. Called automatically by [codeminer_connect()] and
#' after write operations.
#'
#' This refreshes the *metadata inventory* (which tables and versions exist
#' in the database). It does not affect version selections — use
#' [codeminer_clear_versions()] to reset which version is used for each
#' code type.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
codeminer_refresh_cache <- function() {
  con <- .codeminer_env$con
  .codeminer_env$metadata <- list()

  # Map R-side names to DuckDB ATTACH aliases
  alias_map <- c(main = CODEMINER_ALIAS_MAIN, extra = CODEMINER_ALIAS_EXTRA)

  # Read metadata from each attached database
  # extra is iterated first so it takes priority when combined
  for (db_name in intersect(
    c("extra", "main"),
    names(.codeminer_env$db_paths)
  )) {
    schema <- alias_map[[db_name]]
    for (type in c("lookup", "mapping", "relationship")) {
      tbl_name <- codeminer_metadata_table_names[[type]]
      if (schema_table_exists(con, schema, tbl_name)) {
        qualified <- paste0(schema, ".", tbl_name)
        meta_df <- DBI::dbGetQuery(
          con,
          paste0("SELECT * FROM ", qualified)
        )
        if (nrow(meta_df) > 0) {
          meta_df$.schema <- schema
          .codeminer_env$metadata[[type]] <- dplyr::bind_rows(
            .codeminer_env$metadata[[type]],
            meta_df
          )
        }
      }
    }
  }

  invisible()
}

#' Create a snapshot of the extra database
#'
#' Uses DuckDB's `VACUUM INTO` to create a clean, compacted copy of the
#' user's extra database at the specified path.
#'
#' @param path File path for the snapshot.
#'
#' @return The snapshot path, invisibly.
#' @export
#' @family Workbench management
codeminer_snapshot_extra <- function(path) {
  con <- get_db_con()
  if (is.null(.codeminer_env$db_paths$extra)) {
    codeminer_abort(
      "No extra database is attached to the workbench."
    )
  }
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "VACUUM {`CODEMINER_ALIAS_EXTRA`} INTO {path}",
      .con = con
    )
  )
  codeminer_inform("Snapshot saved to {.file {path}}")
  invisible(path)
}

#' Create an empty extra database
#'
#' Creates a new DuckDB file with empty metadata tables, ready to be
#' attached as the user's extra database.
#'
#' @param path File path for the new database.
#'
#' @return The path, invisibly.
#' @noRd
codeminer_init_extra <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), path)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  create_lookup_metadata_table(con)
  create_mapping_metadata_table(con)
  create_relationship_metadata_table(con)
  invisible(path)
}


#' Pin table versions for the session
#'
#' Overrides the default "latest" version resolution for lookup, relationship,
#' and/or mapping tables. Pinned versions persist until cleared with
#' [codeminer_clear_versions()] or [codeminer_disconnect()].
#'
#' @param lookup Named character vector of lookup versions, keyed by code type.
#'   E.g. `c("ICD-10" = "v42", "Read v3" = "v1")`.
#' @param relationship Named character vector of relationship versions, keyed
#'   by code type.
#' @param mapping Named character vector of mapping versions, keyed by
#'   `"from > to"` pairs. E.g. `c("Read 3 > ICD-10" = "v1")`.
#'
#' @details
#' Pinned versions only affect "latest" resolution. Explicit version arguments
#' on query functions (e.g. `CODES(..., lookup_version = "v1")`) always take
#' precedence.
#'
#' Versions are also auto-cached the first time `"latest"` is resolved for a
#' given code type. Calling `codeminer_set_version()` overrides any
#' auto-cached version.
#'
#' New pins are merged with existing ones. To replace all pins, call
#' [codeminer_clear_versions()] first.
#'
#' @return The current pinned versions (a list), invisibly.
#' @export
#' @family Workbench management
#' @examples
#' \dontrun{
#' # Pin lookup versions for multiple code types
#' codeminer_set_version(
#'   lookup = c("ICD-10" = "v42", "Read v3" = "v1")
#' )
#'
#' # Pin mapping version for a specific pair
#' codeminer_set_version(
#'   mapping = c("Read 3 > ICD-10" = "v1")
#' )
#'
#' # Clear all pins
#' codeminer_clear_versions()
#' }
codeminer_set_version <- function(
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  if (is.null(lookup) && is.null(relationship) && is.null(mapping)) {
    codeminer_abort(
      "At least one of {.arg lookup}, {.arg relationship}, or {.arg mapping} must be provided."
    )
  }

  # Trim whitespace from keys and values (users may copy-paste from configs)
  if (!is.null(lookup)) {
    lookup <- trim_pins(lookup)
  }
  if (!is.null(relationship)) {
    relationship <- trim_pins(relationship)
  }
  if (!is.null(mapping)) {
    mapping <- trim_pins(mapping)
  }

  # Reject "latest" as a pin value — it's the sentinel we're overriding
  all_pins <- c(lookup, relationship, mapping)
  if (any(all_pins == "latest")) {
    codeminer_abort(
      "{.val latest} cannot be used as a pinned version. Specify the actual version string instead."
    )
  }

  # Initialise if needed
  if (is.null(.codeminer_env$active_versions)) {
    .codeminer_env$active_versions <- list()
  }

  # Validate and merge each type
  if (!is.null(lookup)) {
    validate_version_pins(lookup, "lookup", "lookup_version", "code_type")
    .codeminer_env$active_versions$lookup <- merge_pins(
      .codeminer_env$active_versions$lookup,
      lookup
    )
  }
  if (!is.null(relationship)) {
    validate_version_pins(
      relationship,
      "relationship",
      "relationship_version",
      "code_type"
    )
    .codeminer_env$active_versions$relationship <- merge_pins(
      .codeminer_env$active_versions$relationship,
      relationship
    )
  }
  if (!is.null(mapping)) {
    mapping <- normalize_mapping_keys(mapping)
    validate_version_pins(mapping, "mapping", "map_version", NULL)
    .codeminer_env$active_versions$mapping <- merge_pins(
      .codeminer_env$active_versions$mapping,
      mapping
    )
  }

  invisible(.codeminer_env$active_versions)
}

#' Clear active version selections
#'
#' Removes version selections from the current session. This covers both
#' versions pinned explicitly with [codeminer_set_version()] and versions
#' auto-cached on first `"latest"` resolution.
#'
#' Called with no arguments, all version selections are cleared and subsequent
#' queries will re-resolve `"latest"` from the database. To clear only
#' specific code types, pass them as character vectors.
#'
#' @param lookup Character vector of code types whose lookup version should
#'   be cleared (e.g. `"ICD-10"`). `NULL` (default) means no lookup versions
#'   are cleared unless all arguments are `NULL`.
#' @param relationship Character vector of code types whose relationship
#'   version should be cleared. `NULL` (default) means no relationship
#'   versions are cleared unless all arguments are `NULL`.
#' @param mapping Character vector of mapping keys (`"from > to"` format)
#'   whose version should be cleared. `NULL` (default) means no mapping
#'   versions are cleared unless all arguments are `NULL`.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
#' @examples
#' \dontrun{
#' # Clear all version selections
#' codeminer_clear_versions()
#'
#' # Clear only the ICD-10 lookup version
#' codeminer_clear_versions(lookup = "ICD-10")
#'
#' # Clear lookup and relationship versions for SNOMED CT
#' codeminer_clear_versions(
#'   lookup = "SNOMED CT",
#'   relationship = "SNOMED CT"
#' )
#' }
codeminer_clear_versions <- function(
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  if (is.null(lookup) && is.null(relationship) && is.null(mapping)) {
    .codeminer_env$active_versions <- list()
  } else {
    for (key in lookup) {
      .codeminer_env$active_versions[["lookup"]][[key]] <- NULL
    }
    for (key in relationship) {
      .codeminer_env$active_versions[["relationship"]][[key]] <- NULL
    }
    for (key in mapping) {
      .codeminer_env$active_versions[["mapping"]][[key]] <- NULL
    }
  }
  invisible()
}


#' Pin column filters for the session
#'
#' Overrides the default column filters defined in table metadata. Pinned
#' filters persist until cleared with [codeminer_clear_col_filters()] or
#' [codeminer_disconnect()].
#'
#' @param lookup Named list of column filters for lookup tables, keyed by
#'   code type. Each value is a named list of `column_name = c(values)` pairs.
#'   E.g. `list("SNOMED CT" = list(active_concept = c("1")))`.
#' @param relationship Named list of column filters for relationship tables,
#'   keyed by code type.
#' @param mapping Named list of column filters for mapping tables, keyed by
#'   `"from > to"` pairs.
#'   E.g. `list("Read 3 > ICD-10" = list(mapping_status = c("E", "G")))`.
#'
#' @details
#' Pinned filters only affect `col_filters = "default"` resolution. Explicit
#' `col_filters` arguments on query functions always take precedence.
#'
#' New pins are merged with existing ones. To replace all pins, call
#' [codeminer_clear_col_filters()] first.
#'
#' @return The current pinned col_filters (a list), invisibly.
#' @export
#' @family Workbench management
#' @seealso [with_col_filters()], [codeminer_clear_col_filters()]
#' @examples
#' \dontrun{
#' # Pin lookup filters — only return active SNOMED concepts
#' codeminer_set_col_filters(
#'   lookup = list("SNOMED CT" = list(active_concept = c("1")))
#' )
#'
#' # Pin mapping filters
#' codeminer_set_col_filters(
#'   mapping = list("Read 3 > ICD-10" = list(mapping_status = c("E", "G")))
#' )
#'
#' # Clear all filter pins
#' codeminer_clear_col_filters()
#' }
codeminer_set_col_filters <- function(
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  if (is.null(lookup) && is.null(relationship) && is.null(mapping)) {
    codeminer_abort(
      "At least one of {.arg lookup}, {.arg relationship}, or {.arg mapping} must be provided."
    )
  }

  # Initialise if needed
  if (is.null(.codeminer_env$active_col_filters)) {
    .codeminer_env$active_col_filters <- list()
  }

  # Validate and merge each type
  if (!is.null(lookup)) {
    validate_col_filter_pins(lookup, "lookup")
    .codeminer_env$active_col_filters$lookup <- merge_col_filter_pins(
      .codeminer_env$active_col_filters$lookup,
      lookup
    )
  }
  if (!is.null(relationship)) {
    validate_col_filter_pins(relationship, "relationship")
    .codeminer_env$active_col_filters$relationship <- merge_col_filter_pins(
      .codeminer_env$active_col_filters$relationship,
      relationship
    )
  }
  if (!is.null(mapping)) {
    validate_col_filter_pins(mapping, "mapping")
    .codeminer_env$active_col_filters$mapping <- merge_col_filter_pins(
      .codeminer_env$active_col_filters$mapping,
      mapping
    )
  }

  invisible(.codeminer_env$active_col_filters)
}

#' Clear all pinned column filters
#'
#' Removes all column filter pins set by [codeminer_set_col_filters()],
#' returning to the metadata-defined defaults.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
codeminer_clear_col_filters <- function() {
  .codeminer_env$active_col_filters <- list()
  invisible()
}

#' Temporarily override column filters
#'
#' Sets column filter pins for the duration of the supplied code block,
#' then restores the previous state. This is useful when you need different
#' filters for a group of calls without permanently changing session state.
#'
#' @inheritParams codeminer_set_col_filters
#' @param code Code to execute with the temporary filters.
#'
#' @return The result of evaluating `code`.
#' @export
#' @family Workbench management
#' @seealso [codeminer_set_col_filters()]
#' @examples
#' \dontrun{
#' # Temporarily include inactive SNOMED concepts
#' with_col_filters(
#'   {
#'     CODES("all", type = "SNOMED CT")
#'   },
#'   lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))
#' )
#' }
with_col_filters <- function(
  code,
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  # Save current state
  old_filters <- .codeminer_env$active_col_filters

  # Restore on exit (even if code errors)
  on.exit(.codeminer_env$active_col_filters <- old_filters, add = TRUE)

  # Apply temporary pins (merge with existing)
  if (!is.null(lookup) || !is.null(relationship) || !is.null(mapping)) {
    codeminer_set_col_filters(
      lookup = lookup,
      relationship = relationship,
      mapping = mapping
    )
  }

  force(code)
}


# Internal helpers --------------------------------------------------------

#' Set the DuckDB search path based on attached databases
#' @noRd
codeminer_set_search_path <- function() {
  alias_map <- c(main = CODEMINER_ALIAS_MAIN, extra = CODEMINER_ALIAS_EXTRA)
  attached <- intersect(c("extra", "main"), names(.codeminer_env$db_paths))
  if (length(attached) > 0) {
    aliases <- alias_map[attached]
    search_path <- paste(aliases, collapse = ",")
    DBI::dbExecute(
      .codeminer_env$con,
      paste0("SET search_path = '", search_path, "'")
    )
  }
}

#' Check if a table exists in a specific attached database (catalog)
#'
#' In DuckDB, ATTACHed databases are catalogs (not schemas). The default
#' schema within each catalog is "main".
#' @noRd
schema_table_exists <- function(con, schema, tbl_name) {
  query <- glue::glue_sql(
    "SELECT 1 FROM information_schema.tables
     WHERE table_catalog = {schema}
     AND table_name = {tbl_name}",
    .con = con
  )
  nrow(DBI::dbGetQuery(con, query)) > 0
}

#' Validate version pins against cached metadata
#' @noRd
validate_version_pins <- function(
  pins,
  type,
  version_col,
  key_col,
  call = rlang::caller_env()
) {
  if (!rlang::is_named(pins) || !is.character(pins)) {
    codeminer_abort(
      "{.arg {type}} must be a named character vector.",
      call = call
    )
  }

  # Validate mapping key format (always, regardless of metadata availability)
  if (type == "mapping") {
    for (key in names(pins)) {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      if (length(parts) != 2) {
        codeminer_abort(
          "Mapping key {.val {key}} must be in {.val from > to} format.",
          call = call
        )
      }
    }
  }

  # Cross-check against cached metadata (skip if metadata not available)
  meta <- .codeminer_env$metadata[[type]]
  if (is.null(meta)) {
    return(invisible())
  }

  for (i in seq_along(pins)) {
    key <- names(pins)[[i]]
    version <- pins[[i]]

    if (type == "mapping") {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      match <- meta$from_code_type == parts[1] &
        meta$to_code_type == parts[2] &
        meta[[version_col]] == version
    } else {
      match <- meta[[key_col]] == key & meta[[version_col]] == version
    }

    if (!any(match)) {
      codeminer_warn(c(
        "!" = "No {type} metadata found for {.val {key}} version {.val {version}}.",
        "i" = "Pin will be set but may not resolve to a valid table."
      ))
    }
  }
  invisible()
}

#' Trim leading/trailing whitespace from pin names and values
#' @noRd
trim_pins <- function(pins) {
  names(pins) <- trimws(names(pins))
  pins[] <- trimws(pins)
  pins
}

#' Normalize mapping pin keys to canonical "from > to" form
#'
#' Accepts flexible spacing (e.g. "Read 3>ICD-10", "Read 3 >  ICD-10") and
#' converts to the canonical form used internally by `get_metadata_for_mapping()`.
#' @noRd
normalize_mapping_keys <- function(pins) {
  new_names <- vapply(
    names(pins),
    function(key) {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      paste(parts, collapse = " > ")
    },
    character(1),
    USE.NAMES = FALSE
  )
  names(pins) <- new_names
  pins
}

#' Merge new pins into existing pins (overwrite by name)
#' @noRd
merge_pins <- function(existing, new_pins) {
  if (is.null(existing)) {
    return(as.list(new_pins))
  }
  existing <- as.list(existing)
  # New pins overwrite existing ones with the same name
  for (nm in names(new_pins)) {
    existing[[nm]] <- new_pins[[nm]]
  }
  existing
}

#' Validate col_filter pin structure
#' @noRd
validate_col_filter_pins <- function(
  pins,
  type,
  call = rlang::caller_env()
) {
  if (!is.list(pins) || !rlang::is_named(pins)) {
    codeminer_abort(
      "{.arg {type}} must be a named list.",
      call = call
    )
  }

  for (key in names(pins)) {
    filters <- pins[[key]]
    if (!is.list(filters) || !rlang::is_named(filters)) {
      codeminer_abort(
        "Column filters for {.val {key}} must be a named list of {.code column = c(values)} pairs.",
        call = call
      )
    }
    for (col in names(filters)) {
      if (!is.character(filters[[col]])) {
        codeminer_abort(
          "Filter values for column {.val {col}} in {.val {key}} must be a character vector.",
          call = call
        )
      }
    }
  }
  invisible()
}

#' Merge col_filter pins (new pins overwrite existing by key)
#' @noRd
merge_col_filter_pins <- function(existing, new_pins) {
  if (is.null(existing)) {
    return(new_pins)
  }
  existing[names(new_pins)] <- new_pins
  existing
}
