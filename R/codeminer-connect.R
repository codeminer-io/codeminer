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
#' [db_path()].
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
  if (is.null(main)) main <- db_path()

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
  if (exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }

  # Create in-memory workbench
  .codeminer_env$con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  .codeminer_env$db_paths <- list()

  # Attach main (read-only)
  if (file.exists(main)) {
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
  if (exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }
  for (field in c("con", "db_paths", "metadata", "active_versions")) {
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
  cli::cli_inform(c(
    "i" = "Workbench active",
    " " = "Main:  {.file {main_path}}",
    " " = "Extra: {.file {extra_path}}"
  ))
  invisible(.codeminer_env$db_paths)
}

#' Refresh the metadata cache
#'
#' Re-reads metadata tables from all attached databases and updates
#' the internal cache. Called automatically by [codeminer_connect()] and
#' after write operations.
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
  for (db_name in intersect(c("extra", "main"), names(.codeminer_env$db_paths))) {
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
            .codeminer_env$metadata[[type]], meta_df
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
