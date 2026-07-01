# Use locked environment to store global variables
codeminer_metadata_table_names <- new.env(parent = emptyenv())
codeminer_metadata_table_names$lookup <- "_lookup_metadata"
codeminer_metadata_table_names$mapping <- "_mapping_metadata"
codeminer_metadata_table_names$relationship <- "_relationship_metadata"
codeminer_metadata_table_names$db <- "_db_metadata"
lockEnvironment(codeminer_metadata_table_names, bindings = TRUE)

#' Build the Codeminer database
#'
#' Set up the codeminer database and create the required lookup and
#' mapping metadata tables.
#'
#' @param overwrite Logical indicating whether to overwrite existing tables
#'   (default: `FALSE`).
#' @param format Character. For folder-mode databases, controls how data
#'   tables are stored inside the folder. One of:
#'   * `"duckdb"` (default): metadata stays as parquet at the folder root;
#'     each data table is a `<name>.duckdb` file. Faster recursive queries
#'     (CHILDREN etc.) at the cost of ~50% larger disk than `"parquet"`.
#'   * `"parquet"`: data tables and metadata are all parquet files at the
#'     folder root. Smaller on disk; recursive queries hit re-scan cost.
#'   Ignored when `CODEMINER_DB_PATH` points at a single `.duckdb` file
#'   (there's only one shape — a single DuckDB file).
#'
#' @return `TRUE` invisibly if successful.
#'
#' @export
#' @examples
#' # Build a temporary database
#' db_path <- tempfile(fileext = ".duckdb")
#' Sys.setenv(CODEMINER_DB_PATH = db_path)
#' build_database()
#' file.exists(db_path)
build_database <- function(overwrite = FALSE, format = c("duckdb", "parquet")) {
  format <- rlang::arg_match(format)
  target_path <- db_path()
  db_exists <- backend_database_exists(target_path)
  if (db_exists) {
    codeminer_inform("Existing database found at {.file {target_path}}")
  } else {
    codeminer_inform("Creating new database at {.file {target_path}}")
  }

  # Existing-DB-without-overwrite path: the schema gate (run on
  # `codeminer_connect()`) will refuse it if the format has drifted; rebuild
  # via `overwrite = TRUE` if so. Nothing for us to do here.
  if (db_exists && !overwrite) {
    codeminer_inform(c(
      "i" = "Database already exists; pass {.code overwrite = TRUE} to recreate from scratch."
    ))
    return(invisible(TRUE))
  }

  # Resolve the storage format to stamp into `_db_metadata`. For file
  # paths there's only one shape; `format` is silently ignored. For
  # folder paths, `"duckdb"` maps to `codeminer_folder` (parquet meta +
  # per-table .duckdb), `"parquet"` maps to `parquet_folder`.
  storage_format <- if (dir.exists(target_path)) {
    switch(
      format,
      duckdb = "codeminer_folder",
      parquet = "parquet_folder"
    )
  } else {
    "duckdb_file"
  }

  # Fresh build (or `overwrite = TRUE`): if the workbench currently holds
  # the file open, release it first so the init routine can take a write
  # connection. The acquire_writable_workbench helper handles both
  # backend kinds and re-attaches on exit.
  acquire_writable_workbench(target_path)

  backend_init(
    target_path,
    overwrite = overwrite,
    storage_format = storage_format
  )

  invisible(TRUE)
}

# Does a database already exist at `path`? Backend-aware: for
# `duckdb_file`, the path is a regular file; for the folder backends
# (`codeminer_folder`, `parquet_folder`), it's a directory containing
# the metadata parquet files.
backend_database_exists <- function(path) {
  kind <- backend_kind(path)
  if (kind == "duckdb_file") {
    return(file.exists(path))
  }
  # folder backends: consider "exists" to mean the directory contains
  # the `_db_metadata` file. An empty directory or a stale folder
  # without the stamp file isn't a usable codeminer DB.
  file.exists(backend_meta_path(path, "db"))
}

#' Create lookup metadata table in database
#'
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite existing table (default: `FALSE`)
#'
#' @return Invisible `TRUE` on success
#' @noRd
create_lookup_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- codeminer_metadata_table_names$lookup

  lookup_cols <- required_lookup_metadata_columns()
  lookup_fields <- rep("VARCHAR", length(lookup_cols))
  names(lookup_fields) <- lookup_cols

  create_table(
    con,
    tbl_name = tbl_name,
    fields = c(
      lookup_table_name = "VARCHAR PRIMARY KEY",
      lookup_fields
    ),
    overwrite = overwrite
  )
}

#' Create mapping metadata table in database
#'
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite existing table (default: `FALSE`)
#'
#' @return Invisible `TRUE` on success
#' @noRd
create_mapping_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- codeminer_metadata_table_names$mapping

  mapping_cols <- required_mapping_metadata_columns()
  mapping_fields <- rep("VARCHAR", length(mapping_cols))
  names(mapping_fields) <- mapping_cols

  create_table(
    con,
    tbl_name = tbl_name,
    fields = c(
      mapping_table_name = "VARCHAR PRIMARY KEY",
      mapping_fields
    ),
    overwrite = overwrite
  )
}

#' Create relationship metadata table in database
#'
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite existing table (default: `FALSE`)
#'
#' @return Invisible `TRUE` on success
#' @noRd
create_relationship_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- codeminer_metadata_table_names$relationship

  relationship_cols <- required_relationship_metadata_columns()
  relationship_fields <- rep("VARCHAR", length(relationship_cols))
  names(relationship_fields) <- relationship_cols

  create_table(
    con,
    tbl_name = tbl_name,
    fields = c(
      relationship_table_name = "VARCHAR PRIMARY KEY",
      relationship_fields
    ),
    overwrite = overwrite
  )
}

#' Create the `_db_metadata` table in the database
#'
#' Single-row table carrying the codeminer schema stamp + install provenance
#' (`codeminer_version`, `schema_version`, `built_at`, `last_migrated_at`,
#' and the renv-style `codeminer_remote_*` fields). Schema for the columns
#' lives in [required_db_metadata_columns()].
#'
#' @param con Database connection, as returned by [DBI::dbConnect()]
#' @param overwrite Logical indicating whether to overwrite an existing
#'   table (default: `FALSE`).
#'
#' @return Invisible `TRUE` on success.
#' @noRd
create_db_metadata_table <- function(con, overwrite = FALSE) {
  tbl_name <- codeminer_metadata_table_names$db
  db_cols <- required_db_metadata_columns()
  db_fields <- rep("VARCHAR", length(db_cols))
  names(db_fields) <- db_cols
  create_table(
    con,
    tbl_name = tbl_name,
    fields = db_fields,
    overwrite = overwrite
  )
}

create_table <- function(con, tbl_name, fields, overwrite = FALSE) {
  tbl_exists <- table_exists(con, tbl_name)
  if (tbl_exists && overwrite) {
    codeminer_inform("Dropping existing table {tbl_name}")
    DBI::dbRemoveTable(con, tbl_name)
  } else if (tbl_exists) {
    codeminer_inform(
      "Table {tbl_name} already exists and `overwrite = FALSE`. Leaving as is."
    )
    return(invisible(TRUE))
  }
  success <- DBI::dbCreateTable(
    con,
    name = tbl_name,
    fields = fields
  )
  return(invisible(success))
}

#' Get required lookup metadata column names
#'
#' @return Character vector of required column names
#' @noRd
required_lookup_metadata_columns <- function() {
  c(
    "code_type",
    "lookup_version",
    "lookup_code_col",
    "lookup_description_col",
    "lookup_category_col",
    "lookup_source",
    "preferred_description_col",
    "preferred_description_indicator",
    "col_filters"
  )
}

required_mapping_metadata_columns <- function() {
  c(
    "from_code_type",
    "to_code_type",
    "map_version",
    "from_col",
    "to_col",
    "map_source",
    "col_filters"
  )
}

#' Get required relationship metadata column names
#'
#' @return Character vector of required column names
#' @noRd
required_relationship_metadata_columns <- function() {
  c(
    "code_type",
    "relationship_version",
    "from_col",
    "to_col",
    "type_col",
    "child_parent_relationship_code", # Code for child -> parent relationship (e.g. SNOMED 'is a')
    "relationship_source",
    "col_filters"
  )
}

# Connect to the database.
# Read-only callers get the persistent workbench connection (via get_db_con()).
# Write callers get a direct file connection that auto-closes when .envir exits.
# If the workbench has the same file ATTACHed, it is DETACHed first to avoid
# DuckDB file locking, then re-ATTACHed via withr::defer().
connect_to_db <- function(..., read_only = TRUE, .envir = parent.frame()) {
  if (read_only) {
    return(get_db_con())
  }

  target_path <- db_path()

  # Check if the workbench holds this file

  workbench_active <- exists("con", envir = .codeminer_env) &&
    DBI::dbIsValid(.codeminer_env$con)
  workbench_holds_file <- workbench_active &&
    identical(.codeminer_env$db_path, target_path)

  if (workbench_holds_file) {
    # Switch to the in-memory catalog first -- DuckDB won't DETACH the
    # "current" database (which search_path may have set to core).
    DBI::dbExecute(.codeminer_env$con, "USE memory")
    DBI::dbExecute(
      .codeminer_env$con,
      glue::glue_sql(
        "DETACH {`CODEMINER_SCHEMA`}",
        .con = .codeminer_env$con
      )
    )
    .codeminer_env$db_path <- NULL
    withr::defer(
      {
        # Only re-ATTACH if workbench is still active and core is still
        # detached. Another code path (e.g. auto-init) may have already
        # reconnected.
        wb_alive <- exists("con", envir = .codeminer_env) &&
          DBI::dbIsValid(.codeminer_env$con)
        if (wb_alive && is.null(.codeminer_env$db_path)) {
          DBI::dbExecute(
            .codeminer_env$con,
            glue::glue_sql(
              "ATTACH {target_path} AS {`CODEMINER_SCHEMA`} (READ_ONLY)",
              .con = .codeminer_env$con
            )
          )
          .codeminer_env$db_path <- target_path
          codeminer_set_search_path()
          codeminer_refresh_cache()
        }
      },
      envir = .envir
    )
  }

  con <- DBI::dbConnect(duckdb::duckdb(), target_path, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = .envir)
  return(con)
}

# Helper function to get the path to the database file
# Either configured through environment variable or default location
db_path <- function() {
  env_value <- Sys.getenv("CODEMINER_DB_PATH")
  if (env_value != "") {
    return(env_value)
  }

  base <- rappdirs::user_data_dir("codeminer")
  dir.create(base, showWarnings = FALSE) # ensure appdir exists
  return(file.path(base, "ontology.duckdb"))
}
