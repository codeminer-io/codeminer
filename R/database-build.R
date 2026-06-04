# Use locked environment to store global variables
codeminer_metadata_table_names <- new.env(parent = emptyenv())
codeminer_metadata_table_names$lookup <- "_lookup_metadata"
codeminer_metadata_table_names$mapping <- "_mapping_metadata"
codeminer_metadata_table_names$relationship <- "_relationship_metadata"
lockEnvironment(codeminer_metadata_table_names, bindings = TRUE)

#' Build the Codeminer database
#'
#' Set up the codeminer database and create the required lookup and
#' mapping metadata tables.
#'
#' @param overwrite Logical indicating whether to overwrite existing tables (default: `FALSE`)
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
build_database <- function(overwrite = FALSE) {
  db_exists <- file.exists(db_path())
  if (db_exists) {
    codeminer_inform("Existing database found at {.file {db_path()}}")
  } else {
    codeminer_inform("Creating new database at {.file {db_path()}}")
  }

  con <- connect_to_db(read_only = FALSE)

  if (db_exists && overwrite) {
    codeminer_inform("Removing existing tables from database")

    # Get all table names
    tables <- DBI::dbListTables(con)

    # Drop each table
    for (table in tables) {
      DBI::dbRemoveTable(con, table)
    }
  }

  create_lookup_metadata_table(con, overwrite = overwrite)
  create_mapping_metadata_table(con, overwrite = overwrite)
  create_relationship_metadata_table(con, overwrite = overwrite)

  # Migrate existing metadata tables to add any new columns (e.g. col_filters)
  if (db_exists && !overwrite) {
    migrate_metadata_schema(con)
  }

  invisible(TRUE)
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

# Migrate metadata tables to add any missing columns.
# Called during `build_database()` when the database already exists and
# `overwrite = FALSE`, so that older databases gain new columns (e.g.
# col_filters) without requiring a full rebuild.
migrate_metadata_schema <- function(con) {
  meta_tables <- list(
    list(
      tbl = codeminer_metadata_table_names$lookup,
      required = required_lookup_metadata_columns()
    ),
    list(
      tbl = codeminer_metadata_table_names$mapping,
      required = required_mapping_metadata_columns()
    ),
    list(
      tbl = codeminer_metadata_table_names$relationship,
      required = required_relationship_metadata_columns()
    )
  )

  for (entry in meta_tables) {
    if (!table_exists(con, entry$tbl)) {
      next
    }

    existing_cols <- DBI::dbListFields(con, entry$tbl)
    missing_cols <- setdiff(entry$required, existing_cols)

    for (col in missing_cols) {
      DBI::dbExecute(
        con,
        glue::glue_sql(
          "ALTER TABLE {`entry$tbl`} ADD COLUMN {`col`} VARCHAR",
          .con = con
        )
      )
    }
  }

  invisible(TRUE)
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
    identical(.codeminer_env$db_paths$main, target_path)

  if (workbench_holds_file) {
    # Switch to the in-memory catalog first -- DuckDB won't DETACH the
    # "current" database (which search_path may have set to core).
    DBI::dbExecute(.codeminer_env$con, "USE memory")
    DBI::dbExecute(
      .codeminer_env$con,
      glue::glue_sql(
        "DETACH {`CODEMINER_ALIAS_MAIN`}",
        .con = .codeminer_env$con
      )
    )
    .codeminer_env$db_paths$main <- NULL
    withr::defer(
      {
        # Only re-ATTACH if workbench is still active and core is still
        # detached. Another code path (e.g. auto-init) may have already
        # reconnected.
        wb_alive <- exists("con", envir = .codeminer_env) &&
          DBI::dbIsValid(.codeminer_env$con)
        if (wb_alive && is.null(.codeminer_env$db_paths$main)) {
          DBI::dbExecute(
            .codeminer_env$con,
            glue::glue_sql(
              "ATTACH {target_path} AS {`CODEMINER_ALIAS_MAIN`} (READ_ONLY)",
              .con = .codeminer_env$con
            )
          )
          .codeminer_env$db_paths$main <- target_path
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
