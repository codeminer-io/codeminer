# Storage backend abstraction.
#
# Codeminer's "main" database can be one of three shapes:
#
# * `duckdb_file` — a single `.duckdb` file. Read access happens via
#   `ATTACH ... (READ_ONLY)` into the workbench; writes open a short-lived
#   direct DuckDB connection to the file.
#
# * `codeminer_folder` — a directory holding parquet metadata files at the
#   root (`_lookup_metadata.parquet`, `_mapping_metadata.parquet`,
#   `_relationship_metadata.parquet`, `_db_metadata.parquet`) plus one
#   `.duckdb` file per registered data table. Metadata stays as parquet
#   (small, rewritten atomically on each add); data tables get the
#   speed of native DuckDB storage — important for recursive queries
#   like CHILDREN that hit the relationship table repeatedly. This is
#   the default folder layout (`build_database(format = "duckdb")`).
#
# * `parquet_folder` — a directory holding parquet files for both metadata
#   and data tables. Smaller on disk than `codeminer_folder` (parquet's
#   column compression beats DuckDB's row storage on typical lookup
#   tables) but slower for recursive queries since each iteration
#   re-scans the data parquet. Opt in via `build_database(format = "parquet")`.
#
# All callers go through the dispatcher functions in this file; no other
# file should hard-code DuckDB or parquet specifics for the "main" backend.

# Detect which backend a path uses.
#
# Rules:
# * If the path is not an existing directory, treat as `duckdb_file`.
# * Otherwise it's a folder. Peek at the file extensions of data files at
#   the root: only `.duckdb` → `codeminer_folder`; only `.parquet` →
#   `parquet_folder`. For empty / ambiguous folders, fall back to the
#   `storage_format` stamp in `_db_metadata.parquet`. As a last resort
#   (no stamp file either), default to `codeminer_folder`.
backend_kind <- function(path) {
  if (!dir.exists(path)) {
    return("duckdb_file")
  }

  metadata_filenames <- vapply(
    c("lookup", "mapping", "relationship", "db"),
    function(type) paste0(backend_metadata_name(type), ".parquet"),
    character(1)
  )

  files <- list.files(path)
  data_files <- setdiff(files, metadata_filenames)

  any_duckdb <- any(grepl("\\.duckdb$", data_files))
  any_parquet <- any(grepl("\\.parquet$", data_files))

  if (any_duckdb && !any_parquet) {
    return("codeminer_folder")
  }
  if (any_parquet && !any_duckdb) {
    return("parquet_folder")
  }

  # Either empty (just-built folder, no data tables yet) or mixed
  # (manually-edited / cross-format leftovers). Consult the stamp.
  meta_path <- file.path(path, "_db_metadata.parquet")
  if (!file.exists(meta_path)) {
    return("codeminer_folder")
  }
  format <- tryCatch(
    backend_read_storage_format_folder(path),
    error = function(e) NA_character_
  )
  if (identical(format, "parquet_folder")) {
    return("parquet_folder")
  }
  "codeminer_folder"
}

# Read the `storage_format` column from `_db_metadata.parquet`. Returns
# `NA_character_` if the file or column isn't present. Called from
# `backend_kind()` as the tiebreaker for empty/ambiguous folders.
backend_read_storage_format_folder <- function(path) {
  meta <- file.path(path, "_db_metadata.parquet")
  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  row <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT storage_format FROM read_parquet({meta})",
      .con = con
    )
  )
  if (nrow(row) == 0L || is.null(row$storage_format)) {
    return(NA_character_)
  }
  as.character(row$storage_format[[1L]])
}

# Initialise a fresh database at `path` for the given backend.
#
# Behaviour matches the existing `build_database()` semantics:
# * If the DB already exists and `overwrite = FALSE`, the caller should
#   short-circuit; this function only runs on a fresh build or overwrite.
# * Creates the metadata tables and writes the initial `_db_metadata`
#   stamp row at the current schema version.
backend_init <- function(path, overwrite = FALSE, storage_format = NULL) {
  # `storage_format` is the *intended* layout for the (possibly
  # not-yet-built) DB; it lets `build_database()` say "I want to make
  # this folder a parquet_folder" even when the directory is currently
  # empty (where `backend_kind()` would default to `codeminer_folder`).
  if (is.null(storage_format)) {
    storage_format <- backend_kind(path)
  }
  switch(
    storage_format,
    duckdb_file = backend_init_duckdb_file(path, overwrite = overwrite),
    codeminer_folder = backend_init_codeminer_folder(path, overwrite = overwrite),
    parquet_folder = backend_init_parquet_folder(path, overwrite = overwrite),
    codeminer_abort("Unknown storage format {.val {storage_format}}.")
  )
}

# Make the database at `path` queryable from the workbench connection
# under the catalog/schema alias `alias`. Idempotent: callers may invoke
# this after writes to refresh views over newly-added data tables.
backend_attach <- function(con, alias, path) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_attach_duckdb_file(con, alias, path),
    codeminer_folder = backend_attach_codeminer_folder(con, alias, path),
    parquet_folder = backend_attach_parquet_folder(con, alias, path),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Read the on-disk schema version stamp from `path`. Returns
# `NA_integer_` for an unstamped DB (no `_db_metadata` table). Used by
# `enforce_schema_gate()`.
backend_read_schema_version <- function(path) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_read_schema_version_duckdb_file(path),
    codeminer_folder = backend_read_schema_version_codeminer_folder(path),
    parquet_folder = backend_read_schema_version_codeminer_folder(path),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Atomically add a (metadata_row, data_df) pair to the database at `path`.
#
# Returns TRUE on success, FALSE if a row with the same `<id_col>` value
# already exists in the metadata table (matching the existing
# `add_metadata_table()` semantics).
#
# `type` is "lookup", "mapping" or "relationship". `metadata_row` is a
# one-row data frame whose columns are the required metadata fields.
# `data_df` is the data table (already validated by the caller).
backend_add_table <- function(path, type, metadata_row, data_df) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_add_table_duckdb_file(
      path,
      type,
      metadata_row,
      data_df
    ),
    codeminer_folder = backend_add_table_codeminer_folder(
      path,
      type,
      metadata_row,
      data_df
    ),
    parquet_folder = backend_add_table_parquet_folder(
      path,
      type,
      metadata_row,
      data_df
    ),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Remove a data table and its metadata row from the database. Returns
# TRUE invisibly on success; raises an error if the metadata row isn't
# present.
backend_remove_table <- function(path, type, table_name) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_remove_table_duckdb_file(path, type, table_name),
    codeminer_folder = backend_remove_table_codeminer_folder(
      path,
      type,
      table_name
    ),
    parquet_folder = backend_remove_table_parquet_folder(
      path,
      type,
      table_name
    ),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Update a single column of a single metadata row. Used by the
# `update_*_metadata()` helpers (currently only `col_filters`).
backend_update_metadata <- function(path, type, table_name, col, value) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_update_metadata_duckdb_file(
      path,
      type,
      table_name,
      col,
      value
    ),
    codeminer_folder = backend_update_metadata_codeminer_folder(
      path,
      type,
      table_name,
      col,
      value
    ),
    parquet_folder = backend_update_metadata_codeminer_folder(
      path,
      type,
      table_name,
      col,
      value
    ),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Read a complete metadata table from the database as a data frame.
# Used by the parquet backend's add/remove/update helpers (where the
# whole metadata file is rewritten on each change) and as a fallback
# when the workbench cache isn't populated.
backend_read_metadata <- function(path, type) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_read_metadata_duckdb_file(path, type),
    codeminer_folder = backend_read_metadata_codeminer_folder(path, type),
    parquet_folder = backend_read_metadata_codeminer_folder(path, type),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Check whether the database at `path` already contains a top-level
# table named `name` (either a real DuckDB table or a parquet file).
backend_table_exists <- function(path, name) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_table_exists_duckdb_file(path, name),
    codeminer_folder = backend_table_exists_codeminer_folder(path, name),
    parquet_folder = backend_table_exists_parquet_folder(path, name),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

# Prepare the workbench connection so that a write against `path` can
# proceed, and schedule the workbench to be re-attached + cache-refreshed
# when the caller's frame exits.
#
# For `duckdb_file`: DETACHes the file from the workbench (matching the
# behaviour of the old `connect_to_db(read_only = FALSE)`) so that the
# write side can open the file in write-mode without a lock conflict.
# A `withr::defer()` re-ATTACHes and refreshes the metadata cache.
#
# For `codeminer_folder`: no DETACH needed — parquet writes don't take a
# lock that conflicts with the workbench's read-only views. A
# `withr::defer()` recreates the views (so newly-added data tables are
# visible) and refreshes the metadata cache.
acquire_writable_workbench <- function(path, .envir = parent.frame()) {
  kind <- backend_kind(path)

  workbench_active <- exists("con", envir = .codeminer_env) &&
    DBI::dbIsValid(.codeminer_env$con)
  workbench_holds_file <- workbench_active &&
    identical(.codeminer_env$db_paths$main, path)

  if (kind == "duckdb_file" && workbench_holds_file) {
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
        wb_alive <- exists("con", envir = .codeminer_env) &&
          DBI::dbIsValid(.codeminer_env$con)
        if (wb_alive && is.null(.codeminer_env$db_paths$main)) {
          backend_attach(.codeminer_env$con, CODEMINER_ALIAS_MAIN, path)
          .codeminer_env$db_paths$main <- path
          codeminer_set_search_path()
          codeminer_refresh_cache()
        }
      },
      envir = .envir
    )
  } else if (
    kind %in% c("codeminer_folder", "parquet_folder") && workbench_holds_file
  ) {
    withr::defer(
      {
        wb_alive <- exists("con", envir = .codeminer_env) &&
          DBI::dbIsValid(.codeminer_env$con)
        if (wb_alive) {
          # Idempotent: CREATE OR REPLACE VIEW handles existing names and
          # picks up the new data table view.
          backend_attach(.codeminer_env$con, CODEMINER_ALIAS_MAIN, path)
          codeminer_refresh_cache()
        }
      },
      envir = .envir
    )
  }
  invisible()
}

# Validate a single metadata row before it is sent to a backend. Centralises
# the "reserved name / forbidden character" checks that used to live in
# `add_metadata_table()` so each backend's `backend_add_table_*` just
# performs the write.
validate_metadata_row <- function(metadata_row, type) {
  id_col <- backend_id_col(type)
  ids <- metadata_row[[id_col]]
  if (is.null(ids)) {
    codeminer_abort("Missing field {id_col} in metadata.")
  }

  version_col <- switch(
    type,
    lookup = "lookup_version",
    mapping = "map_version",
    relationship = "relationship_version"
  )
  versions <- metadata_row[[version_col]]
  if (!is.null(versions) && any(versions == "latest")) {
    codeminer_abort(
      "{.val latest} is a reserved version name and cannot be used."
    )
  }

  code_type_cols <- switch(
    type,
    lookup = "code_type",
    mapping = c("from_code_type", "to_code_type"),
    relationship = "code_type"
  )
  for (col in code_type_cols) {
    vals <- metadata_row[[col]]
    if (!is.null(vals) && any(grepl(">", vals, fixed = TRUE))) {
      codeminer_abort(
        "Code type values must not contain {.val >} (found in column {.field {col}})."
      )
    }
  }
  invisible(metadata_row)
}

# Shared helpers ----------------------------------------------------------

# Internal: resolve the primary-key column name for a metadata type.
backend_id_col <- function(type) {
  switch(
    type,
    lookup = "lookup_table_name",
    mapping = "mapping_table_name",
    relationship = "relationship_table_name",
    codeminer_abort("Invalid metadata type: {.val {type}}.")
  )
}

# Internal: the four metadata "table" names indexed by type. Reuses the
# locked environment from `database-build.R` so the values stay in sync
# across the package.
backend_metadata_name <- function(type) {
  codeminer_metadata_table_names[[type]]
}

# DuckDB-file backend ------------------------------------------------------

backend_init_duckdb_file <- function(path, overwrite = FALSE) {
  db_exists <- file.exists(path)

  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  if (db_exists && overwrite) {
    codeminer_inform("Removing existing tables from database")
    for (table in DBI::dbListTables(con)) {
      DBI::dbRemoveTable(con, table)
    }
  }

  create_lookup_metadata_table(con, overwrite = overwrite)
  create_mapping_metadata_table(con, overwrite = overwrite)
  create_relationship_metadata_table(con, overwrite = overwrite)
  create_db_metadata_table(con, overwrite = overwrite)
  DBI::dbWriteTable(
    con,
    name = codeminer_metadata_table_names$db,
    value = codeminer_initial_stamp_row(storage_format = "duckdb_file"),
    append = TRUE
  )
  invisible(TRUE)
}

backend_attach_duckdb_file <- function(con, alias, path) {
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "ATTACH {path} AS {`alias`} (READ_ONLY)",
      .con = con
    )
  )
  invisible(TRUE)
}

backend_read_schema_version_duckdb_file <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  read_db_schema_version(con)
}

backend_add_table_duckdb_file <- function(path, type, metadata_row, data_df) {
  validate_metadata_row(metadata_row, type)

  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  tbl_name <- backend_metadata_name(type)
  id_col <- backend_id_col(type)

  # Uniqueness check (metadata-only, matching the parquet backend's rule).
  current <- read_table_from_db(con, tbl_name)
  if (any(metadata_row[[id_col]] %in% current[[id_col]])) {
    return(invisible(FALSE))
  }

  table_name <- metadata_row[[id_col]][[1]]

  # Wrap the metadata-row INSERT + data-table CREATE in a single
  # transaction so a mid-write failure leaves no orphan rows or tables.
  DBI::dbExecute(con, "BEGIN")
  on_error <- function(e) {
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    stop(e)
  }
  tryCatch(
    {
      DBI::dbAppendTable(con, tbl_name, as.data.frame(metadata_row))
      DBI::dbWriteTable(
        con,
        name = table_name,
        value = data_df,
        overwrite = FALSE
      )
      DBI::dbExecute(con, "COMMIT")
    },
    error = on_error
  )
  invisible(TRUE)
}

backend_table_exists_duckdb_file <- function(path, name) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  table_exists(con, name)
}

backend_remove_table_duckdb_file <- function(path, type, table_name) {
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  remove_table_entry(con, type, table_name)
  invisible(TRUE)
}

backend_update_metadata_duckdb_file <- function(
  path,
  type,
  table_name,
  col,
  value
) {
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  tbl <- backend_metadata_name(type)
  id_col <- backend_id_col(type)
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "UPDATE {`tbl`} SET {`col`} = {value} WHERE {`id_col`} = {table_name}",
      .con = con
    )
  )
  invisible(TRUE)
}

backend_read_metadata_duckdb_file <- function(path, type) {
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  read_table_from_db(con, backend_metadata_name(type))
}

# Parquet-folder backend ---------------------------------------------------

# Path of the parquet file backing a metadata "table" in folder mode.
backend_meta_path <- function(path, type) {
  file.path(path, paste0(backend_metadata_name(type), ".parquet"))
}

# Path of the DuckDB file backing a data table in folder mode.
backend_data_path <- function(path, table_name) {
  file.path(path, paste0(table_name, ".duckdb"))
}

# Deterministic ATTACH alias for a data table's `.duckdb` file. The
# `_t_` prefix ensures the catalog name can never shadow an unqualified
# `SELECT * FROM <table_name>` against the workbench's `core.main`
# search path.
backend_data_alias <- function(table_name) {
  paste0("_t_", table_name)
}

# Drive parquet reads/writes through a transient in-memory DuckDB
# connection. The workbench connection could be used, but using a
# scratch connection avoids polluting the workbench with helper tables
# and keeps backend writes independent of the workbench's state.
backend_pq_con <- function() {
  DBI::dbConnect(duckdb::duckdb(), ":memory:")
}

# Write a data frame to `target_path` as a parquet file, atomically:
# COPY into `target_path.tmp`, then `file.rename()` to `target_path`.
# Returns the final path invisibly.
backend_write_parquet_atomic <- function(con, df, target_path) {
  tmp_path <- paste0(target_path, ".tmp")
  if (file.exists(tmp_path)) {
    unlink(tmp_path)
  }
  scratch <- paste0(
    "_codeminer_backend_write_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sample.int(.Machine$integer.max, 1L)
  )
  DBI::dbWriteTable(con, scratch, as.data.frame(df), overwrite = TRUE)
  withr::defer(try(DBI::dbRemoveTable(con, scratch), silent = TRUE))
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "COPY (SELECT * FROM {`scratch`}) TO {tmp_path} (FORMAT PARQUET)",
      .con = con
    )
  )
  file.rename(tmp_path, target_path)
  invisible(target_path)
}

# Required-column accessor for the given metadata type.
backend_required_cols <- function(type) {
  switch(
    type,
    lookup = required_lookup_metadata_columns(),
    mapping = required_mapping_metadata_columns(),
    relationship = required_relationship_metadata_columns(),
    db = required_db_metadata_columns(),
    codeminer_abort("Invalid metadata type: {.val {type}}.")
  )
}

# Build a zero-row data frame with the schema of a metadata type so that
# `backend_init_codeminer_folder()` can produce empty parquet files with
# the correct columns.
backend_empty_metadata <- function(type) {
  cols <- backend_required_cols(type)
  if (type %in% c("lookup", "mapping", "relationship")) {
    id_col <- backend_id_col(type)
    cols <- c(id_col, cols)
  }
  df <- as.data.frame(
    stats::setNames(
      replicate(length(cols), character(0), simplify = FALSE),
      cols
    ),
    stringsAsFactors = FALSE
  )
  df
}

backend_init_codeminer_folder <- function(path, overwrite = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (overwrite) {
    existing <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)
    if (length(existing) > 0) {
      codeminer_inform("Removing existing parquet files from {.file {path}}")
      unlink(existing)
    }
    # Stray .tmp files from a previously-interrupted write are also
    # cleared so the fresh build starts from a clean directory.
    leftover_tmp <- list.files(
      path,
      pattern = "\\.parquet\\.tmp$",
      full.names = TRUE
    )
    if (length(leftover_tmp) > 0) {
      unlink(leftover_tmp)
    }
  }

  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  # Write empty metadata parquets for the per-table metadata types.
  for (type in c("lookup", "mapping", "relationship")) {
    target <- backend_meta_path(path, type)
    if (overwrite || !file.exists(target)) {
      backend_write_parquet_atomic(
        con,
        backend_empty_metadata(type),
        target
      )
    }
  }

  # Stamp the _db_metadata file at the current schema version.
  db_target <- backend_meta_path(path, "db")
  if (overwrite || !file.exists(db_target)) {
    backend_write_parquet_atomic(
      con,
      codeminer_initial_stamp_row(storage_format = "codeminer_folder"),
      db_target
    )
  }

  invisible(TRUE)
}

backend_attach_codeminer_folder <- function(con, alias, path) {
  # ATTACH an in-memory catalog under the same alias the duckdb_file
  # backend uses. This keeps object addressing symmetric across the two
  # backends — both end up as `<alias>.main.<table>` — so the rest of
  # the package (search_path, schema_table_exists, etc.) needs no
  # backend-specific branching.
  catalogs <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT database_name FROM duckdb_databases() WHERE database_name = {alias}",
      .con = con
    )
  )
  if (nrow(catalogs) == 0L) {
    DBI::dbExecute(
      con,
      glue::glue_sql("ATTACH ':memory:' AS {`alias`}", .con = con)
    )
  }

  # Views over each metadata file. One file per metadata type in folder
  # mode — `read_parquet()` on the single file returns the metadata
  # rows.
  for (type in c("lookup", "mapping", "relationship", "db")) {
    meta_path <- backend_meta_path(path, type)
    if (!file.exists(meta_path)) {
      next
    }
    view_name <- backend_metadata_name(type)
    DBI::dbExecute(
      con,
      glue::glue_sql(
        "CREATE OR REPLACE VIEW {`alias`}.main.{`view_name`} AS
         SELECT * FROM read_parquet({meta_path})",
        .con = con
      )
    )
  }

  # Views over each registered data table. Each `<name>.duckdb` is
  # ATTACHed as a separate catalog with a deterministic `_t_<name>`
  # alias, then projected into `core.main.<name>` via a view so the
  # rest of the package addresses tables the same way as in
  # `duckdb_file` mode. Names are enumerated from the metadata files
  # so stray `.duckdb` files left by a previous interrupted add
  # (orphans) don't get exposed.
  attached_catalogs <- DBI::dbGetQuery(
    con,
    "SELECT database_name FROM duckdb_databases()"
  )$database_name

  for (type in c("lookup", "mapping", "relationship")) {
    meta_path <- backend_meta_path(path, type)
    if (!file.exists(meta_path)) {
      next
    }
    meta_df <- backend_read_metadata_codeminer_folder(path, type)
    id_col <- backend_id_col(type)
    table_names <- meta_df[[id_col]]
    for (tbl in table_names) {
      data_path <- backend_data_path(path, tbl)
      if (!file.exists(data_path)) {
        next
      }
      data_alias <- backend_data_alias(tbl)
      if (!data_alias %in% attached_catalogs) {
        DBI::dbExecute(
          con,
          glue::glue_sql(
            "ATTACH {data_path} AS {`data_alias`} (READ_ONLY)",
            .con = con
          )
        )
        attached_catalogs <- c(attached_catalogs, data_alias)
      }
      DBI::dbExecute(
        con,
        glue::glue_sql(
          "CREATE OR REPLACE VIEW {`alias`}.main.{`tbl`} AS
           SELECT * FROM {`data_alias`}.main.{`tbl`}",
          .con = con
        )
      )
    }
  }
  invisible(TRUE)
}

backend_read_schema_version_codeminer_folder <- function(path) {
  db_path_file <- backend_meta_path(path, "db")
  if (!file.exists(db_path_file)) {
    return(NA_integer_)
  }
  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  row <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT schema_version FROM read_parquet({db_path_file})",
      .con = con
    )
  )
  if (nrow(row) == 0L) {
    return(NA_integer_)
  }
  as.integer(row$schema_version[[1L]])
}

backend_read_metadata_codeminer_folder <- function(path, type) {
  meta_path <- backend_meta_path(path, type)
  if (!file.exists(meta_path)) {
    return(backend_empty_metadata(type))
  }
  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT * FROM read_parquet({meta_path})",
      .con = con
    )
  )
}

# Four-step transactional add: write metadata.tmp, write data.tmp, rename
# data, rename metadata. If the metadata rename fails after a successful
# data rename, the data file is unlinked to roll the data commit back.
# An R-process death between step 3 and step 4 leaves an orphan data
# file; that's invisible to readers (no metadata row references it) and
# gets overwritten cleanly on the next add of the same name.
backend_table_exists_codeminer_folder <- function(path, name) {
  # Metadata "tables" map to `_*_metadata.parquet`; data tables map to
  # `<name>.duckdb` at the folder root (the `.duckdb` extension is
  # encoded in `backend_data_path()`).
  if (
    name %in%
      vapply(
        c("lookup", "mapping", "relationship", "db"),
        backend_metadata_name,
        character(1)
      )
  ) {
    return(file.exists(file.path(path, paste0(name, ".parquet"))))
  }
  file.exists(backend_data_path(path, name))
}

backend_add_table_codeminer_folder <- function(
  path,
  type,
  metadata_row,
  data_df
) {
  validate_metadata_row(metadata_row, type)

  id_col <- backend_id_col(type)
  table_name <- metadata_row[[id_col]][[1]]

  # Uniqueness check is metadata-only — never `file.exists()` on the data
  # path — so an orphan from a previous crashed add doesn't block a re-add.
  current_meta <- backend_read_metadata_codeminer_folder(path, type)
  if (table_name %in% current_meta[[id_col]]) {
    return(invisible(FALSE))
  }

  meta_path <- backend_meta_path(path, type)
  data_path <- backend_data_path(path, table_name)
  meta_tmp <- paste0(meta_path, ".tmp")
  data_tmp <- paste0(data_path, ".tmp")

  pq_con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(pq_con, shutdown = TRUE))

  # Step 1: write metadata temp (parquet, same as before).
  combined_meta <- dplyr::bind_rows(current_meta, as.data.frame(metadata_row))
  backend_pq_write_to_tmp(pq_con, combined_meta, meta_tmp)

  committed_meta_tmp <- FALSE
  withr::defer(
    if (!committed_meta_tmp && file.exists(meta_tmp)) unlink(meta_tmp)
  )

  # Step 2: write data temp as a fresh DuckDB file. The table inside
  # is named the same as the file's basename (sans .duckdb) so the
  # attach path can address it as `<alias>.main.<table_name>`.
  if (file.exists(data_tmp)) {
    unlink(data_tmp)
  }
  data_con <- DBI::dbConnect(duckdb::duckdb(), data_tmp, read_only = FALSE)
  DBI::dbWriteTable(
    data_con,
    name = table_name,
    value = as.data.frame(data_df),
    overwrite = FALSE
  )
  DBI::dbDisconnect(data_con, shutdown = TRUE)

  committed_data_tmp <- FALSE
  withr::defer(
    if (!committed_data_tmp && file.exists(data_tmp)) unlink(data_tmp)
  )

  # Step 3: commit data. Atomic on POSIX. Once this returns, the data
  # file is the new file; the metadata still references the old set.
  file.rename(data_tmp, data_path)
  committed_data_tmp <- TRUE
  data_committed <- TRUE

  # Step 4: commit metadata. If the rename fails, roll back the data
  # commit so we don't leak an orphan.
  tryCatch(
    file.rename(meta_tmp, meta_path),
    error = function(e) {
      if (data_committed && file.exists(data_path)) {
        unlink(data_path)
      }
      stop(e)
    }
  )
  committed_meta_tmp <- TRUE

  invisible(TRUE)
}

# Internal helper: write a data frame as parquet to an explicit path
# (typically a `.tmp` file). Reuses a scratch table in `con` and removes
# it after the COPY. Does NOT rename — callers control the commit.
backend_pq_write_to_tmp <- function(con, df, tmp_path) {
  if (file.exists(tmp_path)) {
    unlink(tmp_path)
  }
  scratch <- paste0(
    "_codeminer_backend_write_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sample.int(.Machine$integer.max, 1L)
  )
  DBI::dbWriteTable(con, scratch, as.data.frame(df), overwrite = TRUE)
  withr::defer(try(DBI::dbRemoveTable(con, scratch), silent = TRUE))
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "COPY (SELECT * FROM {`scratch`}) TO {tmp_path} (FORMAT PARQUET)",
      .con = con
    )
  )
  invisible(tmp_path)
}

backend_remove_table_codeminer_folder <- function(path, type, table_name) {
  current_meta <- backend_read_metadata_codeminer_folder(path, type)
  id_col <- backend_id_col(type)
  if (!table_name %in% current_meta[[id_col]]) {
    codeminer_abort(
      "No {type} metadata row found for {.val {table_name}}."
    )
  }

  new_meta <- current_meta[current_meta[[id_col]] != table_name, , drop = FALSE]

  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  backend_write_parquet_atomic(
    con,
    new_meta,
    backend_meta_path(path, type)
  )

  data_path <- backend_data_path(path, table_name)
  if (file.exists(data_path)) {
    unlink(data_path)
  }
  invisible(TRUE)
}

# Validate the database at `path`, returning a (possibly empty) named
# list of issues. Backend-aware: see the per-backend implementations
# for what each one checks. Used by the public `validate_database()`
# wrapper.
backend_validate <- function(path) {
  kind <- backend_kind(path)
  switch(
    kind,
    duckdb_file = backend_validate_duckdb_file(path),
    codeminer_folder = backend_validate_codeminer_folder(path),
    parquet_folder = backend_validate_parquet_folder(path),
    codeminer_abort("Unknown backend kind {.val {kind}}.")
  )
}

#' Validate the codeminer database for on-disk inconsistencies
#'
#' Inspects the database at `CODEMINER_DB_PATH` and reports any
#' inconsistencies it finds — without modifying the database. The
#' specific checks depend on the backend:
#'
#' * `codeminer_folder`:
#'   * **orphan data files**: `<name>.duckdb` files at the folder root
#'     with no matching metadata row (typically left by a previous
#'     `add_*_table()` that died after the data file was committed but
#'     before the metadata file was).
#'   * **dangling metadata**: metadata rows that reference a data file
#'     that does not exist.
#'   * **stale temp files**: `*.parquet.tmp` (metadata) or
#'     `*.duckdb.tmp` (data) files left over from an interrupted
#'     write.
#' * `duckdb_file`:
#'   * **dangling metadata**: metadata rows referencing data tables
#'     that do not exist in the file.
#'
#' Issues are reported via informational messages. The function
#' returns a named list of issues invisibly so callers can act on
#' them programmatically.
#'
#' @return A named list of character vectors, one entry per kind of
#'   issue. Empty vectors mean no issues of that kind were found.
#' @export
#' @family Database management
#' @examples
#' \dontrun{
#' issues <- validate_database()
#' }
validate_database <- function() {
  path <- db_path()
  if (!backend_database_exists(path)) {
    codeminer_inform(c(
      "i" = "No database exists at {.file {path}}; nothing to validate."
    ))
    return(invisible(list()))
  }

  issues <- backend_validate(path)
  any_issues <- any(vapply(issues, function(x) length(x) > 0, logical(1)))

  if (!any_issues) {
    codeminer_inform(c(
      "v" = "Database at {.file {path}} looks consistent."
    ))
    return(invisible(issues))
  }

  msgs <- c("!" = "Database at {.file {path}} has issues:")
  if (length(issues$orphan_data_files %||% character(0)) > 0) {
    msgs <- c(
      msgs,
      "x" = paste(
        "Orphan data files (parquet files with no metadata row): ",
        "{.val {issues$orphan_data_files}}"
      )
    )
  }
  if (length(issues$dangling_metadata %||% character(0)) > 0) {
    msgs <- c(
      msgs,
      "x" = paste(
        "Dangling metadata rows (referenced data table missing): ",
        "{.val {issues$dangling_metadata}}"
      )
    )
  }
  if (length(issues$stale_temp_files %||% character(0)) > 0) {
    msgs <- c(
      msgs,
      "x" = "Stale temp files: {.val {issues$stale_temp_files}}"
    )
  }
  codeminer_inform(msgs)
  invisible(issues)
}

# DuckDB-file validation: the only inconsistency that can realistically
# happen is a metadata row whose referenced data table is missing
# (e.g. someone called DBI::dbRemoveTable() directly). Orphan data
# tables are difficult to identify generically because the duckdb_file
# also contains the metadata tables themselves.
backend_validate_duckdb_file <- function(path) {
  issues <- list(
    dangling_metadata = character(0)
  )
  if (!file.exists(path)) {
    return(issues)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  existing_tables <- DBI::dbListTables(con)
  for (type in c("lookup", "mapping", "relationship")) {
    tbl <- backend_metadata_name(type)
    if (!tbl %in% existing_tables) {
      next
    }
    id_col <- backend_id_col(type)
    rows <- DBI::dbGetQuery(
      con,
      glue::glue_sql("SELECT {`id_col`} AS name FROM {`tbl`}", .con = con)
    )
    missing <- setdiff(rows$name, existing_tables)
    if (length(missing) > 0) {
      issues$dangling_metadata <- c(issues$dangling_metadata, missing)
    }
  }
  issues
}

# codeminer_folder validation:
#  * orphan_data_files — `<name>.duckdb` files at root with no matching
#    metadata row (typically left by a crashed add between step 3 and
#    step 4).
#  * dangling_metadata — metadata rows whose `<name>.duckdb` data file
#    is missing.
#  * stale_temp_files — `*.parquet.tmp` (metadata) or `*.duckdb.tmp`
#    (data) files left over from an interrupted write.
backend_validate_codeminer_folder <- function(path) {
  issues <- list(
    orphan_data_files = character(0),
    dangling_metadata = character(0),
    stale_temp_files = character(0)
  )
  if (!dir.exists(path)) {
    return(issues)
  }

  duckdb_files <- list.files(path, pattern = "\\.duckdb$")
  data_table_names <- sub("\\.duckdb$", "", duckdb_files)

  registered <- character(0)
  for (type in c("lookup", "mapping", "relationship")) {
    meta <- backend_read_metadata_codeminer_folder(path, type)
    id_col <- backend_id_col(type)
    registered <- c(registered, meta[[id_col]])
  }
  registered <- unique(registered)

  issues$orphan_data_files <- setdiff(data_table_names, registered)
  issues$dangling_metadata <- setdiff(registered, data_table_names)
  issues$stale_temp_files <- list.files(
    path,
    pattern = "\\.(parquet|duckdb)\\.tmp$"
  )

  issues
}

backend_update_metadata_codeminer_folder <- function(
  path,
  type,
  table_name,
  col,
  value
) {
  current_meta <- backend_read_metadata_codeminer_folder(path, type)
  id_col <- backend_id_col(type)
  if (!table_name %in% current_meta[[id_col]]) {
    codeminer_abort(
      "No {type} metadata row found for {.val {table_name}}."
    )
  }
  current_meta[[col]][current_meta[[id_col]] == table_name] <- value

  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  backend_write_parquet_atomic(
    con,
    current_meta,
    backend_meta_path(path, type)
  )
  invisible(TRUE)
}

# Parquet-folder backend ---------------------------------------------------
#
# Data tables live alongside the metadata parquets at the folder root,
# as `<name>.parquet` files. Both the data and metadata write paths use
# the same write-to-temp + atomic-rename transaction pattern.

# Path of the parquet file backing a data table in parquet_folder mode.
backend_pq_data_path <- function(path, table_name) {
  file.path(path, paste0(table_name, ".parquet"))
}

backend_init_parquet_folder <- function(path, overwrite = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (overwrite) {
    existing <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)
    if (length(existing) > 0) {
      codeminer_inform("Removing existing parquet files from {.file {path}}")
      unlink(existing)
    }
    leftover_tmp <- list.files(
      path,
      pattern = "\\.parquet\\.tmp$",
      full.names = TRUE
    )
    if (length(leftover_tmp) > 0) {
      unlink(leftover_tmp)
    }
  }

  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  for (type in c("lookup", "mapping", "relationship")) {
    target <- backend_meta_path(path, type)
    if (overwrite || !file.exists(target)) {
      backend_write_parquet_atomic(
        con,
        backend_empty_metadata(type),
        target
      )
    }
  }

  db_target <- backend_meta_path(path, "db")
  if (overwrite || !file.exists(db_target)) {
    backend_write_parquet_atomic(
      con,
      codeminer_initial_stamp_row(storage_format = "parquet_folder"),
      db_target
    )
  }

  invisible(TRUE)
}

backend_attach_parquet_folder <- function(con, alias, path) {
  # Same in-memory catalog setup as codeminer_folder so that the rest
  # of the package addresses tables as `<alias>.main.<table>`.
  catalogs <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT database_name FROM duckdb_databases() WHERE database_name = {alias}",
      .con = con
    )
  )
  if (nrow(catalogs) == 0L) {
    DBI::dbExecute(
      con,
      glue::glue_sql("ATTACH ':memory:' AS {`alias`}", .con = con)
    )
  }

  # Metadata views (parquet — same as codeminer_folder).
  for (type in c("lookup", "mapping", "relationship", "db")) {
    meta_path <- backend_meta_path(path, type)
    if (!file.exists(meta_path)) {
      next
    }
    view_name <- backend_metadata_name(type)
    DBI::dbExecute(
      con,
      glue::glue_sql(
        "CREATE OR REPLACE VIEW {`alias`}.main.{`view_name`} AS
         SELECT * FROM read_parquet({meta_path})",
        .con = con
      )
    )
  }

  # Data table views point at `<name>.parquet` via read_parquet — no
  # ATTACH needed, so no `_t_<name>` aliases. Enumerated from metadata
  # to skip orphan files.
  for (type in c("lookup", "mapping", "relationship")) {
    meta_path <- backend_meta_path(path, type)
    if (!file.exists(meta_path)) {
      next
    }
    meta_df <- backend_read_metadata_codeminer_folder(path, type)
    id_col <- backend_id_col(type)
    table_names <- meta_df[[id_col]]
    for (tbl in table_names) {
      data_path <- backend_pq_data_path(path, tbl)
      if (!file.exists(data_path)) {
        next
      }
      DBI::dbExecute(
        con,
        glue::glue_sql(
          "CREATE OR REPLACE VIEW {`alias`}.main.{`tbl`} AS
           SELECT * FROM read_parquet({data_path})",
          .con = con
        )
      )
    }
  }
  invisible(TRUE)
}

backend_table_exists_parquet_folder <- function(path, name) {
  if (
    name %in%
      vapply(
        c("lookup", "mapping", "relationship", "db"),
        backend_metadata_name,
        character(1)
      )
  ) {
    return(file.exists(file.path(path, paste0(name, ".parquet"))))
  }
  file.exists(backend_pq_data_path(path, name))
}

backend_add_table_parquet_folder <- function(
  path,
  type,
  metadata_row,
  data_df
) {
  validate_metadata_row(metadata_row, type)

  id_col <- backend_id_col(type)
  table_name <- metadata_row[[id_col]][[1]]

  current_meta <- backend_read_metadata_codeminer_folder(path, type)
  if (table_name %in% current_meta[[id_col]]) {
    return(invisible(FALSE))
  }

  meta_path <- backend_meta_path(path, type)
  data_path <- backend_pq_data_path(path, table_name)
  meta_tmp <- paste0(meta_path, ".tmp")
  data_tmp <- paste0(data_path, ".tmp")

  pq_con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(pq_con, shutdown = TRUE))

  # Step 1: write metadata temp.
  combined_meta <- dplyr::bind_rows(current_meta, as.data.frame(metadata_row))
  backend_pq_write_to_tmp(pq_con, combined_meta, meta_tmp)
  committed_meta_tmp <- FALSE
  withr::defer(
    if (!committed_meta_tmp && file.exists(meta_tmp)) unlink(meta_tmp)
  )

  # Step 2: write data temp (also parquet).
  backend_pq_write_to_tmp(pq_con, data_df, data_tmp)
  committed_data_tmp <- FALSE
  withr::defer(
    if (!committed_data_tmp && file.exists(data_tmp)) unlink(data_tmp)
  )

  # Step 3: commit data.
  file.rename(data_tmp, data_path)
  committed_data_tmp <- TRUE
  data_committed <- TRUE

  # Step 4: commit metadata; roll back data on failure.
  tryCatch(
    file.rename(meta_tmp, meta_path),
    error = function(e) {
      if (data_committed && file.exists(data_path)) {
        unlink(data_path)
      }
      stop(e)
    }
  )
  committed_meta_tmp <- TRUE
  invisible(TRUE)
}

backend_remove_table_parquet_folder <- function(path, type, table_name) {
  current_meta <- backend_read_metadata_codeminer_folder(path, type)
  id_col <- backend_id_col(type)
  if (!table_name %in% current_meta[[id_col]]) {
    codeminer_abort(
      "No {type} metadata row found for {.val {table_name}}."
    )
  }
  new_meta <- current_meta[current_meta[[id_col]] != table_name, , drop = FALSE]

  con <- backend_pq_con()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  backend_write_parquet_atomic(
    con,
    new_meta,
    backend_meta_path(path, type)
  )

  data_path <- backend_pq_data_path(path, table_name)
  if (file.exists(data_path)) {
    unlink(data_path)
  }
  invisible(TRUE)
}

# parquet_folder validation. Same three issue types as codeminer_folder
# but data files are `.parquet` and the stale-temp pattern only needs to
# catch `.parquet.tmp` (both metadata and data writes use it).
backend_validate_parquet_folder <- function(path) {
  issues <- list(
    orphan_data_files = character(0),
    dangling_metadata = character(0),
    stale_temp_files = character(0)
  )
  if (!dir.exists(path)) {
    return(issues)
  }

  metadata_filenames <- vapply(
    c("lookup", "mapping", "relationship", "db"),
    function(type) paste0(backend_metadata_name(type), ".parquet"),
    character(1)
  )

  parquet_files <- list.files(path, pattern = "\\.parquet$")
  data_files <- setdiff(parquet_files, metadata_filenames)
  data_table_names <- sub("\\.parquet$", "", data_files)

  registered <- character(0)
  for (type in c("lookup", "mapping", "relationship")) {
    meta <- backend_read_metadata_codeminer_folder(path, type)
    id_col <- backend_id_col(type)
    registered <- c(registered, meta[[id_col]])
  }
  registered <- unique(registered)

  issues$orphan_data_files <- setdiff(data_table_names, registered)
  issues$dangling_metadata <- setdiff(registered, data_table_names)
  issues$stale_temp_files <- list.files(path, pattern = "\\.parquet\\.tmp$")

  issues
}
