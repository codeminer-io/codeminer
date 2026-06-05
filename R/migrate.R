# Registered migrations + the public `migrate_database()` entry point.
#
# A migration is a small list with fields:
#   - from        integer source schema version
#   - to          integer target schema version
#   - mode        one of "auto_additive", "manual_additive", "breaking"
#   - description one-line string shown in inform/error messages
#   - up          function(con) — applies the migration
#
# Modes:
#   * auto_additive   — applied silently on connect when no other migration on
#                       the chain is non-auto. Examples: a new optional column
#                       with NULL default, a new index.
#   * manual_additive — additive but requires user action (e.g. needs a
#                       backfill from external data). Connect refuses, points
#                       at `migrate_database()`.
#   * breaking        — renames, retypes, drops. Connect refuses, points at
#                       `migrate_database()`; user may need to back up first.
#
# To register a new migration: append an entry to `codeminer_migrations()`,
# bump `current_schema_version()`. See CLAUDE.md.

codeminer_migrations <- function() {
  list(
    list(
      from = 0L,
      to = 1L,
      mode = "auto_additive",
      description = "Stamp the database with codeminer provenance and schema version.",
      up = function(con) {
        # 1. Ensure every index metadata table exists. Idempotent — no-op
        #    for DBs that already have them. Covers the edge case where a
        #    bare DB file exists with no codeminer tables yet.
        create_lookup_metadata_table(con, overwrite = FALSE)
        create_mapping_metadata_table(con, overwrite = FALSE)
        create_relationship_metadata_table(con, overwrite = FALSE)

        # 2. Bring those tables up to the column set the package currently
        #    expects (additive ALTER ... ADD COLUMN). Historically this ran
        #    silently on every `build_database()`; we now do it once, as
        #    part of v0 -> v1, then never again.
        migrate_metadata_schema(con)

        # 3. Create `_db_metadata` and write the initial stamp row.
        create_db_metadata_table(con, overwrite = FALSE)
        DBI::dbWriteTable(
          con,
          name = codeminer_metadata_table_names$db,
          value = codeminer_initial_stamp_row(),
          append = TRUE
        )
      }
    )
  )
}

# Return the subset of migrations whose `from` falls in [start, end). If
# start == end this is empty (no work). Errors if the chain is broken.
pending_migrations <- function(start, end) {
  if (start == end) {
    return(list())
  }
  all_m <- codeminer_migrations()
  chain <- Filter(function(m) m$from >= start && m$to <= end, all_m)
  chain <- chain[order(vapply(chain, function(m) m$from, integer(1)))]

  if (length(chain) == 0L) {
    codeminer_abort(
      c(
        "No registered migrations cover the path from schema v{start} to v{end}.",
        "i" = "This usually means a migration is missing in the package source."
      )
    )
  }

  expected <- start
  for (m in chain) {
    if (m$from != expected) {
      codeminer_abort(
        c(
          "Migration chain is broken between schema v{expected} and v{m$from}.",
          "i" = "Missing migration {expected} -> {m$from} in the registry."
        )
      )
    }
    expected <- m$to
  }
  if (expected != end) {
    codeminer_abort(
      c(
        "Migration chain stops at v{expected}; expected to reach v{end}.",
        "i" = "Missing migration {expected} -> {end} in the registry."
      )
    )
  }
  chain
}

# Update the stamp after a migration has run. Bumps `schema_version` to the
# migration's target and records `last_migrated_at`.
record_migration <- function(con, to_version, now = Sys.time()) {
  tbl <- codeminer_metadata_table_names$db
  ts <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  DBI::dbExecute(
    con,
    glue::glue_sql(
      "UPDATE {`tbl`}
       SET schema_version = {as.character(to_version)},
           last_migrated_at = {ts}",
      .con = con
    )
  )
}

# Enforce the schema-version gate against a DB at `path`. Called from
# `codeminer_connect()` before the workbench attaches the file, and from the
# `get_db_con()` auto-initialise path. Behaviour mirrors the documented
# policy:
#
#   * file does not exist           -> caller will build it; nothing to gate
#   * stamped == current_schema     -> proceed silently
#   * stamped > current_schema      -> hard refuse (DB too new)
#   * stamped < min_readable_schema -> hard refuse (DB too old)
#   * stamped in [min_readable, current), only auto-additive migrations
#     between -> run them, return TRUE
#   * stamped in [min_readable, current), at least one manual/breaking
#     migration on the chain -> refuse with a pointer at `migrate_database()`
#
# Returns TRUE invisibly on success. Errors via `codeminer_abort()` on
# refusal.
enforce_schema_gate <- function(path) {
  if (!file.exists(path)) {
    return(invisible(TRUE))
  }

  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  version <- effective_schema_version(read_db_schema_version(con))
  DBI::dbDisconnect(con, shutdown = TRUE)

  current <- current_schema_version()
  min_v <- min_readable_schema_version()

  if (version == current) {
    return(invisible(TRUE))
  }

  if (version > current) {
    codeminer_abort(
      c(
        "Database at {.file {path}} is at schema v{version}; this codeminer supports up to v{current}.",
        "i" = "Upgrade codeminer to a release that supports schema >= v{version}."
      )
    )
  }

  if (version < min_v) {
    codeminer_abort(
      c(
        "Database at {.file {path}} is at schema v{version}; this codeminer requires >= v{min_v}.",
        "i" = "Use an older codeminer release to migrate the database forward first, then upgrade."
      )
    )
  }

  chain <- pending_migrations(version, current)
  modes <- vapply(chain, function(m) m$mode, character(1))

  if (all(modes == "auto_additive")) {
    codeminer_inform(c(
      "i" = "Auto-migrating database from schema v{version} to v{current} ({length(chain)} migration{?s})."
    ))
    withr::with_envvar(
      c(CODEMINER_DB_PATH = path),
      migrate_database()
    )
    return(invisible(TRUE))
  }

  non_auto <- chain[modes != "auto_additive"]
  non_auto_steps <- vapply(
    non_auto,
    function(m) sprintf("v%d->v%d (%s)", m$from, m$to, m$mode),
    character(1)
  )
  codeminer_abort(
    c(
      "Database at {.file {path}} is at schema v{version}; this codeminer expects v{current}.",
      "x" = "Pending migrations include non-auto steps: {non_auto_steps}",
      "i" = "Run {.code codeminer::migrate_database()} to apply them."
    )
  )
}

#' Migrate the codeminer database to the current schema version
#'
#' Walks the registered migration chain from the database's stamped schema
#' version up to `current_schema_version()`. If the database has no stamp
#' (built before the stamping migration landed) it is treated as schema
#' version 0 and migrated forward from there.
#'
#' @param dry_run If `TRUE`, prints the migrations that would run and returns
#'   without modifying the database.
#'
#' @return The new schema version, invisibly. `NULL` if nothing to do.
#' @export
#' @family Database management
migrate_database <- function(dry_run = FALSE) {
  con <- connect_to_db(read_only = FALSE)
  # No `check_database(con)` here: the v0 -> v1 migration is responsible for
  # creating the metadata tables if they are missing.

  from <- effective_schema_version(read_db_schema_version(con))
  to <- current_schema_version()

  if (from == to) {
    codeminer_inform(c(
      "v" = "Database is already at schema v{to}; nothing to migrate."
    ))
    return(invisible(NULL))
  }

  if (from > to) {
    codeminer_abort(
      c(
        "Database is at schema v{from}, newer than this codeminer (v{to}).",
        "i" = "Install a codeminer release that supports schema >= v{from}."
      )
    )
  }

  if (from < min_readable_schema_version()) {
    codeminer_abort(
      c(
        "Database is at schema v{from}; this codeminer requires >= v{min_readable_schema_version()}.",
        "i" = "Use an older codeminer to migrate the database forward first, then upgrade."
      )
    )
  }

  chain <- pending_migrations(from, to)

  if (dry_run) {
    codeminer_inform(c(
      "i" = "Would run {length(chain)} migration{?s} (dry run, no changes made):"
    ))
    for (m in chain) {
      codeminer_inform(c(
        " " = "v{m$from} -> v{m$to} ({m$mode}): {m$description}"
      ))
    }
    return(invisible(to))
  }

  for (m in chain) {
    codeminer_inform(c(
      "i" = "Migrating v{m$from} -> v{m$to} ({m$mode}): {m$description}"
    ))
    m$up(con)
    record_migration(con, to_version = m$to)
  }

  codeminer_inform(c("v" = "Database migrated to schema v{to}."))
  invisible(to)
}
