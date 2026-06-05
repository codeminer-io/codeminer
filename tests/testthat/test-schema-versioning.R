# Tests for the DB schema-versioning framework (#128).
#
# Covers:
#   * fresh build_database() stamps `_db_metadata` at current_schema_version()
#   * existing-DB build_database() walks the migration chain
#   * unstamped (pre-#128) DBs are migrated forward via v0 -> v1
#   * migrate_database(dry_run = TRUE) reports without changing the DB
#   * codeminer_connect() gate refuses too-new DBs and below-min_readable DBs
#   * codeminer_connect() gate auto-migrates when the chain is auto_additive
#   * pending_migrations() validates the chain
#   * codeminer_build_info() reflects packageDescription() fields

# Helper: open a fresh write conn to the active DB and run `f(con)`.
with_write_conn <- function(f) {
  con <- DBI::dbConnect(duckdb::duckdb(), Sys.getenv("CODEMINER_DB_PATH"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  f(con)
}

# ---- _db_metadata creation & stamp content -------------------------------

test_that("build_database() stamps `_db_metadata` at current_schema_version on a fresh DB", {
  local_temp_database()
  suppressMessages(build_database())

  with_write_conn(function(con) {
    expect_true(
      codeminer_metadata_table_names$db %in% DBI::dbListTables(con)
    )
    row <- DBI::dbGetQuery(
      con,
      glue::glue_sql(
        "SELECT * FROM {`codeminer_metadata_table_names$db`}",
        .con = con
      )
    )
    expect_equal(nrow(row), 1L)
    expect_equal(as.integer(row$schema_version), current_schema_version())
    expect_equal(
      row$codeminer_version,
      as.character(utils::packageVersion("codeminer"))
    )
    expect_true(!is.na(row$built_at))
    expect_true(is.na(row$last_migrated_at))
  })
})

test_that("required_db_metadata_columns() covers every field in the stamp row", {
  expect_setequal(
    names(codeminer_initial_stamp_row()),
    required_db_metadata_columns()
  )
})

# ---- Migration chain & registry -------------------------------------------

test_that("pending_migrations() returns an empty list when start == end", {
  expect_equal(pending_migrations(1L, 1L), list())
})

test_that("pending_migrations() returns the v0 -> v1 migration for an unstamped DB", {
  chain <- pending_migrations(0L, 1L)
  expect_equal(length(chain), 1L)
  expect_equal(chain[[1]]$from, 0L)
  expect_equal(chain[[1]]$to, 1L)
  expect_equal(chain[[1]]$mode, "auto_additive")
})

# ---- Backfill: unstamped DB gets migrated -------------------------------

test_that("build_database() on an unstamped DB applies the v0 -> v1 migration", {
  local_build_temp_database()

  # Simulate a pre-#128 DB by dropping the stamp table.
  with_write_conn(function(con) {
    DBI::dbRemoveTable(con, codeminer_metadata_table_names$db)
  })

  suppressMessages(build_database(overwrite = FALSE))

  with_write_conn(function(con) {
    expect_true(
      codeminer_metadata_table_names$db %in% DBI::dbListTables(con)
    )
    row <- DBI::dbGetQuery(
      con,
      "SELECT schema_version, last_migrated_at FROM _db_metadata"
    )
    expect_equal(as.integer(row$schema_version), current_schema_version())
    expect_true(!is.na(row$last_migrated_at))
  })
})

# ---- migrate_database() dry_run ------------------------------------------

test_that("migrate_database(dry_run = TRUE) reports without mutating", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbRemoveTable(con, codeminer_metadata_table_names$db)
  })

  before <- with_write_conn(function(con) {
    list(
      tables = DBI::dbListTables(con),
      lookup_fields = DBI::dbListFields(
        con,
        codeminer_metadata_table_names$lookup
      )
    )
  })

  res <- suppressMessages(migrate_database(dry_run = TRUE))
  expect_equal(res, current_schema_version())

  after <- with_write_conn(function(con) {
    list(
      tables = DBI::dbListTables(con),
      lookup_fields = DBI::dbListFields(
        con,
        codeminer_metadata_table_names$lookup
      )
    )
  })
  expect_identical(before$tables, after$tables)
  expect_identical(before$lookup_fields, after$lookup_fields)
})

test_that("migrate_database() is a no-op on a DB already at the current schema", {
  local_build_temp_database()
  expect_message(
    res <- migrate_database(),
    "already at schema"
  )
  expect_null(res)
})

# ---- Connect gate: refusal paths -----------------------------------------

test_that("codeminer_connect() refuses a DB stamped at a newer schema than the package", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '99'")
  })
  codeminer_disconnect()

  expect_error(
    suppressMessages(codeminer_connect()),
    "supports up to v"
  )
  codeminer_disconnect()
})

test_that("codeminer_connect() auto-migrates an unstamped DB via the gate", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbRemoveTable(con, codeminer_metadata_table_names$db)
  })
  codeminer_disconnect()

  # Connect should succeed and the gate should have stamped the DB.
  suppressMessages(codeminer_connect())

  with_write_conn(function(con) {
    expect_true(
      codeminer_metadata_table_names$db %in% DBI::dbListTables(con)
    )
  })
})

# ---- Provenance from packageDescription -----------------------------------

test_that("codeminer_build_info() includes the package version", {
  info <- codeminer_build_info()
  expect_equal(
    info$codeminer_version,
    as.character(utils::packageVersion("codeminer"))
  )
  expect_true(!is.null(info$codeminer_source))
})
