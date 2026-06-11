# Tests for the DB schema-versioning stamp + gate.
#
# Covers:
#   * fresh build_database() stamps `_db_metadata` at current_schema_version()
#   * required_db_metadata_columns() matches the stamp row shape
#   * codeminer_connect() gate refuses too-new and older DBs
#   * codeminer_connect() tears down the workbench when the gate refuses
#   * codeminer_build_info() reflects packageDescription() fields
#
# Pre-1.0 there is intentionally no in-place migration path — the gate
# refuses any DB whose stamp doesn't match the current package's schema
# version, and the user rebuilds via `build_database(overwrite = TRUE)`.
# See #139 for the discussion that landed this policy.

# Helper: open a write connection to the active DB and run `f(con)`. Uses
# `connect_to_db(read_only = FALSE)` because that path detaches the
# workbench's ATTACH first and re-attaches on exit — necessary on Windows,
# where DuckDB refuses to open a file already held by another connection.
with_write_conn <- function(f) {
  con <- connect_to_db(read_only = FALSE)
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

test_that("build_database() on an existing DB without overwrite is a no-op", {
  local_build_temp_database()
  # Second call should not throw and should not have re-stamped the row.
  before <- with_write_conn(function(con) {
    DBI::dbGetQuery(con, "SELECT built_at FROM _db_metadata")$built_at
  })
  expect_message(suppressMessages(build_database()), NA) # no error
  after <- with_write_conn(function(con) {
    DBI::dbGetQuery(con, "SELECT built_at FROM _db_metadata")$built_at
  })
  expect_equal(after, before)
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

test_that("codeminer_connect() refuses a DB stamped at an older schema than the package", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '0'")
  })
  codeminer_disconnect()

  expect_error(
    suppressMessages(codeminer_connect()),
    "Rebuild the database"
  )
  codeminer_disconnect()
})

test_that("codeminer_connect() refuses an unstamped DB (no _db_metadata table)", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbRemoveTable(con, codeminer_metadata_table_names$db)
  })
  codeminer_disconnect()

  # `read_db_schema_version()` returns NA for an unstamped DB; the gate
  # treats that as v0 and refuses with the same "rebuild" message.
  expect_error(
    suppressMessages(codeminer_connect()),
    "Rebuild the database"
  )
  codeminer_disconnect()
})

test_that("codeminer_connect() tears down the workbench when the schema gate refuses", {
  # Regression for #140 — after a gate refusal the in-memory `:memory:`
  # workbench was left valid but with no file attached, so subsequent
  # `get_db_con()` calls returned the cached con and queries hit raw
  # DuckDB catalog errors instead of the friendly gate message.
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '99'")
  })
  codeminer_disconnect()

  expect_error(
    suppressMessages(codeminer_connect()),
    "supports up to v"
  )

  # After the refusal the workbench should be gone.
  expect_false(exists("con", envir = .codeminer_env))

  # A subsequent get_db_con() should re-trigger the gate, raising the
  # same friendly error rather than a raw DuckDB catalog error.
  expect_error(
    suppressMessages(get_db_con()),
    "supports up to v"
  )
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
