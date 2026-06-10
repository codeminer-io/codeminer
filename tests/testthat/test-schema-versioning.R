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

  # Pretend the package is only at schema v1 — so the gate has a pure-auto
  # chain (just v0 -> v1) to run. With the real registry, the v1 -> v2 step
  # is `breaking` and would (correctly) refuse here.
  testthat::local_mocked_bindings(
    current_schema_version = function() 1L
  )

  # Connect should succeed and the gate should have stamped the DB.
  suppressMessages(codeminer_connect())

  with_write_conn(function(con) {
    expect_true(
      codeminer_metadata_table_names$db %in% DBI::dbListTables(con)
    )
  })
})

test_that("codeminer_connect() refuses when the chain has a non-auto migration", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbRemoveTable(con, codeminer_metadata_table_names$db)
  })
  codeminer_disconnect()

  # Pretend the registry has a `breaking` v0 -> v1 migration. The gate
  # should refuse rather than auto-running it.
  testthat::local_mocked_bindings(
    current_schema_version = function() 1L,
    codeminer_migrations = function() {
      list(
        list(
          from = 0L,
          to = 1L,
          mode = "breaking",
          description = "fake breaking migration for tests",
          up = function(con) stop("must not run")
        )
      )
    }
  )

  expect_error(
    suppressMessages(codeminer_connect()),
    "non-auto"
  )
  codeminer_disconnect()
})

# ---- v1 -> v2: canonical code_type rename -------------------------------

test_that("v1 -> v2 migration renames metadata + underlying tables to canonical code_type", {
  local_build_temp_database()

  # Seed the DB with rows + tables using the OLD (pre-canonical) strings
  # via the public add_*_table() helpers. The metadata constructors derive
  # the table name from the code_type — e.g. lookup_metadata("OPCS4",
  # lookup_version = "test_v1") produces lookup_table_name "OPCS4_test_v1".
  suppressMessages(add_lookup_table(
    tibble::tibble(code = "A011", description = "Excision of gallbladder"),
    lookup_metadata(
      code_type = "OPCS4",
      lookup_version = "test_v1",
      lookup_source = "fake"
    )
  ))
  suppressMessages(add_mapping_table(
    tibble::tibble(from = "0204", to = "12345001"),
    mapping_metadata(
      from_code_type = "bnf",
      to_code_type = "dmd",
      map_version = "v0",
      map_source = "fake"
    )
  ))
  suppressMessages(add_relationship_table(
    tibble::tibble(from = "A011", to = "A01", type = "is a"),
    relationship_metadata(
      code_type = "OPCS4",
      relationship_version = "test_v1",
      relationship_source = "fake"
    )
  ))

  # Push the stamp back to v1 so the gate has work to do.
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '1'")
  })
  codeminer_disconnect()

  # Run the migration.
  suppressMessages(migrate_database())

  # Verify metadata + underlying tables now use canonical names.
  with_write_conn(function(con) {
    tables <- DBI::dbListTables(con)
    expect_true("OPCS-4_test_v1" %in% tables)
    expect_false("OPCS4_test_v1" %in% tables)
    expect_true("BNF_DM+D_v0" %in% tables)
    expect_false("bnf_dmd_v0" %in% tables)
    expect_true("OPCS-4_relationship_test_v1" %in% tables)
    expect_false("OPCS4_relationship_test_v1" %in% tables)

    lookup <- dplyr::tbl(con, codeminer_metadata_table_names$lookup) |>
      dplyr::collect()
    expect_true("OPCS-4" %in% lookup$code_type)
    expect_false("OPCS4" %in% lookup$code_type)

    mapping <- dplyr::tbl(con, codeminer_metadata_table_names$mapping) |>
      dplyr::collect()
    expect_true("BNF" %in% mapping$from_code_type)
    expect_true("DM+D" %in% mapping$to_code_type)

    rel <- dplyr::tbl(con, codeminer_metadata_table_names$relationship) |>
      dplyr::collect()
    expect_true("OPCS-4" %in% rel$code_type)
  })
})

# ---- migrate_database(): refusal paths -----------------------------------

test_that("migrate_database() refuses a DB stamped at a newer schema than the package", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '99'")
  })

  expect_error(
    suppressMessages(migrate_database()),
    "newer than this codeminer"
  )
})

test_that("migrate_database() refuses a DB stamped below min_readable_schema_version()", {
  local_build_temp_database()
  with_write_conn(function(con) {
    DBI::dbExecute(con, "UPDATE _db_metadata SET schema_version = '-1'")
  })

  expect_error(
    suppressMessages(migrate_database()),
    "this codeminer requires"
  )
})

# ---- pending_migrations(): chain validation -------------------------------

test_that("pending_migrations() errors when no registered migrations cover the path", {
  testthat::local_mocked_bindings(
    codeminer_migrations = function() list()
  )
  expect_error(
    pending_migrations(0L, 1L),
    "No registered migrations"
  )
})

test_that("pending_migrations() errors when the registered chain has a gap", {
  testthat::local_mocked_bindings(
    codeminer_migrations = function() {
      list(
        list(
          from = 0L,
          to = 1L,
          mode = "auto_additive",
          description = "v0->v1",
          up = function(con) NULL
        ),
        # gap: missing v1 -> v2
        list(
          from = 2L,
          to = 3L,
          mode = "auto_additive",
          description = "v2->v3",
          up = function(con) NULL
        )
      )
    }
  )
  expect_error(
    pending_migrations(0L, 3L),
    "chain is broken"
  )
})

test_that("pending_migrations() errors when the chain stops short of the target", {
  testthat::local_mocked_bindings(
    codeminer_migrations = function() {
      list(
        list(
          from = 0L,
          to = 1L,
          mode = "auto_additive",
          description = "v0->v1",
          up = function(con) NULL
        )
      )
    }
  )
  expect_error(
    pending_migrations(0L, 5L),
    "chain stops at"
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
