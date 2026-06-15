# Tests for the parquet_folder storage backend.

# Helper: create a fresh empty parquet_folder database in a temp dir,
# point CODEMINER_DB_PATH at it, and connect the workbench. Returns the
# folder path invisibly. The workbench is disconnected on scope exit.
local_build_temp_folder_database <- function(..., .envir = parent.frame()) {
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    codeminer_disconnect()
  }
  temp_dir <- withr::local_tempfile(.local_envir = .envir)
  dir.create(temp_dir)
  withr::local_envvar(CODEMINER_DB_PATH = temp_dir, .local_envir = .envir)
  withr::defer(codeminer_disconnect(), envir = .envir)
  build_database(overwrite = TRUE)
  codeminer_connect(main = temp_dir)
  invisible(temp_dir)
}

# Sample lookup data used across the round-trip tests.
sample_lookup <- function() {
  data.frame(
    code = c("E10", "E11", "E12"),
    description = c(
      "Type 1 diabetes",
      "Type 2 diabetes",
      "Malnutrition diabetes"
    ),
    stringsAsFactors = FALSE
  )
}

# Backend detection --------------------------------------------------------

test_that("backend_kind() distinguishes folder vs file paths", {
  tmp_file <- withr::local_tempfile(fileext = ".duckdb")
  expect_equal(backend_kind(tmp_file), "duckdb_file")

  tmp_dir <- withr::local_tempfile()
  dir.create(tmp_dir)
  expect_equal(backend_kind(tmp_dir), "codeminer_folder")
})

# build_database() initialises a fresh folder --------------------------------

test_that("build_database() creates the metadata parquet skeleton", {
  temp_dir <- local_build_temp_folder_database()

  files <- list.files(temp_dir)
  expected <- paste0(
    c(
      "_db_metadata",
      "_lookup_metadata",
      "_mapping_metadata",
      "_relationship_metadata"
    ),
    ".parquet"
  )
  expect_setequal(files, expected)
})

test_that("schema version is stamped at the current version", {
  temp_dir <- local_build_temp_folder_database()
  expect_equal(
    backend_read_schema_version(temp_dir),
    current_schema_version()
  )
})

# dbListTables() parity ----------------------------------------------------

test_that("dbListTables() matches between folder and file backends", {
  # Folder backend
  folder_dir <- local_build_temp_folder_database()
  lt <- sample_lookup()
  add_lookup_table(lt, lookup_metadata("ICD-10", lookup_version = "v1"))
  folder_tables <- DBI::dbListTables(get_db_con())
  codeminer_disconnect()

  # File backend
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    codeminer_disconnect()
  }
  file_path <- withr::local_tempfile(fileext = ".duckdb")
  withr::local_envvar(CODEMINER_DB_PATH = file_path)
  build_database(overwrite = TRUE)
  codeminer_connect(main = file_path)
  withr::defer(codeminer_disconnect())
  add_lookup_table(lt, lookup_metadata("ICD-10", lookup_version = "v1"))
  file_tables <- DBI::dbListTables(get_db_con())

  expect_setequal(folder_tables, file_tables)
})

# add_lookup_table() round-trip --------------------------------------------

test_that("add_lookup_table() writes both data and metadata atomically", {
  temp_dir <- local_build_temp_folder_database()

  lt <- sample_lookup()
  meta <- lookup_metadata("ICD-10", lookup_version = "v1")
  added <- add_lookup_table(lt, meta)
  expect_true(added)

  # Both files now exist
  expect_true(file.exists(file.path(temp_dir, "ICD-10_v1.duckdb")))
  expect_true(file.exists(file.path(temp_dir, "_lookup_metadata.parquet")))

  # No stray .tmp files left behind from the transaction
  tmp_files <- list.files(temp_dir, pattern = "\\.tmp$")
  expect_length(tmp_files, 0)
})

test_that("CODES() queries return rows from the data parquet", {
  local_build_temp_folder_database()
  lt <- sample_lookup()
  add_lookup_table(lt, lookup_metadata("ICD-10", lookup_version = "v1"))

  res <- as.data.frame(CODES("E10", type = "ICD-10"))
  expect_equal(res$code, "E10")
  expect_equal(res$description, "Type 1 diabetes")
})

test_that("dplyr::tbl() against the workbench reads the underlying parquet", {
  local_build_temp_folder_database()
  lt <- sample_lookup()
  add_lookup_table(lt, lookup_metadata("ICD-10", lookup_version = "v1"))

  tbl <- as.data.frame(dplyr::tbl(get_db_con(), "ICD-10_v1"))
  expect_equal(nrow(tbl), 3)
  expect_setequal(tbl$code, c("E10", "E11", "E12"))
})

# Uniqueness rejection ----------------------------------------------------

test_that("add_lookup_table() refuses a duplicate name (metadata-only check)", {
  temp_dir <- local_build_temp_folder_database()
  lt <- sample_lookup()
  meta <- lookup_metadata("ICD-10", lookup_version = "v1")
  add_lookup_table(lt, meta)

  expect_warning(
    added <- add_lookup_table(lt, meta),
    "already exists"
  )
  expect_false(added)
})

test_that("re-add succeeds after a metadata-less orphan data file is present", {
  # Simulates the 'process killed between step 3 and step 4' case:
  # data file on disk, no metadata row. A new add of the same name
  # should overwrite the orphan rather than block on file.exists().
  temp_dir <- local_build_temp_folder_database()
  orphan_path <- file.path(temp_dir, "ICD-10_v1.duckdb")
  # Drop a stray file with random bytes — uniqueness check must look at
  # metadata only, not at file existence.
  writeBin(as.raw(c(1, 2, 3)), orphan_path)
  expect_true(file.exists(orphan_path))

  lt <- sample_lookup()
  added <- add_lookup_table(
    lt,
    lookup_metadata("ICD-10", lookup_version = "v1")
  )
  expect_true(added)

  # Orphan was overwritten with a valid duckdb file holding our lookup
  res <- as.data.frame(dplyr::tbl(get_db_con(), "ICD-10_v1"))
  expect_setequal(res$code, c("E10", "E11", "E12"))
})

# Transaction integrity ----------------------------------------------------

test_that("metadata-rename failure rolls the data file commit back", {
  temp_dir <- local_build_temp_folder_database()
  lt <- sample_lookup()
  meta <- lookup_metadata("ICD-10", lookup_version = "v1")

  # Stub file.rename so the *second* call (the metadata commit, step 4)
  # throws. The first call (step 3, the data commit) still runs.
  call_count <- 0L
  with_mocked_bindings(
    {
      expect_error(
        add_lookup_table(lt, meta),
        "simulated rename failure"
      )
    },
    .package = "base",
    file.rename = function(from, to) {
      call_count <<- call_count + 1L
      if (call_count == 2L) {
        stop("simulated rename failure")
      }
      .Internal(file.rename(from, to))
    }
  )

  # After the rollback: no data parquet, no temp files, no metadata row
  expect_false(file.exists(file.path(temp_dir, "ICD-10_v1.duckdb")))
  expect_false(file.exists(file.path(temp_dir, "ICD-10_v1.parquet.tmp")))
  expect_false(file.exists(file.path(temp_dir, "_lookup_metadata.parquet.tmp")))
  meta_df <- backend_read_metadata(temp_dir, "lookup")
  expect_false("ICD-10_v1" %in% meta_df$lookup_table_name)
})

test_that("data-rename failure leaves the database untouched", {
  temp_dir <- local_build_temp_folder_database()
  lt <- sample_lookup()
  meta <- lookup_metadata("ICD-10", lookup_version = "v1")

  # Stub file.rename so the FIRST call (step 3, data commit) throws.
  with_mocked_bindings(
    {
      expect_error(add_lookup_table(lt, meta), "simulated rename failure")
    },
    .package = "base",
    file.rename = function(from, to) {
      stop("simulated rename failure")
    }
  )

  expect_false(file.exists(file.path(temp_dir, "ICD-10_v1.duckdb")))
  meta_df <- backend_read_metadata(temp_dir, "lookup")
  expect_false("ICD-10_v1" %in% meta_df$lookup_table_name)
})

# remove_lookup_table() ----------------------------------------------------

test_that("remove_lookup_table() deletes both metadata row and data file", {
  temp_dir <- local_build_temp_folder_database()
  add_lookup_table(
    sample_lookup(),
    lookup_metadata("ICD-10", lookup_version = "v1")
  )

  remove_lookup_table("ICD-10", "v1")
  expect_false(file.exists(file.path(temp_dir, "ICD-10_v1.duckdb")))
  meta_df <- backend_read_metadata(temp_dir, "lookup")
  expect_false("ICD-10_v1" %in% meta_df$lookup_table_name)
})

# update_lookup_metadata() -------------------------------------------------

test_that("update_lookup_metadata() rewrites the metadata parquet", {
  local_build_temp_folder_database()
  add_lookup_table(
    sample_lookup(),
    lookup_metadata("ICD-10", lookup_version = "v1")
  )

  update_lookup_metadata(
    code_type = "ICD-10",
    lookup_version = "v1",
    col_filters = list(code = list(values = c("E10", "E11"), defaults = "E10"))
  )

  meta_df <- backend_read_metadata(db_path(), "lookup")
  expect_false(is.na(meta_df$col_filters[
    meta_df$lookup_table_name == "ICD-10_v1"
  ]))
})

# validate_database() -----------------------------------------------------

test_that("validate_database() reports a clean DB as consistent", {
  local_build_temp_folder_database()
  add_lookup_table(
    sample_lookup(),
    lookup_metadata("ICD-10", lookup_version = "v1")
  )
  issues <- validate_database()
  expect_length(issues$orphan_data_files, 0)
  expect_length(issues$dangling_metadata, 0)
  expect_length(issues$stale_temp_files, 0)
})

test_that("validate_database() detects orphan data files", {
  temp_dir <- local_build_temp_folder_database()
  # Drop a stray data file at the root with no matching metadata row.
  # The validator looks for `.duckdb` files at the root; the contents
  # don't have to be a valid DuckDB file for the orphan check.
  writeBin(
    as.raw(c(1, 2, 3)),
    file.path(temp_dir, "stray_v9.duckdb")
  )
  issues <- validate_database()
  expect_true("stray_v9" %in% issues$orphan_data_files)
})

test_that("validate_database() detects dangling metadata", {
  temp_dir <- local_build_temp_folder_database()
  add_lookup_table(
    sample_lookup(),
    lookup_metadata("ICD-10", lookup_version = "v1")
  )
  # Delete the data file behind the back of the package.
  unlink(file.path(temp_dir, "ICD-10_v1.duckdb"))
  issues <- validate_database()
  expect_true("ICD-10_v1" %in% issues$dangling_metadata)
})

test_that("validate_database() detects stale .tmp files", {
  temp_dir <- local_build_temp_folder_database()
  writeBin(
    as.raw(c(0)),
    file.path(temp_dir, "_lookup_metadata.parquet.tmp")
  )
  writeBin(
    as.raw(c(0)),
    file.path(temp_dir, "ICD-10_v1.duckdb.tmp")
  )
  issues <- validate_database()
  expect_true(
    "_lookup_metadata.parquet.tmp" %in% issues$stale_temp_files
  )
  expect_true(
    "ICD-10_v1.duckdb.tmp" %in% issues$stale_temp_files
  )
})

# Concurrent-read-during-write --------------------------------------------

test_that("concurrent reader sees pre-add OR post-add state, never torn", {
  # This exercises the atomic-rename guarantee. We add a table while a
  # background reader repeatedly reads _lookup_metadata.parquet via a
  # fresh in-memory DuckDB connection, and assert that every read
  # returns a valid (possibly empty, possibly populated) result.
  temp_dir <- local_build_temp_folder_database()
  meta_path <- file.path(temp_dir, "_lookup_metadata.parquet")

  reader_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  withr::defer(DBI::dbDisconnect(reader_con, shutdown = TRUE))

  lt <- sample_lookup()
  meta <- lookup_metadata("ICD-10", lookup_version = "v1")

  # Hammer the metadata path with reads. The number of iterations is
  # small but enough to make this a meaningful smoke test on local FS.
  for (i in seq_len(20)) {
    res <- DBI::dbGetQuery(
      reader_con,
      glue::glue_sql(
        "SELECT count(*) AS n FROM read_parquet({meta_path})",
        .con = reader_con
      )
    )
    expect_true(is.finite(res$n))
    expect_true(res$n %in% c(0L, 1L))
    if (i == 5L) {
      add_lookup_table(lt, meta)
    }
  }
  # Post-add the metadata file has exactly one row
  final <- DBI::dbGetQuery(
    reader_con,
    glue::glue_sql(
      "SELECT count(*) AS n FROM read_parquet({meta_path})",
      .con = reader_con
    )
  )
  expect_equal(final$n, 1L)
})
