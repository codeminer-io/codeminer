# Create a temporary db file and point the workbench at it.
# Tears down any existing workbench so that the next query auto-initializes
# against the temp file. The workbench is disconnected on scope exit.
local_temp_database <- function(..., .envir = parent.frame()) {
  # Disconnect any existing workbench first
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    codeminer_disconnect()
  }

  temp_db <- withr::local_tempfile(fileext = ".duckdb", .local_envir = .envir)
  withr::local_envvar(CODEMINER_DB_PATH = temp_db, .local_envir = .envir)

  # Ensure workbench is torn down when the calling scope exits
  withr::defer(codeminer_disconnect(), envir = .envir)

  return(invisible(temp_db))
}

# Build a temporary test database and connect the workbench to it.
local_build_temp_database <- function(..., .envir = parent.frame()) {
  temp_db <- local_temp_database(.envir = .envir)
  build_database(overwrite = TRUE)
  codeminer_connect(main = temp_db)
  invisible(temp_db)
}

# Build a temporary database with dummy data (ICD-10, Read 3, mappings, etc.)
# and connect the workbench. Use this when tests need real code data.
local_build_temp_dummy_database <- function(..., .envir = parent.frame()) {
  temp_db <- local_temp_database(.envir = .envir)
  build_database(overwrite = TRUE)
  add_ukb_resource_592(path = dummy_ukb_resource_592_path())
  codeminer_connect(main = temp_db)
  invisible(temp_db)
}
