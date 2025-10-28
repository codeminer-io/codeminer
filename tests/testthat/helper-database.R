local_temp_database <- function(..., .envir = parent.frame()) {
  temp_db <- withr::local_tempfile(fileext = ".duckdb", .local_envir = .envir)
  withr::local_envvar(CODEMINER_DB_PATH = temp_db, .local_envir = .envir)
  return(invisible(temp_db))
}
