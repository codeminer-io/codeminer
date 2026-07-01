# Package-level internal environment for session state.
# Stores the workbench connection, cached metadata, and other session data.
.codeminer_env <- new.env(parent = emptyenv())

#' Get the workbench database connection
#'
#' Returns a DBI connection to the workbench. If `con` is provided,
#' returns it directly (manual override). Otherwise returns the cached
#' workbench connection, auto-initializing via [codeminer_connect()] if needed.
#'
#' @param con Optional DBI connection to use instead of the workbench.
#' @return A DBI connection object.
#' @noRd
get_db_con <- function(con = NULL) {
  if (!is.null(con)) {
    return(con)
  }

  needs_connect <-
    !exists("con", envir = .codeminer_env) ||
    !DBI::dbIsValid(.codeminer_env$con)

  # Also reconnect if CODEMINER_DB_PATH changed since last connect
  # (but not while core is temporarily DETACHed during a write)
  if (!needs_connect && !is.null(.codeminer_env$db_path)) {
    needs_connect <- !identical(
      .codeminer_env$db_path,
      db_path()
    )
  }

  if (needs_connect) {
    codeminer_connect()
  }

  .codeminer_env$con
}

.onLoad <- function(libname, pkgname) {
  invisible()
}

.onUnload <- function(libpath) {
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }
}
