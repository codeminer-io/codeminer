# Database schema versioning.
#
# The on-disk codeminer database carries its own schema version, independent
# of the package version. The package declares `current_schema_version()` —
# what `build_database()` produces — and refuses to attach any DB stamped at
# a different version via `enforce_schema_gate()`. There is intentionally no
# in-place migration path: pre-1.0 we expect users to rebuild when the
# format changes. See `vignettes/developer-guide.Rmd` for the gate policy.

# Current schema version. BUMP this when the on-disk format changes. The
# gate will then refuse pre-bump DBs with a "rebuild required" message.
# See CLAUDE.md for what counts as a format change.
current_schema_version <- function() {
  5L
}

required_db_metadata_columns <- function() {
  c(
    # Initial stamp + most-recent provenance
    "codeminer_version",
    "schema_version",
    # On-disk layout. One of "duckdb_file", "codeminer_folder",
    # "parquet_folder". Recorded once at `build_database()` time and used
    # by `backend_kind()` to distinguish folder layouts that look
    # identical at the metadata level.
    "storage_format",
    "built_at",
    "last_migrated_at",
    # renv.lock-style install provenance: filled in from packageDescription()
    # so DBs built from a GitHub install record the repo + SHA. Local/CRAN
    # installs leave the remote fields NA.
    "codeminer_source",
    "codeminer_remote_type",
    "codeminer_remote_host",
    "codeminer_remote_repo",
    "codeminer_remote_username",
    "codeminer_remote_sha"
  )
}

# Pull install provenance from the installed codeminer DESCRIPTION. Mirrors
# the field names renv writes to renv.lock so anyone familiar with renv
# recognises them. Fields default to NA when the install path didn't record
# them (e.g. CRAN, local source, `devtools::load_all()`).
codeminer_build_info <- function() {
  desc <- utils::packageDescription("codeminer")
  if (is.null(desc) || identical(desc, NA)) {
    desc <- list()
  }
  list(
    codeminer_version = as.character(utils::packageVersion("codeminer")),
    codeminer_source = desc$Source %||% desc$Repository %||% "Local",
    codeminer_remote_type = desc$RemoteType %||% NA_character_,
    codeminer_remote_host = desc$RemoteHost %||% NA_character_,
    codeminer_remote_repo = desc$RemoteRepo %||% NA_character_,
    codeminer_remote_username = desc$RemoteUsername %||% NA_character_,
    codeminer_remote_sha = desc$RemoteSha %||%
      desc$GithubSHA1 %||%
      NA_character_
  )
}

# The single row that `build_database()` writes into `_db_metadata` on a
# fresh DB. Schema-version is the current package value; built_at is now;
# last_migrated_at stays NA (kept on the row for forward compatibility in
# case we re-introduce migrations). `storage_format` records the on-disk
# layout chosen at build time and is consulted by `backend_kind()` to
# disambiguate folder DBs.
codeminer_initial_stamp_row <- function(
  storage_format = "duckdb_file",
  now = Sys.time()
) {
  info <- codeminer_build_info()
  data.frame(
    codeminer_version = info$codeminer_version,
    schema_version = as.character(current_schema_version()),
    storage_format = storage_format,
    built_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    last_migrated_at = NA_character_,
    codeminer_source = info$codeminer_source,
    codeminer_remote_type = info$codeminer_remote_type,
    codeminer_remote_host = info$codeminer_remote_host,
    codeminer_remote_repo = info$codeminer_remote_repo,
    codeminer_remote_username = info$codeminer_remote_username,
    codeminer_remote_sha = info$codeminer_remote_sha,
    stringsAsFactors = FALSE
  )
}

# Read the schema version stamped on a DB. Returns `NA_integer_` for an
# unstamped DB (no `_db_metadata` table — i.e. older than #128).
read_db_schema_version <- function(con) {
  tbl <- codeminer_metadata_table_names$db
  if (!table_exists(con, tbl)) {
    return(NA_integer_)
  }
  row <- DBI::dbGetQuery(
    con,
    glue::glue_sql("SELECT schema_version FROM {`tbl`}", .con = con)
  )
  if (nrow(row) == 0L) {
    return(NA_integer_)
  }
  as.integer(row$schema_version[[1L]])
}

# Treat unstamped DBs (pre-#128) as version 0 for the gate so they get the
# same "rebuild required" refusal as any other older version.
effective_schema_version <- function(read_version) {
  if (is.na(read_version)) 0L else read_version
}

# Refuse to attach a database whose stamped schema version differs from what
# the current package expects. Called from `codeminer_connect()` before the
# read-only ATTACH so the user gets a friendly error rather than a raw
# DuckDB catalog miss.
#
# Behaviour:
#   * file does not exist        -> caller will build it; nothing to gate
#   * stamped == current_schema  -> proceed silently
#   * stamped > current_schema   -> refuse: "upgrade codeminer"
#   * stamped < current_schema   -> refuse: "rebuild the database"
#     (covers unstamped DBs too: effective_schema_version() maps them to 0L)
#
# Returns TRUE invisibly on success. Errors via `codeminer_abort()` on
# refusal.
enforce_schema_gate <- function(path) {
  if (!backend_database_exists(path)) {
    return(invisible(TRUE))
  }

  version <- effective_schema_version(backend_read_schema_version(path))

  current <- current_schema_version()
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

  codeminer_abort(
    c(
      "Database at {.file {path}} is at schema v{version}; this codeminer expects v{current}.",
      "i" = "Rebuild the database with {.code build_database(overwrite = TRUE)} (or install an older codeminer)."
    )
  )
}
