# Database schema versioning.
#
# The on-disk codeminer database carries its own schema version, independent
# of the package version. The package declares `current_schema_version()` —
# what fresh `build_database()` produces — and `min_readable_schema_version()`
# — how far back this codeminer can still operate. Older DBs are migrated
# forward (auto-additive migrations) or hard-refused with a pointer at
# `migrate_database()` (non-additive). Newer-than-package DBs are refused.

# Current schema version. BUMP this when registering a new migration. See
# CLAUDE.md and `vignettes/developer-guide.Rmd` for what warrants a bump.
current_schema_version <- function() {
  1L
}

# Oldest schema version this codeminer can still operate on. Bump only when
# carrying support for an ancient schema becomes a maintenance burden. DBs
# stamped older than this are hard-refused at connect time.
#
# `0L` is the implicit "unstamped" version — i.e. databases built before
# `_db_metadata` existed in the codeminer source.
min_readable_schema_version <- function() {
  0L
}

required_db_metadata_columns <- function() {
  c(
    # Initial stamp + most-recent provenance
    "codeminer_version",
    "schema_version",
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
# last_migrated_at is NA until a migration runs.
codeminer_initial_stamp_row <- function(now = Sys.time()) {
  info <- codeminer_build_info()
  data.frame(
    codeminer_version = info$codeminer_version,
    schema_version = as.character(current_schema_version()),
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

# `read_db_schema_version()` returns NA for unstamped DBs. The gate treats
# that as version 0 (pre-#128). This helper centralises that translation.
effective_schema_version <- function(read_version) {
  if (is.na(read_version)) 0L else read_version
}
