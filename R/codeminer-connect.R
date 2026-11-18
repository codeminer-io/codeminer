# Schema the read-only ontology database is attached under. "main" is reserved
# in DuckDB (the default schema for any connection), so we use "core".
CODEMINER_SCHEMA <- "core"

#' Connect to the codeminer workbench
#'
#' Creates an in-memory DuckDB connection and ATTACHes the codeminer database
#' file as read-only. Writes to the database go through a short-lived direct
#' connection that detaches/reattaches around the operation.
#'
#' If called with no arguments, uses the default database path from the
#' `CODEMINER_DB_PATH` environment variable (or `rappdirs` default).
#'
#' @param main Path to the codeminer DuckDB database file. Defaults to the
#'   path from `CODEMINER_DB_PATH` env var or
#'   `rappdirs::user_data_dir("codeminer")`.
#'
#' @return The DBI connection object, invisibly.
#' @export
#' @family Workbench management
#' @seealso [codeminer_disconnect()], [codeminer_status()]
codeminer_connect <- function(main = NULL) {
  main_was_explicit <- !is.null(main)
  if (is.null(main)) {
    main <- db_path()
  }
  # DuckDB's ATTACH does not expand `~`, so an unexpanded home-relative path
  # would silently attach a new, empty database at a literal "~/..." location.
  main <- path.expand(main)

  if (!main_was_explicit) {
    codeminer_inform(c(
      "i" = "Using database at {.file {main}}",
      "i" = paste(
        "Set {.envvar CODEMINER_DB_PATH} or use",
        "{.fun codeminer_connect} to change this."
      )
    ))
  }

  # Tear down existing workbench if one exists
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }

  # Create in-memory workbench
  .codeminer_env$con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  .codeminer_env$db_path <- NULL

  # If anything below throws (gate refusal, failed ATTACH, ...) the
  # workbench would be left valid but with no main file attached. The
  # next `get_db_con()` call would then short-circuit on the cached con
  # and queries would hit raw DuckDB catalog errors instead of the
  # friendly gate message. Use on.exit to tear down on any early exit
  # and clear the guard on success.
  connection_complete <- FALSE
  on.exit(
    {
      if (!connection_complete) {
        if (
          exists("con", envir = .codeminer_env) &&
            DBI::dbIsValid(.codeminer_env$con)
        ) {
          try(
            DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE),
            silent = TRUE
          )
        }
        if (exists("con", envir = .codeminer_env)) {
          rm("con", envir = .codeminer_env)
        }
        .codeminer_env$db_path <- NULL
      }
    },
    add = TRUE
  )

  # Attach main (read-only). A backend may be either a `.duckdb` file or
  # a `parquet_folder` directory; `backend_kind()` picks the right path
  # mechanically.
  exists_at_main <- backend_database_exists(main)
  if (main_was_explicit && !exists_at_main) {
    codeminer_abort(c(
      "Database not found at {.file {main}}.",
      "i" = paste(
        "To create a new database, use",
        "{.code Sys.setenv(CODEMINER_DB_PATH = ...)} then",
        "{.fun build_database}."
      )
    ))
  }
  if (exists_at_main) {
    # Gate the schema version BEFORE the read-only ATTACH so an explicit
    # rebuild request can open its own write connection without lock
    # contention.
    enforce_schema_gate(main)
    backend_attach(.codeminer_env$con, CODEMINER_SCHEMA, main)
    .codeminer_env$db_path <- main
    # Recorded so that `get_db_con()` does not replace an explicitly chosen
    # database with `CODEMINER_DB_PATH` on the next query.
    .codeminer_env$main_explicit <- main_was_explicit
    codeminer_set_search_path()
  }

  # Clear cached version selections — they may refer to a different database
  .codeminer_env$active_versions <- list()
  # Re-validate the `codeminer.col_filters` option against the new metadata
  clear_option_col_filters_cache()

  # Cache metadata from the attached database
  codeminer_refresh_cache()

  connection_complete <- TRUE
  invisible(.codeminer_env$con)
}

#' Disconnect the codeminer workbench
#'
#' Tears down the in-memory DuckDB connection and clears cached metadata.
#' Non-connection state (e.g. extracted file paths) is preserved.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
codeminer_disconnect <- function() {
  if (
    exists("con", envir = .codeminer_env) && DBI::dbIsValid(.codeminer_env$con)
  ) {
    DBI::dbDisconnect(.codeminer_env$con, shutdown = TRUE)
  }
  for (field in c(
    "con",
    "db_path",
    "main_explicit",
    "metadata",
    "active_versions",
    "active_col_filters",
    "option_col_filters"
  )) {
    if (exists(field, envir = .codeminer_env)) {
      rm(list = field, envir = .codeminer_env)
    }
  }
  invisible()
}

#' Show workbench status
#'
#' Prints the current state of the workbench connection: the attached
#' database, pinned versions and the `codeminer.col_filters` option when set.
#'
#' @return The attached database path, invisibly (`NULL` if none attached).
#' @export
#' @family Workbench management
codeminer_status <- function() {
  if (
    !exists("con", envir = .codeminer_env) ||
      !DBI::dbIsValid(.codeminer_env$con)
  ) {
    cli::cli_inform("No active workbench connection.")
    return(invisible(NULL))
  }
  db_path <- .codeminer_env$db_path
  if (is.null(db_path) || !nzchar(db_path)) {
    db_path <- "not attached"
  }
  msgs <- c(
    "i" = "Workbench active",
    " " = "Database: {.file {db_path}}"
  )

  # Show pinned versions if any
  pins <- .codeminer_env$active_versions
  if (length(pins) > 0) {
    msgs <- c(msgs, "i" = "Active versions:")
    for (type in names(pins)) {
      for (key in names(pins[[type]])) {
        # Pre-render: cli interpolates `msgs` only when emitted below, by
        # which point the loop variables would all hold their final values.
        msgs <- c(
          msgs,
          " " = escape_cli(cli::format_inline(
            "  {type}: {.val {key}} = {.val {pins[[type]][[key]]}}"
          ))
        )
      }
    }
  }

  # Show the codeminer.col_filters option if set
  cf_option <- tryCatch(
    option_col_filters(),
    codeminer_col_filters_invalid = function(e) "invalid"
  )
  if (identical(cf_option, "invalid")) {
    msgs <- c(
      msgs,
      "x" = "Option {.code codeminer.col_filters}: invalid (queries will error)."
    )
  } else if (is_col_filters_off(cf_option)) {
    msgs <- c(
      msgs,
      "i" = "Option {.code codeminer.col_filters}: NA (no filtering)."
    )
  } else if (length(cf_option) > 0) {
    msgs <- c(msgs, "i" = "Option {.code codeminer.col_filters}:")
    for (type in names(cf_option)) {
      for (key in names(cf_option[[type]])) {
        filter_desc <- paste(
          format_filter_entry(cf_option[[type]][[key]]),
          collapse = "; "
        )
        # Pre-render: cli interpolates `msgs` only when emitted below, by
        # which point the loop variables would all hold their final values.
        msgs <- c(
          msgs,
          " " = escape_cli(cli::format_inline(
            "  {type}: {.val {key}} -> {filter_desc}"
          ))
        )
      }
    }
  }

  cli::cli_inform(msgs)
  invisible(.codeminer_env$db_path)
}

#' Refresh the metadata cache
#'
#' Re-reads metadata tables from all attached databases and updates
#' the internal cache. Called automatically by [codeminer_connect()] and
#' after write operations.
#'
#' This refreshes the *metadata inventory* (which tables and versions exist
#' in the database). It does not affect version selections — use
#' [codeminer_clear_versions()] to reset which version is used for each
#' code type.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
codeminer_refresh_cache <- function() {
  con <- .codeminer_env$con
  .codeminer_env$metadata <- list()

  if (is.null(.codeminer_env$db_path)) {
    return(invisible())
  }

  schema <- CODEMINER_SCHEMA
  for (type in c("lookup", "mapping", "relationship")) {
    tbl_name <- codeminer_metadata_table_names[[type]]
    if (schema_table_exists(con, schema, tbl_name)) {
      qualified <- paste0(schema, ".", tbl_name)
      meta_df <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", qualified))
      if (nrow(meta_df) > 0) {
        meta_df$.schema <- schema
        .codeminer_env$metadata[[type]] <- meta_df
      }
    }
  }

  invisible()
}


#' Pin table versions for the session
#'
#' Overrides the default "latest" version resolution for lookup, relationship,
#' and/or mapping tables. Pinned versions persist until cleared with
#' [codeminer_clear_versions()] or [codeminer_disconnect()].
#'
#' @param lookup Named character vector of lookup versions, keyed by code type.
#'   E.g. `c("ICD-10" = "v42", "Read v3" = "v1")`.
#' @param relationship Named character vector of relationship versions, keyed
#'   by code type.
#' @param mapping Named character vector of mapping versions, keyed by
#'   `"from > to"` pairs. E.g. `c("Read 3 > ICD-10" = "v1")`.
#'
#' @details
#' Pinned versions only affect "latest" resolution. Explicit version arguments
#' on query functions (e.g. `CODES(..., lookup_version = "v1")`) always take
#' precedence.
#'
#' Versions are also auto-cached the first time `"latest"` is resolved for a
#' given code type. Calling `codeminer_set_version()` overrides any
#' auto-cached version.
#'
#' New pins are merged with existing ones. To replace all pins, call
#' [codeminer_clear_versions()] first.
#'
#' @return The current pinned versions (a list), invisibly.
#' @export
#' @family Workbench management
#' @examples
#' \dontrun{
#' # Pin lookup versions for multiple code types
#' codeminer_set_version(
#'   lookup = c("ICD-10" = "v42", "Read v3" = "v1")
#' )
#'
#' # Pin mapping version for a specific pair
#' codeminer_set_version(
#'   mapping = c("Read 3 > ICD-10" = "v1")
#' )
#'
#' # Clear all pins
#' codeminer_clear_versions()
#' }
codeminer_set_version <- function(
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  if (is.null(lookup) && is.null(relationship) && is.null(mapping)) {
    codeminer_abort(
      "At least one of {.arg lookup}, {.arg relationship}, or {.arg mapping} must be provided."
    )
  }

  # Trim whitespace from keys and values (users may copy-paste from configs)
  if (!is.null(lookup)) {
    lookup <- trim_pins(lookup)
  }
  if (!is.null(relationship)) {
    relationship <- trim_pins(relationship)
  }
  if (!is.null(mapping)) {
    mapping <- trim_pins(mapping)
  }

  # Reject "latest" as a pin value — it's the sentinel we're overriding
  all_pins <- c(lookup, relationship, mapping)
  if (any(all_pins == "latest")) {
    codeminer_abort(
      "{.val latest} cannot be used as a pinned version. Specify the actual version string instead."
    )
  }

  # Initialise if needed
  if (is.null(.codeminer_env$active_versions)) {
    .codeminer_env$active_versions <- list()
  }

  # Validate and merge each type
  if (!is.null(lookup)) {
    validate_version_pins(lookup, "lookup", "lookup_version", "code_type")
    .codeminer_env$active_versions$lookup <- merge_pins(
      .codeminer_env$active_versions$lookup,
      lookup
    )
  }
  if (!is.null(relationship)) {
    validate_version_pins(
      relationship,
      "relationship",
      "relationship_version",
      "code_type"
    )
    .codeminer_env$active_versions$relationship <- merge_pins(
      .codeminer_env$active_versions$relationship,
      relationship
    )
  }
  if (!is.null(mapping)) {
    mapping <- normalize_mapping_keys(mapping)
    validate_version_pins(mapping, "mapping", "map_version", NULL)
    .codeminer_env$active_versions$mapping <- merge_pins(
      .codeminer_env$active_versions$mapping,
      mapping
    )
  }

  invisible(.codeminer_env$active_versions)
}

#' Clear active version selections
#'
#' Removes version selections from the current session. This covers both
#' versions pinned explicitly with [codeminer_set_version()] and versions
#' auto-cached on first `"latest"` resolution.
#'
#' Called with no arguments, all version selections are cleared and subsequent
#' queries will re-resolve `"latest"` from the database. To clear only
#' specific code types, pass them as character vectors.
#'
#' @param lookup Character vector of code types whose lookup version should
#'   be cleared (e.g. `"ICD-10"`). `NULL` (default) means no lookup versions
#'   are cleared unless all arguments are `NULL`.
#' @param relationship Character vector of code types whose relationship
#'   version should be cleared. `NULL` (default) means no relationship
#'   versions are cleared unless all arguments are `NULL`.
#' @param mapping Character vector of mapping keys (`"from > to"` format)
#'   whose version should be cleared. `NULL` (default) means no mapping
#'   versions are cleared unless all arguments are `NULL`.
#'
#' @return `NULL`, invisibly.
#' @export
#' @family Workbench management
#' @examples
#' \dontrun{
#' # Clear all version selections
#' codeminer_clear_versions()
#'
#' # Clear only the ICD-10 lookup version
#' codeminer_clear_versions(lookup = "ICD-10")
#'
#' # Clear lookup and relationship versions for SNOMED CT
#' codeminer_clear_versions(
#'   lookup = "SNOMED CT",
#'   relationship = "SNOMED CT"
#' )
#' }
codeminer_clear_versions <- function(
  lookup = NULL,
  relationship = NULL,
  mapping = NULL
) {
  if (is.null(lookup) && is.null(relationship) && is.null(mapping)) {
    .codeminer_env$active_versions <- list()
  } else {
    for (key in lookup) {
      .codeminer_env$active_versions[["lookup"]][[key]] <- NULL
    }
    for (key in relationship) {
      .codeminer_env$active_versions[["relationship"]][[key]] <- NULL
    }
    for (key in mapping) {
      .codeminer_env$active_versions[["mapping"]][[key]] <- NULL
    }
  }
  invisible()
}


#' Temporarily override column filters
#'
#' Applies column filters for the duration of the supplied expression, then
#' restores the previous setting (even on error). This is useful when you
#' need different filters for a group of calls without changing the setting
#' for the whole session. For a single call, prefer the `col_filters`
#' argument on the query function itself (see [CODES()]) — it has the same
#' semantics, scoped to that call.
#'
#' @param col_filters The filters to apply: a type-layered list or
#'   [get_col_filters()] object (each table entry replaces that table's
#'   current filters wholesale), or `NULL` / `NA` for no filtering at all.
#' @param code Code to execute with the temporary filters. The scope covers
#'   query *construction*: every codeminer read made while `code` evaluates
#'   resolves against the temporary filters.
#'
#' @details
#' Note the argument order (filters first, code last, as in
#' `withr::with_*()`): wrap the expression rather than piping into this
#' function.
#'
#' The function works through the `codeminer.col_filters` option: it merges
#' `col_filters` into the option's current value by table key, sets the
#' option for the duration of `code`, and restores the previous value
#' afterwards. Tables `col_filters` does not name keep whatever the option
#' or the metadata defaults give them. Because the option is the mechanism,
#' the scope applies to any code that reads the option, not just to the
#' query functions in this package. See `vignette("configure-queries")`.
#'
#' @return The result of evaluating `code`.
#' @export
#' @family Workbench management
#' @seealso [get_col_filters()], [codeminer_status()]
#' @examples
#' \dontrun{
#' # Temporarily include inactive SNOMED concepts
#' with_col_filters(
#'   list(lookup = list("SNOMED CT" = list(active_concept = c("0", "1")))),
#'   CODES("all", type = "SNOMED CT")
#' )
#'
#' # Temporarily disable all filtering
#' with_col_filters(NA, CODES("all", type = "SNOMED CT"))
#' }
with_col_filters <- function(col_filters, code) {
  if (identical(col_filters, "default")) {
    return(force(code))
  }

  if (is.null(col_filters) || is_col_filters_off(col_filters)) {
    new_option <- NA
  } else {
    overlay <- validate_col_filters_value(
      col_filters,
      call = rlang::current_env()
    )
    current <- option_col_filters()
    if (is.null(current) || is_col_filters_off(current)) {
      current <- list()
    }
    for (type in names(overlay)) {
      current[[type]] <- merge_col_filter_tables(
        current[[type]],
        overlay[[type]]
      )
    }
    new_option <- current
  }

  # The merged value is already validated: prime the cache so the first
  # query inside the scope does not warn a second time, and restore it with
  # the option afterwards.
  old_cache <- .codeminer_env$option_col_filters
  on.exit(.codeminer_env$option_col_filters <- old_cache, add = TRUE)
  if (!is_col_filters_off(new_option)) {
    .codeminer_env$option_col_filters <- list(
      raw = new_option,
      validated = new_option
    )
  }

  withr::with_options(list(codeminer.col_filters = new_option), code)
}


# Internal helpers --------------------------------------------------------

#' Set the DuckDB search path to the attached main database
#' @noRd
codeminer_set_search_path <- function() {
  if (is.null(.codeminer_env$db_path)) {
    return(invisible())
  }
  DBI::dbExecute(
    .codeminer_env$con,
    paste0("SET search_path = '", CODEMINER_SCHEMA, "'")
  )
}

#' Check if a table exists in a specific attached database (catalog)
#'
#' In DuckDB, ATTACHed databases are catalogs (not schemas). The default
#' schema within each catalog is "main".
#' @noRd
schema_table_exists <- function(con, schema, tbl_name) {
  query <- glue::glue_sql(
    "SELECT 1 FROM information_schema.tables
     WHERE table_catalog = {schema}
     AND table_name = {tbl_name}",
    .con = con
  )
  nrow(DBI::dbGetQuery(con, query)) > 0
}

#' Validate version pins against cached metadata
#' @noRd
validate_version_pins <- function(
  pins,
  type,
  version_col,
  key_col,
  call = rlang::caller_env()
) {
  if (!rlang::is_named(pins) || !is.character(pins)) {
    codeminer_abort(
      "{.arg {type}} must be a named character vector.",
      call = call
    )
  }

  # Validate mapping key format (always, regardless of metadata availability)
  if (type == "mapping") {
    for (key in names(pins)) {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      if (length(parts) != 2) {
        codeminer_abort(
          "Mapping key {.val {key}} must be in {.val from > to} format.",
          call = call
        )
      }
    }
  }

  # Cross-check against cached metadata (skip if metadata not available)
  meta <- .codeminer_env$metadata[[type]]
  if (is.null(meta)) {
    return(invisible())
  }

  for (i in seq_along(pins)) {
    key <- names(pins)[[i]]
    version <- pins[[i]]

    if (type == "mapping") {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      match <- meta$from_code_type == parts[1] &
        meta$to_code_type == parts[2] &
        meta[[version_col]] == version
    } else {
      match <- meta[[key_col]] == key & meta[[version_col]] == version
    }

    if (!any(match)) {
      codeminer_warn(c(
        "!" = "No {type} metadata found for {.val {key}} version {.val {version}}.",
        "i" = "Pin will be set but may not resolve to a valid table."
      ))
    }
  }
  invisible()
}

#' Trim leading/trailing whitespace from pin names and values
#' @noRd
trim_pins <- function(pins) {
  names(pins) <- trimws(names(pins))
  pins[] <- trimws(pins)
  pins
}

#' Normalize mapping pin keys to canonical "from > to" form
#'
#' Accepts flexible spacing (e.g. "Read 3>ICD-10", "Read 3 >  ICD-10") and
#' converts to the canonical form used internally by `get_metadata_for_mapping()`.
#' @noRd
normalize_mapping_keys <- function(pins) {
  new_names <- vapply(
    names(pins),
    function(key) {
      parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
      paste(parts, collapse = " > ")
    },
    character(1),
    USE.NAMES = FALSE
  )
  names(pins) <- new_names
  pins
}

#' Merge new pins into existing pins (overwrite by name)
#' @noRd
merge_pins <- function(existing, new_pins) {
  if (is.null(existing)) {
    return(as.list(new_pins))
  }
  existing <- as.list(existing)
  # New pins overwrite existing ones with the same name
  for (nm in names(new_pins)) {
    existing[[nm]] <- new_pins[[nm]]
  }
  existing
}

#' Merge one type's layer of column filters (new entries overwrite by key)
#' @noRd
merge_col_filter_tables <- function(existing, new_entries) {
  if (is.null(existing)) {
    return(new_entries)
  }
  existing[names(new_entries)] <- new_entries
  existing
}
