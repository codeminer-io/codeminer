#' Is `x` the NA "unfiltered" sentinel?
#'
#' `NA` means "unfiltered" wherever it appears in column-filter state: as the
#' whole filter state (nothing is filtered), as a table entry (that table is
#' unfiltered), or as a column's values (that column is unfiltered, i.e.
#' omitted from the replacement set).
#'
#' @param x Any value.
#' @return `TRUE` if `x` is a length-1 non-list `NA`.
#' @keywords internal
#' @noRd
is_col_filters_off <- function(x) {
  length(x) == 1 && !is.list(x) && is.na(x)
}

#' Lightweight class for column-filter specifications
#'
#' @param x A type-layered list (`lookup` / `relationship` / `mapping`, each
#'   keyed by code type or `"from > to"`).
#' @param defaults_only Whether `x` holds applied values only (`TRUE`) or the
#'   full `values`/`defaults` specification (`FALSE`).
#' @return A `codeminer_col_filters` object (list with additional class).
#' @keywords internal
#' @noRd
new_col_filters <- function(x, defaults_only = TRUE) {
  class(x) <- c("codeminer_col_filters", class(x))
  attr(x, "defaults_only") <- defaults_only
  x
}

#' Format a character vector of filter values for display
#' @keywords internal
#' @noRd
format_filter_values <- function(values) {
  if (length(values) == 0) {
    return("(none)")
  }
  paste(values, collapse = ", ")
}

#' Format one table's filter entry as display lines
#'
#' Shared by [print.codeminer_col_filters()] and [codeminer_status()].
#'
#' @param entry A table's filter entry: a named list of `column = values`
#'   pairs (or `column = list(values =, defaults =)` when `full_spec`), the
#'   `NA` sentinel, or an empty list.
#' @param full_spec Whether entries hold the full `values`/`defaults` spec.
#' @return A character vector: one or more lines per column, or a single
#'   `"(unfiltered)"` / `"(no filters)"` line. In `full_spec` mode a column
#'   may contribute extra indented `description` / `value_labels` lines.
#' @keywords internal
#' @noRd
format_filter_entry <- function(entry, full_spec = FALSE) {
  if (is_col_filters_off(entry)) {
    return("(unfiltered)")
  }
  if (length(entry) == 0) {
    return("(no filters)")
  }
  unlist(
    lapply(
      names(entry),
      function(col) {
        value <- entry[[col]]
        if (full_spec) {
          lines <- paste0(
            col,
            ": defaults ",
            format_filter_values(value$defaults),
            " (values: ",
            format_filter_values(value$values),
            ")"
          )
          if (!is.null(value$description)) {
            lines <- c(lines, paste0("  description: ", value$description))
          }
          if (!is.null(value$value_labels)) {
            labels <- value$value_labels
            lines <- c(
              lines,
              "  value_labels:",
              paste0("    ", names(labels), " = ", labels)
            )
          }
          lines
        } else if (is_col_filters_off(value)) {
          paste0(col, ": (unfiltered)")
        } else {
          paste0(col, ": ", format_filter_values(value))
        }
      }
    ),
    use.names = FALSE
  )
}

#' Format one filter leaf (a column's value vector) for the tree display
#'
#' @param value An atomic vector (values / defaults / description), a named
#'   vector (`value_labels`, shown as `name = value`), the `NA` sentinel, or
#'   an empty vector.
#' @return A single string.
#' @keywords internal
#' @noRd
format_col_filter_leaf <- function(value) {
  if (is_col_filters_off(value)) {
    return("(unfiltered)")
  }
  if (length(value) == 0) {
    return("(none)")
  }
  nms <- names(value)
  rendered <- if (!is.null(nms) && all(nzchar(nms))) {
    # Named leaf (e.g. value_labels): show the value -> label mapping.
    paste(paste0(nms, " = ", value), collapse = ", ")
  } else {
    paste(value, collapse = ", ")
  }
  # Keep lines tidy: long descriptions / label sets are truncated for display.
  if (nchar(rendered) > 60) {
    rendered <- paste0(substr(rendered, 1, 59), "\u2026")
  }
  rendered
}

#' Render a nested named list as a tree with box-drawing connectors
#'
#' Structural display shared by [print.codeminer_col_filters()]. Branch nodes
#' (lists) print their name; leaf nodes print `name: value`. Styled after
#' `lobstr::tree()`.
#'
#' @param x A named list.
#' @param prefix The accumulated indentation prefix for this level.
#' @keywords internal
#' @noRd
cat_col_filter_tree <- function(x, prefix = "") {
  nms <- names(x)
  n <- length(x)
  for (i in seq_len(n)) {
    last <- i == n
    connector <- if (last) "\u2514\u2500" else "\u251c\u2500"
    child_prefix <- paste0(prefix, if (last) "  " else "\u2502 ")
    value <- x[[i]]
    if (is.list(value)) {
      cli::cat_line(prefix, connector, nms[[i]])
      cat_col_filter_tree(value, child_prefix)
    } else {
      cli::cat_line(
        prefix,
        connector,
        nms[[i]],
        ": ",
        format_col_filter_leaf(value)
      )
    }
  }
  invisible(x)
}

#' Print method for codeminer_col_filters
#'
#' @param x A `codeminer_col_filters` object.
#' @param ... Additional arguments (unused).
#' @export
#' @keywords internal
print.codeminer_col_filters <- function(x, ...) {
  # cat_line rather than cli_text throughout: print methods write to stdout,
  # and cli trims the leading whitespace that renders the tree.
  full_spec <- identical(attr(x, "defaults_only"), FALSE)
  cli::cat_line(
    "<codeminer_col_filters>",
    if (full_spec) "  (full specification)" else ""
  )
  body <- unclass(x)
  attr(body, "defaults_only") <- NULL
  if (length(body) == 0) {
    cli::cat_line("No column filters registered.")
    return(invisible(x))
  }
  cat_col_filter_tree(body)
  invisible(x)
}

# --- Registry lookups (metadata cache only, no table scans) --------------------

#' Registered filter keys for a table type
#'
#' Keys come from the cached metadata: code types for lookup/relationship
#' tables, and `"from > to"` pairs for mappings. Mapping keys include the
#' reversed form of every registered row, so a filter keyed by either
#' direction of a reverse-swapped mapping validates.
#'
#' @param type One of "lookup", "relationship", or "mapping".
#' @return A character vector of keys, or `NULL` when the cache is empty.
#' @keywords internal
#' @noRd
registered_col_filter_keys <- function(type) {
  meta <- .codeminer_env$metadata[[type]]
  if (is.null(meta) || nrow(meta) == 0) {
    return(NULL)
  }
  if (type == "mapping") {
    unique(c(
      paste(meta$from_code_type, ">", meta$to_code_type),
      paste(meta$to_code_type, ">", meta$from_code_type)
    ))
  } else {
    unique(meta$code_type)
  }
}

#' Metadata row for a registered (type, key) pair
#'
#' Best effort: when several versions are registered for a key, the last row
#' is used. Filters apply to whichever version resolves at query time, so
#' validation against one version's table/spec is advisory only.
#'
#' @param type One of "lookup", "relationship", or "mapping".
#' @param key A code type, or a normalised `"from > to"` pair (either
#'   direction matches for mappings).
#' @return A single metadata row (data frame), or `NULL` when not found.
#' @keywords internal
#' @noRd
registered_meta_row_for_key <- function(type, key) {
  meta <- .codeminer_env$metadata[[type]]
  if (is.null(meta) || nrow(meta) == 0) {
    return(NULL)
  }
  rows <- if (type == "mapping") {
    parts <- trimws(strsplit(key, ">", fixed = TRUE)[[1]])
    if (length(parts) != 2) {
      return(NULL)
    }
    meta[
      (meta$from_code_type == parts[1] & meta$to_code_type == parts[2]) |
        (meta$from_code_type == parts[2] & meta$to_code_type == parts[1]),
    ]
  } else {
    meta[meta$code_type == key, ]
  }
  if (nrow(rows) == 0) {
    return(NULL)
  }
  rows[nrow(rows), ]
}

# --- Validation of user-supplied column filters --------------------------------

#' Escape cli/glue braces in a pre-rendered string
#' @keywords internal
#' @noRd
escape_cli <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

#' Validate and normalise a user-supplied column-filter value
#'
#' Shared by the `col_filters` argument on query functions,
#' [with_col_filters()], and [codeminer_set_col_filters()]. Aborts on
#' malformed shapes (including the pre-#170 flat `list(column = values)`
#' form) and emits one consolidated warning for entries that do not match
#' the registered tables.
#'
#' @param x A type-layered list or `codeminer_col_filters` object.
#' @param call The calling environment for error messages.
#' @return The normalised type-layered list: mapping keys canonicalised,
#'   `NA`/`NULL` table entries normalised to `NA`, values coerced to
#'   character, `NA` column entries dropped.
#' @keywords internal
#' @noRd
validate_col_filters_value <- function(x, call = rlang::caller_env()) {
  if (!is.list(x)) {
    codeminer_abort(
      paste0(
        "{.arg col_filters} must be {.val default}, {.code NULL}, ",
        "{.code NA}, or a table-keyed list."
      ),
      class = "codeminer_col_filters_invalid",
      call = call
    )
  }

  if (identical(attr(x, "defaults_only"), FALSE)) {
    codeminer_abort(
      c(
        "{.arg col_filters} looks like {.code get_col_filters(defaults_only = FALSE)} output.",
        "i" = "Pass {.code get_col_filters()} output (applied values only) instead."
      ),
      class = "codeminer_col_filters_invalid",
      call = call
    )
  }

  known_types <- c("lookup", "relationship", "mapping")
  nms <- names(x)
  if (
    length(x) > 0 &&
      (is.null(nms) || any(!nzchar(nms)) || !all(nms %in% known_types))
  ) {
    codeminer_abort(
      c(
        paste0(
          "{.arg col_filters} must be a table-keyed list with top-level ",
          "names {.val lookup}, {.val relationship}, and/or {.val mapping}."
        ),
        "i" = paste0(
          "Example: {.code list(lookup = list(\"SNOMED CT\" = ",
          "list(active_concept = \"1\")))}."
        ),
        "i" = "Each table entry replaces that table's default filters wholesale.",
        "i" = "See {.fun get_col_filters} for the registered filters."
      ),
      class = "codeminer_col_filters_invalid",
      call = call
    )
  }

  x <- unclass(x)
  attr(x, "defaults_only") <- NULL

  for (type in names(x)) {
    layer <- x[[type]]
    if (is.null(layer)) {
      x[[type]] <- NULL
      next
    }
    if (type == "mapping" && length(layer) > 0) {
      layer <- normalize_mapping_keys(layer)
    }
    x[[type]] <- validate_col_filter_layer(layer, type, call = call)
  }

  check_col_filters_registered(x)
  x
}

#' Validate one type's layer of column filters
#'
#' Structural validation shared by the query-argument path and session pins:
#' a named list keyed by table, each entry either the `NA` sentinel
#' ("unfiltered") or a named list of `column = c(values)` pairs.
#'
#' @param pins A named list keyed by code type or `"from > to"` pair.
#' @param type One of "lookup", "relationship", or "mapping" (for messages).
#' @param call The calling environment for error messages.
#' @return The normalised layer.
#' @keywords internal
#' @noRd
validate_col_filter_layer <- function(pins, type, call = rlang::caller_env()) {
  if (!is.list(pins) || (length(pins) > 0 && !rlang::is_named(pins))) {
    codeminer_abort(
      "{.arg {type}} must be a named list.",
      call = call
    )
  }

  for (key in names(pins)) {
    filters <- pins[[key]]
    # NA (and a present-but-NULL element, as parsed from a JSON null) both
    # mean "no filters for this table".
    if (is.null(filters) || is_col_filters_off(filters)) {
      pins[key] <- list(NA)
      next
    }
    if (
      !is.list(filters) || (length(filters) > 0 && !rlang::is_named(filters))
    ) {
      codeminer_abort(
        c(
          "Column filters for {.val {key}} must be a named list of {.code column = c(values)} pairs.",
          "i" = "Use {.code NA} to un-filter the table."
        ),
        call = call
      )
    }
    for (col in names(filters)) {
      values <- filters[[col]]
      if (is.list(values) && all(c("values", "defaults") %in% names(values))) {
        codeminer_abort(
          c(
            paste0(
              "Column {.field {col}} for {.val {key}} looks like a full ",
              "filter specification ({.field values} + {.field defaults})."
            ),
            "i" = "Pass applied values only, e.g. {.code get_col_filters()} output."
          ),
          class = "codeminer_col_filters_invalid",
          call = call
        )
      }
      # NA (or JSON null) at column level means "this column unfiltered" —
      # i.e. omit it from the replacement set.
      if (is.null(values) || is_col_filters_off(values)) {
        pins[[key]][[col]] <- NULL
        next
      }
      # jsonlite parses an empty JSON array as list(); treat as an empty
      # selection (matches no rows).
      if (is.list(values) && length(values) == 0) {
        values <- character(0)
      }
      if (!is.atomic(values) || anyNA(values)) {
        codeminer_abort(
          "Filter values for column {.val {col}} in {.val {key}} must be an atomic vector without {.code NA}s.",
          call = call
        )
      }
      pins[[key]][[col]] <- as.character(values)
    }
  }
  pins
}

#' Warn about filters that do not match the registered tables
#'
#' Lenient, cache-based check: an unknown key (matching no registered table
#' at all), a column absent from a known key's table, or a value outside a
#' column's registered `values` each warn once, consolidated. Entries for
#' registered tables a query does not use are silently ignored by design, so
#' they never warn. Skipped entirely when no database is connected.
#'
#' @param x A normalised type-layered filter list.
#' @keywords internal
#' @noRd
check_col_filters_registered <- function(x) {
  meta_cache <- .codeminer_env$metadata
  if (is.null(meta_cache) || length(meta_cache) == 0) {
    return(invisible())
  }

  bullets <- character()
  for (type in names(x)) {
    registered <- registered_col_filter_keys(type)
    if (is.null(registered)) {
      next
    }
    layer <- x[[type]]
    for (key in names(layer)) {
      if (!key %in% registered) {
        bullets <- c(
          bullets,
          "x" = escape_cli(cli::format_inline(
            "No registered {type} table keyed {.val {key}}. Registered {type} keys: {.val {registered}}."
          ))
        )
        next
      }
      entry <- layer[[key]]
      if (is_col_filters_off(entry) || length(entry) == 0) {
        next
      }
      row <- registered_meta_row_for_key(type, key)
      if (is.null(row)) {
        next
      }
      table_cols <- tryCatch(
        DBI::dbListFields(
          .codeminer_env$con,
          row[[paste0(type, "_table_name")]]
        ),
        error = function(e) NULL
      )
      if (!is.null(table_cols)) {
        bad_cols <- setdiff(names(entry), table_cols)
        if (length(bad_cols) > 0) {
          bullets <- c(
            bullets,
            "x" = escape_cli(cli::format_inline(paste0(
              "{cli::qty(bad_cols)}Column{?s} {.field {bad_cols}} not in ",
              "the {type} table for {.val {key}}. ",
              "Available columns: {.field {table_cols}}."
            )))
          )
        }
      }
      spec <- deserialise_col_filters(row$col_filters)
      if (!is.null(spec)) {
        for (col in intersect(names(entry), names(spec))) {
          bad_values <- setdiff(entry[[col]], spec[[col]]$values)
          if (length(bad_values) > 0) {
            bullets <- c(
              bullets,
              "x" = escape_cli(cli::format_inline(paste0(
                "{cli::qty(bad_values)}Value{?s} {.val {bad_values}} for ",
                "column {.field {col}} in {.val {key}} not among the ",
                "registered values: {.val {spec[[col]]$values}}."
              )))
            )
          }
        }
      }
    }
  }

  if (length(bullets) > 0) {
    codeminer_warn(
      c(
        "!" = "Some column filters do not match the registered tables:",
        bullets,
        "i" = "Filters for tables a query does not use are ignored.",
        "i" = "See {.fun get_col_filters} for the registered filters."
      ),
      class = "codeminer_col_filters_unmatched"
    )
  }
  invisible()
}

#' Abort when a per-type pin argument is given a whole filter object
#'
#' Catches `codeminer_set_col_filters(lookup = get_col_filters())` style
#' mistakes, which would otherwise fail with a confusing low-level error.
#'
#' @param x The per-type argument value.
#' @param arg The argument name (for the message).
#' @param call The calling environment for error messages.
#' @keywords internal
#' @noRd
check_not_whole_col_filters <- function(x, arg, call = rlang::caller_env()) {
  looks_whole <- inherits(x, "codeminer_col_filters")
  if (!looks_whole && is.list(x) && length(x) > 0 && rlang::is_named(x)) {
    looks_whole <- all(names(x) %in% c("lookup", "relationship", "mapping"))
  }
  if (looks_whole) {
    codeminer_abort(
      c(
        "{.arg {arg}} looks like a whole column-filter object (type-layered list).",
        "i" = "Did you mean {.code codeminer_set_col_filters(col_filters = x)}?"
      ),
      call = call
    )
  }
  invisible()
}

#' Inform when session pins replace registered default filters
#'
#' A pin replaces its table's default filters wholesale, so a pin that does
#' not restate a default column silently drops that default. Emitted by
#' [codeminer_set_col_filters()] only — scoped filters (the `col_filters`
#' argument and [with_col_filters()]) are visible at the call site and stay
#' quiet.
#'
#' @param pins_by_type A normalised type-layered filter list.
#' @keywords internal
#' @noRd
inform_replaced_col_filter_defaults <- function(pins_by_type) {
  meta_cache <- .codeminer_env$metadata
  if (is.null(meta_cache) || length(meta_cache) == 0) {
    return(invisible())
  }

  bullets <- character()
  for (type in names(pins_by_type)) {
    layer <- pins_by_type[[type]]
    for (key in names(layer)) {
      entry <- layer[[key]]
      # Un-filtering via NA is explicit intent; nothing to flag
      if (is_col_filters_off(entry)) {
        next
      }
      row <- registered_meta_row_for_key(type, key)
      if (is.null(row)) {
        next
      }
      spec <- deserialise_col_filters(row$col_filters)
      if (is.null(spec)) {
        next
      }
      default_cols <- names(Filter(
        function(entry_spec) length(entry_spec$defaults) > 0,
        spec
      ))
      dropped <- setdiff(default_cols, names(entry))
      if (length(dropped) > 0) {
        bullets <- c(
          bullets,
          "i" = escape_cli(cli::format_inline(
            "Pin for {type} {.val {key}} replaces its default {cli::qty(dropped)}filter{?s} on {.field {dropped}}."
          ))
        )
      }
    }
  }

  if (length(bullets) > 0) {
    codeminer_inform(c(
      bullets,
      "i" = "To keep defaults, amend {.fun get_col_filters} output and pin that."
    ))
  }
  invisible()
}

# --- Call-scoped filter state ---------------------------------------------------

#' Apply a query's col_filters argument as a call-scoped overlay
#'
#' The `col_filters` argument on query functions is a per-call pin: each
#' table entry replaces that table's session pin / metadata defaults for the
#' duration of the calling function, and every internal read the query makes
#' resolves against the overlaid state. Callers pair this with
#' `on.exit(pop_col_filters(old), add = TRUE)`.
#'
#' @param col_filters The argument value: `"default"` (no change), `NULL` /
#'   `NA` (no filtering for any table), or a type-layered list.
#' @param call The calling environment for error messages.
#' @return `NULL` when nothing was changed, else the previous state wrapped
#'   in a list for [pop_col_filters()].
#' @keywords internal
#' @noRd
push_col_filters <- function(col_filters, call = rlang::caller_env()) {
  if (identical(col_filters, "default")) {
    return(NULL)
  }

  old <- list(state = .codeminer_env$active_col_filters)

  if (is.null(col_filters) || is_col_filters_off(col_filters)) {
    .codeminer_env$active_col_filters <- NA
    return(old)
  }

  overlay <- validate_col_filters_value(col_filters, call = call)

  state <- old$state
  if (is.null(state) || is_col_filters_off(state)) {
    state <- list()
  }
  for (type in names(overlay)) {
    state[[type]] <- merge_col_filter_pins(state[[type]], overlay[[type]])
  }
  .codeminer_env$active_col_filters <- state
  old
}

#' Restore the filter state saved by [push_col_filters()]
#' @keywords internal
#' @noRd
pop_col_filters <- function(old) {
  if (!is.null(old)) {
    .codeminer_env$active_col_filters <- old$state
  }
  invisible()
}

#' Hint bullet when active column filters may explain missing codes
#'
#' Returns an `"i"` bullet for [missing_codes_warning()]'s `extra` argument
#' when the table's resolved filters are non-`NULL`, so users discover that
#' filtering (defaults, pins, or the `col_filters` argument) may have
#' excluded their codes.
#'
#' @inheritParams resolve_col_filters
#' @return A named character vector, or `NULL` when no filters apply.
#' @keywords internal
#' @noRd
active_col_filters_hint <- function(
  metadata_col_filters,
  pin_type,
  pin_key,
  alt_keys = NULL
) {
  resolved <- resolve_col_filters(
    metadata_col_filters,
    pin_type,
    pin_key,
    alt_keys
  )
  if (is.null(resolved) || length(resolved) == 0) {
    return(NULL)
  }
  c(
    "i" = paste0(
      "Active column filters may exclude codes - see {.fun codeminer_status} ",
      "and the {.arg col_filters} argument."
    )
  )
}
