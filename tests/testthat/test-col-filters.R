# test-col-filters.R
# Tests for col_filters feature (GitHub issue #88)

# === Serialisation/deserialisation ==========================================

test_that("serialise/deserialise round-trips correctly", {
  spec <- list(
    active_concept = list(
      values = c("0", "1"),
      defaults = c("1")
    ),
    active_description = list(
      values = c("0", "1"),
      defaults = c("1")
    )
  )

  json <- serialise_col_filters(spec)
  expect_type(json, "character")
  expect_length(json, 1)

  result <- deserialise_col_filters(json)
  expect_identical(result, spec)
})

test_that("serialise(NULL) returns NA; deserialise(NA) returns NULL", {
  expect_identical(serialise_col_filters(NULL), NA_character_)
  expect_null(deserialise_col_filters(NA_character_))
  expect_null(deserialise_col_filters(NA))
  expect_null(deserialise_col_filters(""))
})

test_that("serialise validates structure", {
  # defaults not subset of values
  expect_error(
    serialise_col_filters(list(
      col = list(values = c("a", "b"), defaults = c("c"))
    )),
    "subset"
  )

  # missing 'values'
  expect_error(
    serialise_col_filters(list(col = list(defaults = c("a")))),
    "values.*defaults"
  )

  # missing 'defaults'
  expect_error(
    serialise_col_filters(list(col = list(values = c("a")))),
    "values.*defaults"
  )

  # not a named list
  expect_error(
    serialise_col_filters(list(c("a", "b"))),
    "named list"
  )

  # non-character values are coerced (no error)
  expect_no_error(
    serialise_col_filters(list(col = list(values = 1:3, defaults = 1L)))
  )
})

test_that("serialise/deserialise round-trips description and value_labels", {
  spec <- list(
    moduleId_concept = list(
      values = c("111", "222", "333"),
      defaults = character(0),
      description = "SNOMED CT module the concept belongs to.",
      value_labels = c("111" = "Core module", "333" = "UK drug extension")
    )
  )

  json <- serialise_col_filters(spec)
  result <- deserialise_col_filters(json)

  # Full round-trip, including value_labels' names surviving.
  expect_identical(result, spec)
  expect_identical(
    names(result$moduleId_concept$value_labels),
    c("111", "333")
  )
})

test_that("a spec with neither field serialises byte-for-byte as before", {
  spec <- list(
    active_concept = list(values = c("0", "1"), defaults = c("1"))
  )
  # The documentation fields are purely additive: omitting them must not
  # change the on-disk JSON at all.
  expect_identical(
    as.character(serialise_col_filters(spec)),
    "{\"active_concept\":{\"values\":[\"0\",\"1\"],\"defaults\":[\"1\"]}}"
  )
})

test_that("serialise rejects a value_labels name not in values", {
  expect_error(
    serialise_col_filters(list(
      col = list(
        values = c("a", "b"),
        defaults = c("a"),
        value_labels = c("a" = "Ay", "c" = "See")
      )
    )),
    "value_labels.*subset|subset.*values"
  )
})

test_that("serialise rejects a non-string description", {
  expect_error(
    serialise_col_filters(list(
      col = list(
        values = c("a", "b"),
        defaults = c("a"),
        description = c("too", "long")
      )
    )),
    "description.*single string"
  )
})

# === lookup_metadata with col_filters =======================================

test_that("lookup_metadata() includes serialised col_filters", {
  meta <- lookup_metadata(
    "test_type",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )

  expect_true("col_filters" %in% names(meta))
  expect_type(meta$col_filters, "character")

  # Should round-trip back
  cf <- deserialise_col_filters(meta$col_filters)
  expect_identical(cf$status$defaults, "1")
})

test_that("lookup_metadata() with col_filters = NULL gives NA", {
  meta <- lookup_metadata("test_type")
  expect_identical(meta$col_filters, NA_character_)
})

# === Write-time validation ==================================================

test_that("add_lookup_table errors when col_filters references non-existent column", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("letter A", "letter B")
  )

  meta <- lookup_metadata(
    "test",
    col_filters = list(
      nonexistent_col = list(values = c("a"), defaults = c("a"))
    )
  )

  expect_error(
    suppressMessages(add_lookup_table(table, meta)),
    "nonexistent_col"
  )
})

# === CODES with col_filters =================================================

test_that("CODES with col_filters = 'default' applies metadata defaults", {
  local_build_temp_database()

  # Create a lookup table with a status column
  table <- data.frame(
    code = c("A1", "A2", "B1", "B2"),
    description = c("Active 1", "Active 2", "Inactive 1", "Inactive 2"),
    status = c("1", "1", "0", "0")
  )

  meta <- lookup_metadata(
    "filtered_type",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )

  suppressMessages(add_lookup_table(table, meta))

  # Default should only return active codes
  result <- CODES("all", type = "filtered_type")
  expect_equal(nrow(result), 2)
  expect_true(all(result$code %in% c("A1", "A2")))
})

test_that("CODES with col_filters = NULL returns all rows", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A1", "A2", "B1", "B2"),
    description = c("Active 1", "Active 2", "Inactive 1", "Inactive 2"),
    status = c("1", "1", "0", "0")
  )

  meta <- lookup_metadata(
    "filtered_type",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )

  suppressMessages(add_lookup_table(table, meta))

  # NULL should return all rows
  result <- CODES("all", type = "filtered_type", col_filters = NULL)
  expect_equal(nrow(result), 4)
})

test_that("CODES with custom col_filters applies custom filter", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A1", "A2", "B1", "B2"),
    description = c("Active 1", "Active 2", "Inactive 1", "Inactive 2"),
    status = c("1", "1", "0", "0")
  )

  meta <- lookup_metadata(
    "filtered_type",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )

  suppressMessages(add_lookup_table(table, meta))

  # Custom filter should return only inactive codes
  result <- CODES(
    "all",
    type = "filtered_type",
    col_filters = list(lookup = list("filtered_type" = list(status = c("0"))))
  )
  expect_equal(nrow(result), 2)
  expect_true(all(result$code %in% c("B1", "B2")))
})

test_that("legacy flat col_filters and malformed shapes abort", {
  local_build_temp_database()

  table <- data.frame(code = "A", description = "a", status = "1")
  suppressMessages(add_lookup_table(table, lookup_metadata("shape_test")))

  # The pre-#170 flat form
  expect_error(
    CODES("all", type = "shape_test", col_filters = list(status = "1")),
    class = "codeminer_col_filters_invalid"
  )
  # Not a list at all
  expect_error(
    CODES("all", type = "shape_test", col_filters = "everything"),
    class = "codeminer_col_filters_invalid"
  )
  # Full specification (values + defaults) instead of applied values
  expect_error(
    CODES(
      "all",
      type = "shape_test",
      col_filters = get_col_filters(defaults_only = FALSE)
    ),
    class = "codeminer_col_filters_invalid"
  )
})

test_that("CODES with no col_filters in metadata returns all rows for 'default'", {
  local_build_temp_database()

  table <- data.frame(
    code = c("X", "Y", "Z"),
    description = c("X desc", "Y desc", "Z desc")
  )

  meta <- lookup_metadata("no_filter_type")
  suppressMessages(add_lookup_table(table, meta))

  result <- CODES("all", type = "no_filter_type")
  expect_equal(nrow(result), 3)
})

# === MAP with col_filters ===================================================

test_that("MAP col_filters applies to mapping table", {
  local_build_temp_database()

  # Target lookup
  target <- data.frame(
    code = c("T1", "T2", "T3"),
    description = c("Target 1", "Target 2", "Target 3")
  )
  suppressMessages(
    add_lookup_table(target, lookup_metadata("target_type"))
  )

  # Source lookup
  source <- data.frame(
    code = c("S1", "S2", "S3"),
    description = c("Source 1", "Source 2", "Source 3")
  )
  suppressMessages(
    add_lookup_table(source, lookup_metadata("source_type"))
  )

  # Mapping table with a quality column
  mapping <- data.frame(
    from = c("S1", "S2", "S3"),
    to = c("T1", "T2", "T3"),
    quality = c("exact", "approximate", "exact")
  )

  map_meta <- mapping_metadata(
    "source_type",
    "target_type",
    col_filters = list(
      quality = list(
        values = c("exact", "approximate"),
        defaults = c("exact")
      )
    )
  )
  suppressMessages(add_mapping_table(mapping, map_meta))

  # Default: only exact mappings (S2 warning expected — approximate only)
  expect_warning(
    result <- MAP("S1", "S2", "S3", from = "source_type", to = "target_type"),
    "not found"
  )
  expect_true(all(result$code %in% c("T1", "T3")))

  # Override: all mappings
  result_all <- MAP(
    "S1",
    "S2",
    "S3",
    from = "source_type",
    to = "target_type",
    col_filters = NULL
  )
  expect_true(all(c("T1", "T2", "T3") %in% result_all$code))
})

# === update_*_metadata ======================================================

test_that("update_lookup_metadata updates col_filters", {
  local_build_temp_database()

  # Add table without col_filters
  table <- data.frame(
    code = c("A1", "B1"),
    description = c("Active", "Inactive"),
    status = c("1", "0")
  )
  suppressMessages(
    add_lookup_table(table, lookup_metadata("update_test"))
  )

  # Before update: all rows returned
  result_before <- CODES("all", type = "update_test")
  expect_equal(nrow(result_before), 2)

  # Update metadata to add col_filters
  suppressMessages(
    update_lookup_metadata(
      "update_test",
      col_filters = list(
        status = list(values = c("0", "1"), defaults = c("1"))
      )
    )
  )

  # After update: only active rows returned by default
  result_after <- CODES("all", type = "update_test")
  expect_equal(nrow(result_after), 1)
  expect_equal(result_after$code, "A1")
})

# === Session pinning ========================================================

test_that("codeminer_set_col_filters overrides metadata defaults", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A1", "B1"),
    description = c("Active", "Inactive"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "pin_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # Default: only active
  result <- CODES("all", type = "pin_test")
  expect_equal(nrow(result), 1)

  # Pin: include inactive too
  codeminer_set_col_filters(
    lookup = list("pin_test" = list(status = c("0", "1")))
  )

  result_pinned <- CODES("all", type = "pin_test")
  expect_equal(nrow(result_pinned), 2)

  # Clear: back to metadata defaults
  codeminer_clear_col_filters()
  result_cleared <- CODES("all", type = "pin_test")
  expect_equal(nrow(result_cleared), 1)
})

test_that("codeminer_set_col_filters validates input", {
  # Must provide at least one argument
  expect_error(
    codeminer_set_col_filters(),
    "At least one"
  )

  # Must be named list
  expect_error(
    codeminer_set_col_filters(lookup = "not a list"),
    "named list"
  )

  # Filter values must be atomic (numeric values are coerced, lists abort)
  expect_error(
    codeminer_set_col_filters(
      lookup = list("test" = list(col = list("a")))
    ),
    "atomic"
  )

  # The whole object and the per-type args are mutually exclusive
  expect_error(
    codeminer_set_col_filters(
      col_filters = list(lookup = list("test" = list(col = "a"))),
      lookup = list("test" = list(col = "a"))
    ),
    "not both"
  )

  # A whole object passed as a per-type argument gets a pointed hint
  expect_error(
    codeminer_set_col_filters(
      lookup = list(lookup = list("test" = list(col = "a")))
    ),
    "col_filters = x"
  )
})

# === with_col_filters =======================================================

test_that("with_col_filters temporarily overrides filters", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A1", "B1"),
    description = c("Active", "Inactive"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "with_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # Default: only active
  expect_equal(nrow(CODES("all", type = "with_test")), 1)

  # Temporary override: include all (filters first, code last)
  result <- with_col_filters(
    list(lookup = list("with_test" = list(status = c("0", "1")))),
    CODES("all", type = "with_test")
  )
  expect_equal(nrow(result), 2)

  # NA disables filtering for the scope
  result_na <- with_col_filters(NA, CODES("all", type = "with_test"))
  expect_equal(nrow(result_na), 2)

  # After block: back to default
  expect_equal(nrow(CODES("all", type = "with_test")), 1)

  # The old code-first / per-type-args signature is gone
  expect_error(
    with_col_filters(
      CODES("all", type = "with_test"),
      lookup = list("with_test" = list(status = c("0", "1")))
    ),
    "unused argument"
  )
})

test_that("with_col_filters restores state even on error", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A1"),
    description = c("Test"),
    status = c("1")
  )
  meta <- lookup_metadata(
    "error_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  expect_error(
    with_col_filters(
      list(lookup = list("error_test" = list(status = c("0", "1")))),
      stop("intentional error")
    ),
    "intentional error"
  )

  # Filters should be restored (not stuck on the temporary override)
  expect_null(.codeminer_env$active_col_filters$lookup)
})

# === DESCRIPTION bug regression =============================================

test_that("DESCRIPTION does not match inactive descriptions", {
  local_build_temp_database()

  # Create a lookup with active/inactive descriptions for different codes.
  # Code "C100" has an active description "Diabetes mellitus".
  # Code "C200" has an inactive description "Diabetes insipidus" (status = "0")
  # and an active description "Kidney disease" (status = "1").
  table <- data.frame(
    code = c("C100", "C200", "C200"),
    description = c(
      "Diabetes mellitus",
      "Diabetes insipidus",
      "Kidney disease"
    ),
    status = c("1", "0", "1")
  )
  meta <- lookup_metadata(
    "desc_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # Search for "Diabetes" — should only find C100, not C200
  # (C200's only "Diabetes" description is inactive)
  result <- DESCRIPTION("Diabetes", type = "desc_test")
  expect_equal(nrow(result), 1)
  expect_equal(result$code, "C100")
})

# === Relationship table col_filters =========================================

test_that("CHILDREN applies col_filters to relationship table", {
  local_build_temp_database()

  # Lookup table
  lookup <- data.frame(
    code = c("P", "C1", "C2"),
    description = c("Parent", "Child 1", "Child 2")
  )
  suppressMessages(
    add_lookup_table(lookup, lookup_metadata("rel_filter_test"))
  )

  # Relationship table with active/inactive flag
  rel <- data.frame(
    from = c("C1", "C2"),
    to = c("P", "P"),
    type = c("is a", "is a"),
    active = c("1", "0")
  )
  rel_meta <- relationship_metadata(
    "rel_filter_test",
    col_filters = list(
      active = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_relationship_table(rel, rel_meta))

  # Default: only C1 (active relationship) should be a child of P
  result <- CHILDREN("P", type = "rel_filter_test")
  expect_true("C1" %in% result$code)
  expect_false("C2" %in% result$code)

  # Override: include inactive relationships
  result_all <- CHILDREN(
    "P",
    type = "rel_filter_test",
    col_filters = NULL
  )
  expect_true(all(c("C1", "C2") %in% result_all$code))
})

# === get_col_filters ========================================================

test_that("get_col_filters returns full spec when defaults_only = FALSE", {
  local_build_temp_database()

  table <- data.frame(
    code = c("X"),
    description = c("Test"),
    status = c("1")
  )
  meta <- lookup_metadata(
    "gcf_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  full <- get_col_filters(defaults_only = FALSE)
  expect_true("lookup" %in% names(full))
  expect_true("gcf_test" %in% names(full$lookup))
  expect_identical(full$lookup$gcf_test$status$values, c("0", "1"))
  expect_identical(full$lookup$gcf_test$status$defaults, c("1"))
})

test_that("get_col_filters(defaults_only = FALSE) surfaces description and value_labels", {
  local_build_temp_database()

  table <- data.frame(
    code = c("X"),
    description = c("Test"),
    status = c("1")
  )
  meta <- lookup_metadata(
    "gcf_doc_test",
    col_filters = list(
      status = list(
        values = c("0", "1"),
        defaults = c("1"),
        description = "Whether the code is active.",
        value_labels = c("1" = "Active", "0" = "Inactive")
      )
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  spec <- get_col_filters(defaults_only = FALSE)$lookup$gcf_doc_test$status
  expect_identical(spec$description, "Whether the code is active.")
  expect_identical(spec$value_labels, c("1" = "Active", "0" = "Inactive"))
})

test_that("get_col_filters returns defaults only when defaults_only = TRUE", {
  local_build_temp_database()

  table <- data.frame(
    code = c("X"),
    description = c("Test"),
    status = c("1")
  )
  meta <- lookup_metadata(
    "gcf_default_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = c("1"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  defaults <- get_col_filters(defaults_only = TRUE)
  expect_true("lookup" %in% names(defaults))
  expect_identical(defaults$lookup$gcf_default_test$status, c("1"))
})

test_that("get_col_filters(defaults_only = TRUE) round-trips reproducing the default", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B", "C"),
    description = c("a", "b", "c"),
    status = c("1", "1", "0"),
    module = c("m1", "m2", "m1")
  )
  meta <- lookup_metadata(
    "rt_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = "1"),
      # All values listed as defaults -> included in full by default.
      module = list(values = c("m1", "m2"), defaults = c("m1", "m2"))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # Each column's default is its applied value set (the full set for `module`).
  d <- get_col_filters(defaults_only = TRUE)$lookup$rt_test
  expect_identical(d$status, "1")
  expect_identical(d$module, c("m1", "m2"))

  # Round-trip: passing the applied defaults back reproduces the default query.
  n_default <- nrow(dplyr::collect(suppressMessages(get_lookup_table(
    "rt_test"
  ))))
  n_passback <- nrow(dplyr::collect(suppressMessages(get_lookup_table(
    "rt_test",
    col_filters = get_col_filters(TRUE)
  ))))
  expect_identical(n_default, n_passback)

  # Narrowing a column (plain assignment) filters further.
  cf <- get_col_filters(TRUE)
  cf$lookup$rt_test$module <- "m1"
  n_m1 <- nrow(dplyr::collect(suppressMessages(get_lookup_table(
    "rt_test",
    col_filters = cf
  ))))
  expect_true(n_m1 < n_default)
})

test_that("an empty default is an empty allow-set (excludes everything)", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    flag = c("x", "y")
  )
  meta <- lookup_metadata(
    "empty_default_test",
    col_filters = list(
      # Empty default -> empty allow-set: matches no rows by default.
      flag = list(values = c("x", "y"), defaults = character(0))
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # `[]` means "exclude everything" for a default just as for an explicit
  # selection — a column is left unfiltered only by NA / omission.
  n_default <- nrow(dplyr::collect(suppressMessages(get_lookup_table(
    "empty_default_test"
  ))))
  expect_identical(n_default, 0L)

  # The empty default is surfaced as-is and round-trips (still 0 rows).
  expect_identical(
    get_col_filters(TRUE)$lookup$empty_default_test$flag,
    character(0)
  )
})

test_that("get_col_filters returns empty list when no col_filters set", {
  local_build_temp_database()

  table <- data.frame(
    code = c("X"),
    description = c("Test")
  )
  suppressMessages(
    add_lookup_table(table, lookup_metadata("no_cf_test"))
  )

  result <- get_col_filters()
  # Should not have an entry for "no_cf_test" since it has no col_filters
  expect_true(
    is.null(result$lookup$no_cf_test) ||
      length(result$lookup$no_cf_test) == 0
  )
})

# === Table-keyed col_filters argument (issue #170) ==========================

test_that("MAP col_filters reaches the target lookup (issue #170)", {
  local_build_temp_database()

  # Target lookup with a filterable status column, unrestricted by default
  # (its defaults list all values).
  target <- data.frame(
    code = c("T1", "T2", "T3"),
    description = c("Target 1", "Target 2", "Target 3"),
    status = c("1", "0", "1")
  )
  suppressMessages(add_lookup_table(
    target,
    lookup_metadata(
      "map_target",
      col_filters = list(
        status = list(values = c("0", "1"), defaults = c("0", "1"))
      )
    )
  ))
  source <- data.frame(
    code = c("S1", "S2", "S3"),
    description = c("Source 1", "Source 2", "Source 3")
  )
  suppressMessages(add_lookup_table(source, lookup_metadata("map_source")))
  mapping <- data.frame(
    from = c("S1", "S2", "S3"),
    to = c("T1", "T2", "T3"),
    quality = c("good", "good", "poor")
  )
  suppressMessages(add_mapping_table(
    mapping,
    mapping_metadata("map_source", "map_target")
  ))

  # Unfiltered: all three targets come back
  result_all <- suppressMessages(
    MAP("S1", "S2", "S3", from = "map_source", to = "map_target")
  )
  expect_setequal(result_all$code, c("T1", "T2", "T3"))

  # Filter the TARGET LOOKUP via the col_filters argument (previously
  # impossible: the argument only reached the mapping table). T2 is dropped
  # by the lookup filter, so its code goes missing with a warning.
  expect_warning(
    result <- suppressMessages(MAP(
      "S1",
      "S2",
      "S3",
      from = "map_source",
      to = "map_target",
      col_filters = list(lookup = list("map_target" = list(status = "1")))
    )),
    class = "codeminer_missing_codes"
  )
  expect_setequal(result$code, c("T1", "T3"))

  # Both tables filtered in one call: the mapping filter drops S3 -> T3 and
  # the lookup filter drops T2, leaving T1 only.
  suppressWarnings(
    both <- suppressMessages(MAP(
      "S1",
      "S2",
      "S3",
      from = "map_source",
      to = "map_target",
      col_filters = list(
        mapping = list("map_source > map_target" = list(quality = "good")),
        lookup = list("map_target" = list(status = "1"))
      )
    ))
  )
  expect_identical(both$code, "T1")

  # A session pin on the target lookup reaches it too
  suppressMessages(codeminer_set_col_filters(
    lookup = list("map_target" = list(status = "1"))
  ))
  suppressWarnings(
    pinned <- suppressMessages(
      MAP("S1", "S2", "S3", from = "map_source", to = "map_target")
    )
  )
  expect_setequal(pinned$code, c("T1", "T3"))
  codeminer_clear_col_filters()
})

test_that("a col_filters entry replaces that table's defaults wholesale", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B", "C", "D"),
    description = c("a", "b", "c", "d"),
    status = c("1", "1", "0", "0"),
    region = c("north", "south", "north", "south")
  )
  meta <- lookup_metadata(
    "replace_test",
    col_filters = list(
      status = list(values = c("0", "1"), defaults = "1"),
      region = list(values = c("north", "south"), defaults = "north")
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  # Defaults: status "1" AND region "north" -> A only
  expect_identical(
    suppressMessages(CODES("all", type = "replace_test"))$code,
    "A"
  )

  # An entry naming only region REPLACES the whole set: the status default
  # is dropped, not merged
  result <- suppressMessages(CODES(
    "all",
    type = "replace_test",
    col_filters = list(lookup = list("replace_test" = list(region = "south")))
  ))
  expect_setequal(result$code, c("B", "D"))

  # Key-level NA un-filters the table
  result_na <- suppressMessages(CODES(
    "all",
    type = "replace_test",
    col_filters = list(lookup = list("replace_test" = NA))
  ))
  expect_setequal(result_na$code, c("A", "B", "C", "D"))

  # Round-trip: amend get_col_filters() output to change one column while
  # keeping the other defaults
  cf <- get_col_filters()
  cf$lookup[["replace_test"]]$region <- c("north", "south")
  result_rt <- suppressMessages(CODES(
    "all",
    type = "replace_test",
    col_filters = cf
  ))
  expect_setequal(result_rt$code, c("A", "B"))

  # An empty selection matches no rows
  result_none <- suppressMessages(CODES(
    "all",
    type = "replace_test",
    col_filters = list(
      lookup = list("replace_test" = list(status = character(0)))
    )
  ))
  expect_equal(nrow(result_none), 0)
})

test_that("col_filters argument overrides a session pin for that call only", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "arg_pin_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  suppressMessages(codeminer_set_col_filters(
    lookup = list("arg_pin_test" = list(status = c("0", "1")))
  ))
  expect_equal(nrow(suppressMessages(CODES("all", type = "arg_pin_test"))), 2)

  # The argument replaces the pin for this call
  result <- suppressMessages(CODES(
    "all",
    type = "arg_pin_test",
    col_filters = list(lookup = list("arg_pin_test" = list(status = "0")))
  ))
  expect_identical(result$code, "B")

  # The pin is untouched afterwards
  expect_equal(nrow(suppressMessages(CODES("all", type = "arg_pin_test"))), 2)
  codeminer_clear_col_filters()
})

test_that("col_filters round-trips through JSON", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "json_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  # auto_unbox = TRUE turns single values into JSON scalars; they still work
  cf <- list(lookup = list(json_test = list(status = "0")))
  cf_json <- jsonlite::fromJSON(
    jsonlite::toJSON(cf, auto_unbox = TRUE),
    simplifyVector = TRUE
  )
  result <- suppressMessages(CODES(
    "all",
    type = "json_test",
    col_filters = cf_json
  ))
  expect_identical(result$code, "B")

  # A JSON null at key level un-filters the table (parses to a
  # present-but-NULL element, treated like NA)
  null_json <- jsonlite::fromJSON(
    '{"lookup": {"json_test": null}}',
    simplifyVector = TRUE
  )
  result_null <- suppressMessages(CODES(
    "all",
    type = "json_test",
    col_filters = null_json
  ))
  expect_equal(nrow(result_null), 2)
})

test_that("mapping col_filters apply across a reverse swap, keyed either way", {
  local_build_temp_database()

  a <- data.frame(code = c("A1", "A2"), description = c("a1", "a2"))
  b <- data.frame(code = c("B1", "B2"), description = c("b1", "b2"))
  suppressMessages(add_lookup_table(a, lookup_metadata("type_a")))
  suppressMessages(add_lookup_table(b, lookup_metadata("type_b")))
  mapping <- data.frame(
    from = c("A1", "A2"),
    to = c("B1", "B2"),
    assured = c("Y", "N")
  )
  suppressMessages(add_mapping_table(
    mapping,
    mapping_metadata("type_a", "type_b")
  ))

  # Only "type_a > type_b" is registered; calling the reverse triggers the
  # swap warning. A filter keyed by the REQUESTED direction applies...
  w1 <- capture_warnings(
    rev1 <- suppressMessages(MAP(
      "all",
      from = "type_b",
      to = "type_a",
      col_filters = list(
        mapping = list("type_b > type_a" = list(assured = "Y"))
      )
    ))
  )
  expect_true(any(grepl("reverse", w1)))
  expect_false(any(grepl("do not match", w1)))
  expect_identical(rev1$from, "B1")

  # ...and so does one keyed by the REGISTERED direction
  w2 <- capture_warnings(
    rev2 <- suppressMessages(MAP(
      "all",
      from = "type_b",
      to = "type_a",
      col_filters = list(
        mapping = list("type_a > type_b" = list(assured = "Y"))
      )
    ))
  )
  expect_true(any(grepl("reverse", w2)))
  expect_false(any(grepl("do not match", w2)))
  expect_identical(rev2$from, "B1")
})

# === Typo guard =============================================================

test_that("col_filters typo guard warns on unknown keys, columns, and values", {
  local_build_temp_database()

  table <- data.frame(code = c("A"), description = c("a"), status = c("1"))
  meta <- lookup_metadata(
    "guard_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  # Unknown key (matches no registered table)
  expect_warning(
    suppressMessages(CODES(
      "all",
      type = "guard_test",
      col_filters = list(lookup = list("gaurd_test" = list(status = "1")))
    )),
    class = "codeminer_col_filters_unmatched"
  )

  # Unknown column on a known key
  expect_warning(
    suppressMessages(CODES(
      "all",
      type = "guard_test",
      col_filters = list(lookup = list("guard_test" = list(staus = "1")))
    )),
    class = "codeminer_col_filters_unmatched"
  )

  # Value outside the column's registered values
  expect_warning(
    suppressMessages(CODES(
      "all",
      type = "guard_test",
      col_filters = list(lookup = list("guard_test" = list(status = "yes")))
    )),
    class = "codeminer_col_filters_unmatched"
  )

  # A real column with no registered spec accepts any value silently
  expect_no_warning(
    suppressMessages(CODES(
      "all",
      type = "guard_test",
      col_filters = list(lookup = list("guard_test" = list(description = "a")))
    ))
  )

  # An entry for a registered table the query does not use is silent
  suppressMessages(add_lookup_table(
    data.frame(code = "B", description = "b"),
    lookup_metadata("other_test")
  ))
  expect_no_warning(
    suppressMessages(CODES(
      "all",
      type = "other_test",
      col_filters = list(lookup = list("guard_test" = list(status = "1")))
    ))
  )
})

test_that("col_filters validation is silent when no database is connected", {
  # No local_build_temp_database(): fresh state, no metadata cache
  if (
    exists("con", envir = .codeminer_env) &&
      DBI::dbIsValid(.codeminer_env$con)
  ) {
    codeminer_disconnect()
  }
  expect_no_warning(
    codeminer_set_col_filters(
      lookup = list("nowhere" = list(anything = "1"))
    )
  )
  codeminer_clear_col_filters()
})

# === Setter ergonomics ======================================================

test_that("codeminer_set_col_filters accepts a whole object and NA", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "setter_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  # Amend-then-pin round trip with the classed object
  cf <- get_col_filters()
  cf$lookup[["setter_test"]]$status <- c("0", "1")
  suppressMessages(codeminer_set_col_filters(col_filters = cf))
  expect_equal(nrow(suppressMessages(CODES("all", type = "setter_test"))), 2)

  # NA disables all filtering for the session; clear restores defaults
  codeminer_set_col_filters(NA)
  expect_equal(nrow(suppressMessages(CODES("all", type = "setter_test"))), 2)
  codeminer_clear_col_filters()
  expect_equal(nrow(suppressMessages(CODES("all", type = "setter_test"))), 1)

  # Setting pins replaces a session-wide NA
  codeminer_set_col_filters(NA)
  suppressMessages(codeminer_set_col_filters(
    lookup = list("setter_test" = list(status = "0"))
  ))
  expect_identical(
    suppressMessages(CODES("all", type = "setter_test"))$code,
    "B"
  )
  codeminer_clear_col_filters()

  # Numeric values are coerced to character
  suppressMessages(codeminer_set_col_filters(
    lookup = list("setter_test" = list(status = 1))
  ))
  expect_identical(
    suppressMessages(CODES("all", type = "setter_test"))$code,
    "A"
  )
  codeminer_clear_col_filters()
})

test_that("setter informs when a pin drops registered defaults", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0"),
    region = c("north", "south")
  )
  meta <- lookup_metadata(
    "inform_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  # Pin names a different column: the status default is dropped -> inform
  expect_message(
    codeminer_set_col_filters(
      lookup = list("inform_test" = list(region = "south"))
    ),
    "replaces its default filter"
  )
  codeminer_clear_col_filters()

  # Pin restates the default column: silent
  expect_no_message(
    codeminer_set_col_filters(
      lookup = list("inform_test" = list(status = "0"))
    )
  )
  codeminer_clear_col_filters()

  # Un-filtering via NA is explicit intent: silent
  expect_no_message(
    codeminer_set_col_filters(lookup = list("inform_test" = NA))
  )
  codeminer_clear_col_filters()

  # Scoped filters never inform
  expect_no_message(
    with_col_filters(
      list(lookup = list("inform_test" = list(region = "south"))),
      NULL
    )
  )
})

# === Class and print ========================================================

test_that("get_col_filters returns a classed object with a print method", {
  local_build_temp_database()

  table <- data.frame(code = "A", description = "a", status = "1")
  meta <- lookup_metadata(
    "class_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  cf <- get_col_filters()
  expect_s3_class(cf, "codeminer_col_filters")
  expect_output(print(cf), "codeminer_col_filters")
  expect_output(print(cf), "status: 1")

  # Plain assignment keeps the class
  cf$lookup[["class_test"]]$status <- c("0", "1")
  expect_s3_class(cf, "codeminer_col_filters")

  # Empty object
  expect_output(
    print(new_col_filters(list())),
    "No column filters registered"
  )

  # Full specification prints values and defaults
  full <- get_col_filters(defaults_only = FALSE)
  expect_s3_class(full, "codeminer_col_filters")
  expect_output(print(full), "values")
})

test_that("print renders description and value_labels in full-spec mode", {
  local_build_temp_database()

  table <- data.frame(code = "A", description = "a", status = "1")
  meta <- lookup_metadata(
    "print_doc_test",
    col_filters = list(
      status = list(
        values = c("0", "1"),
        defaults = "1",
        description = "Whether the code is active.",
        value_labels = c("1" = "Active", "0" = "Inactive")
      )
    )
  )
  suppressMessages(add_lookup_table(table, meta))

  full <- get_col_filters(defaults_only = FALSE)
  expect_output(print(full), "description: Whether the code is active\\.")
  expect_output(print(full), "value_labels:")
  expect_output(print(full), "1 = Active")
})

# === Missing-codes hint =====================================================

test_that("missing-codes warning mentions active column filters", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "hint_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  # B is excluded by the default filter: the miss warning carries the hint
  expect_warning(
    suppressMessages(CODES("B", type = "hint_test")),
    "Active column filters"
  )

  # With filtering off, the hint is absent
  w <- capture_warnings(
    suppressMessages(CODES("Z", type = "hint_test", col_filters = NULL))
  )
  expect_true(length(w) > 0)
  expect_false(any(grepl("Active column filters", w)))
})

# === Getters accept the keyed argument ======================================

test_that("table getters accept the table-keyed col_filters argument", {
  local_build_temp_database()

  table <- data.frame(
    code = c("A", "B"),
    description = c("a", "b"),
    status = c("1", "0")
  )
  meta <- lookup_metadata(
    "getter_test",
    col_filters = list(status = list(values = c("0", "1"), defaults = "1"))
  )
  suppressMessages(add_lookup_table(table, meta))

  rows <- suppressMessages(
    get_lookup_table(
      "getter_test",
      col_filters = list(lookup = list(getter_test = list(status = "0")))
    ) |>
      dplyr::collect()
  )
  expect_identical(rows$code, "B")

  # Query construction happens inside the getter, so filters stay baked into
  # a lazy tbl collected after the scope has been restored
  lazy <- suppressMessages(with_col_filters(
    NA,
    get_lookup_table("getter_test")
  ))
  expect_equal(nrow(dplyr::collect(lazy)), 2)
})
