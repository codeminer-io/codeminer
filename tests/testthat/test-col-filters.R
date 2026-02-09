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
    col_filters = list(status = c("0"))
  )
  expect_equal(nrow(result), 2)
  expect_true(all(result$code %in% c("B1", "B2")))
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
    "source_type", "target_type",
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
    "S1", "S2", "S3",
    from = "source_type",
    to = "target_type",
    col_filters = NULL
  )
  expect_true(all(c("T1", "T2", "T3") %in% result_all$code))
})

# === Schema migration =======================================================

test_that("build_database migrates schema for existing databases", {
  local_build_temp_database()

  # Manually drop the col_filters column to simulate an old database
  con <- connect_to_db(read_only = FALSE)
  for (tbl in c(
    codeminer_metadata_table_names$lookup,
    codeminer_metadata_table_names$mapping,
    codeminer_metadata_table_names$relationship
  )) {
    DBI::dbExecute(con, paste0("ALTER TABLE ", tbl, " DROP COLUMN col_filters"))
  }

  # Rebuild (should add the column back via migration)
  build_database(overwrite = FALSE)

  # Verify col_filters column exists again
  con2 <- connect_to_db(read_only = FALSE)
  for (tbl in c(
    codeminer_metadata_table_names$lookup,
    codeminer_metadata_table_names$mapping,
    codeminer_metadata_table_names$relationship
  )) {
    fields <- DBI::dbListFields(con2, tbl)
    expect_true("col_filters" %in% fields, info = paste("Missing in", tbl))
  }
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
  # Must provide at least one type
  expect_error(
    codeminer_set_col_filters(),
    "At least one"
  )

  # Must be named list
  expect_error(
    codeminer_set_col_filters(lookup = "not a list"),
    "named list"
  )

  # Filter values must be character
  expect_error(
    codeminer_set_col_filters(
      lookup = list("test" = list(col = 123))
    ),
    "character vector"
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

  # Temporary override: include all
  result <- with_col_filters(
    {
      CODES("all", type = "with_test")
    },
    lookup = list("with_test" = list(status = c("0", "1")))
  )
  expect_equal(nrow(result), 2)

  # After block: back to default
  expect_equal(nrow(CODES("all", type = "with_test")), 1)
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
      {
        stop("intentional error")
      },
      lookup = list("error_test" = list(status = c("0", "1")))
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
    description = c("Diabetes mellitus", "Diabetes insipidus", "Kidney disease"),
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
