# CLAUDE.md

## Project overview

codeminer is an R package for working with clinical coding systems (ICD-10, SNOMED CT, Read codes, etc.). It uses DuckDB as a backend database for storing lookup tables, mapping tables, and relationship tables with associated metadata.

## Style guidelines

- Use UK English spelling throughout (e.g. "serialise", "centralise", "behaviour", "colour", "summarise")
- Follow the existing code style: `snake_case` for functions and variables
- Use `cli` for error handling (`codeminer_abort`, `codeminer_warn`, `codeminer_inform`)
- Use `glue::glue_sql()` for parameterised SQL queries
- Use `dplyr` / `dbplyr` for lazy table operations where possible
- Prefer tidyverse packages already in Imports (`dplyr`, `purrr`, `stringr`, `tidyr`, `tibble`) over base R equivalents where they read more clearly — e.g. `purrr::map()` rather than a `for` loop over rows, `stringr::str_sub()` rather than `substr()`. Don't force tidyverse where base is genuinely simpler (vectorised arithmetic, predicates, etc.)
- Avoid superseded dplyr verbs (e.g. `transmute()`); use the current equivalents (`mutate()` + `select()`)

## Database schema changes

The on-disk codeminer database carries a schema version separate from the
package version. `current_schema_version()` lives in `R/schema.R`; a row in
the `_db_metadata` table records which schema version a given DB is on.
`enforce_schema_gate()` refuses any DB whose stamp doesn't match the
package's current version. There is intentionally **no in-place migration
path** — pre-1.0 we expect users to rebuild via `build_database(overwrite = TRUE)`
when the format changes. See `vignettes/developer-guide.Rmd` for the gate
policy.

When your change touches **how data is stored**, you MUST bump
`current_schema_version()`. Triggers:

- Add/remove a column in `required_*_metadata_columns()` (lookup, mapping,
  relationship, or db)
- Add/remove/rename a metadata table
- Rename, retype, or change the semantics of any stored column
- Change the composition rule for `*_table_name` (e.g. how it is `paste`d)
- Change the on-disk serialisation of any field (e.g. the `col_filters` JSON
  shape)

When your change is **R-side only**, do NOT bump. Examples that do not
warrant a bump:

- Renaming columns at read time inside `get_lookup_table()` (the on-disk
  shape is unchanged)
- Bug fixes that don't alter stored data
- New R functions, refactors, tests, docs
- Adding/changing function arguments that don't change persisted data

If/when we add external users we may reintroduce a migration registry —
see issue #139 for that discussion.

## Testing and checks

- Tests use `testthat` (edition 3)
- Run tests with `devtools::test()`
- Run linter with `lintr::lint_package()`
- Format code with `air format .`
- Generate documentation with `devtools::document()`
- Run `R CMD check` with `devtools::check()`
- Build pkgdown site with `pkgdown::build_site()`
