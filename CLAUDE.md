# CLAUDE.md

## Project overview

codeminer is an R package for working with clinical coding systems (ICD-10, SNOMED CT, Read codes, etc.). It uses DuckDB as a backend database for storing lookup tables, mapping tables, and relationship tables with associated metadata.

## Style guidelines

- Use UK English spelling throughout (e.g. "serialise", "centralise", "behaviour", "colour", "summarise")
- Follow the existing code style: `snake_case` for functions and variables
- Use `cli` for error handling (`codeminer_abort`, `codeminer_warn`, `codeminer_inform`)
- Use `glue::glue_sql()` for parameterised SQL queries
- Use `dplyr` / `dbplyr` for lazy table operations where possible

## Testing and checks

- Tests use `testthat` (edition 3)
- Run tests with `devtools::test()`
- Run linter with `lintr::lint_package()`
- Format code with `air format .`
- Generate documentation with `devtools::document()`
- Run `R CMD check` with `devtools::check()`
- Build pkgdown site with `pkgdown::build_site()`
