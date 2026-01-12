#' Add clinical code tables to database
#'
#' @description Internal helper function that processes a nested list of tables
#'   with metadata and adds them to the database using the appropriate
#'   `add_*_table()` function based on table type.
#'
#' @param table_list A nested list where each element contains table groups, and each
#'   table group contains tables with their metadata. The structure should be:
#'   list(table_name = list(type = list(table = data.frame, metadata = list)))
#'   where type is one of "lookup", "relationship", or "mapping".
#' @param call The execution environment of the calling function. Used for error
#'   reporting. Defaults to the caller environment.
#'
#' @return Nothing. Called for side effects (adding tables to database).
#'
#' @details This function iterates through the nested list structure and
#'   dispatches to the appropriate add_*_table() function:
#'   * lookup -> add_lookup_table()
#'   * relationship -> add_relationship_table()
#'   * mapping -> add_mapping_table()
#'
#'   If an unrecognized table type is encountered, an error is thrown.
#'
#' @noRd
add_tables_to_database <- function(table_list, call = rlang::caller_env()) {
  cli::cli_inform("Adding tables to database")

  table_list |>
    purrr::iwalk(function(table_group, table_name) {
      table_group |>
        purrr::iwalk(function(table_obj, table_type) {
          switch(
            table_type,
            lookup = add_lookup_table(
              table = table_obj$table,
              metadata = table_obj$metadata
            ),
            relationship = add_relationship_table(
              table = table_obj$table,
              metadata = table_obj$metadata
            ),
            mapping = add_mapping_table(
              table = table_obj$table,
              metadata = table_obj$metadata
            ),
            cli::cli_abort(
              sprintf(
                "Invalid table type '%s' for argument '%s'",
                table_type,
                table_name
              ),
              call = call
            )
          )
        })
    })

  invisible()
}
