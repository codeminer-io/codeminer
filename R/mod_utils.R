# PRIVATE -----------------------------------------------------------------

## Utils -------------------------------------------------------------------

#' Execute a query statement
#'
#' To be used in codelistBuilder app
#'
#' @param query A call object.
#' @param code_type Character. Code type e.g. "icd10"
#' @param qb List. Raw query from jqbr query builder
#' @param query_options Reactive.
#' @param saved_queries Reactive.
#' @param dag_igraph Reactive.
#'
#' @return A list
#' @noRd
run_query <- function(query,
                      code_type,
                      qb,
                      query_options,
                      saved_queries,
                      dag_igraph) {
  # query should only use codeminer exports
  validate_codemapper_calls(query)

  if (is.null(qb)) {
    qb <- translate_codeminer_query_to_qbr_list(query)
  }

  execute_query <-
    purrr::safely(\(x) withr::with_options(
      eval(query_options()),
      eval(query, envir = saved_queries()$results)
    ))

  x <- list(
    query = query,
    result = execute_query(),
    qb = qb,
    code_type = code_type
  )

  if (!rlang::is_empty(x$result$error) ||
      nrow(x$result$result) == 0) {
    # error, or empty result (no codes matching search criteria)
    x <- x$result
  } else {
    # success
    x$result <- x$result$result

    # get saved query dependencies (if any)
    dependencies <- get_qbr_saved_queries(x$qb) %>%
      unique()

    # append dependencies
    if ((length(dependencies) > 0)) {
      if (nrow(saved_queries()$dag$edges) > 0) {
        dependencies <- dependencies %>%
          purrr::set_names() %>%
          purrr::map(
            ~ find_node_dependencies(
              graph = dag_igraph(),
              node = .x,
              mode = "in",
              node_rm = FALSE
            )
          ) %>%
          purrr::compact() %>%
          purrr::reduce(c, .init = NULL) %>%
          unique() %>%
          c(dependencies)

        dependencies <- saved_queries()$dag$nodes %>%
          dplyr::filter(.data[["id"]] %in% !!dependencies) %>%

          # TODO - remove this, temp fix for previously saved codelist
          # bookmarks
          dplyr::mutate("order" = as.integer(.data[["order"]])) %>%

          dplyr::arrange(.data[["order"]]) %>%
          dplyr::pull(tidyselect::all_of("id"))
      }

      # ordered dependencies
      x$dependencies <- dependencies
    }

    # code to generate query result
    query_code <- list(rlang::call2(.fn = "=",
                                    rlang::sym("RESULT"),
                                    query))

    if (length(dependencies) > 0) {
      if (length(dependencies) == 1) {
        query_code_deps <- dependencies %>%
          purrr::map(
            ~ rlang::call2(
              .fn = "=",
              rlang::sym(.x),
              saved_queries()$results_meta[[.x]]$query
            )
          )

      } else {
        query_code_deps <- dependencies %>%
          purrr::map(~ {
            rlang::call2(.fn = "=",
                         rlang::sym(.x),
                         saved_queries()$results_meta[[.x]]$query)
          })
      }

      # then append final query
      query_code <- c(query_code_deps,
                      query_code)

      # add indicator columns, showing which codes came from which query(/queries)
      for (dep in dependencies) {
        print(dep)
        x$result[[dep]] <-
          dplyr::case_when(x$result$code %in% saved_queries()$results[[dep]]$code ~ "1",
                           TRUE ~ ".")
      }
    }
    # add indicator column for inactive snomed codes
    if (code_type == "sct") {
      inactive_codes <- CODES(
        x$result$code,
        code_type = "sct",
        preferred_description_only = TRUE,
        standardise_output = TRUE,
        unrecognised_codes = "warning",
        col_filters = list(
          sct_description = list(
            active_concept = '0',
            active_description = '1'
          )
        )
      ) %>%
        .$code %>%
        suppressWarnings()

      x$result <- x$result %>%
        dplyr::mutate("...inactive_sct" = dplyr::case_when(.data[["code"]] %in% inactive_codes ~ "1",
                                                           TRUE ~ "."))
    }

    x$query_code <- query_code
  }

  return(x)
}

validate_codemapper_calls <- function(call_obj) {
  # Get all exported functions from codemapper
  codemapper_exports <- getNamespaceExports("codemapper")

  # Get all function calls
  fn_calls <- get_function_calls(call_obj)

  # Check which calls are not in codemapper exports
  invalid_calls <- setdiff(fn_calls, codemapper_exports)

  # "(" is allowed
  invalid_calls <- invalid_calls[!invalid_calls == "("]

  if (length(invalid_calls) > 0) {
    stop(paste0("Invalid function calls found: `",
         paste(invalid_calls, sep = "", collapse="`, `"),
         "`. These functions are not exported by codeminer"))
  }

  # Return the valid function calls if no error
  invisible(fn_calls)
}

get_function_calls <- function(call_obj) {
  if (!is.call(call_obj)) return(character(0))

  # Get function name from current call, handling namespace qualification
  current_fn <- if (is.name(call_obj[[1]]) || is.call(call_obj[[1]])) {
    # Convert the function part to character, which handles both name and namespace::fn
    deparse1(call_obj[[1]])
  } else {
    character(0)
  }

  # Recursively get function calls from all arguments
  arg_fns <- unlist(lapply(as.list(call_obj[-1]), function(arg) {
    if (is.call(arg)) get_function_calls(arg)
    else character(0)
  }))

  sub("^.*:{2,3}", "", unique(c(current_fn, arg_fns)))
}

#' Determine which saved queries are down or upstream of a given saved query
#'
#' @param graph An igraph object
#' @param node A string
#' @param mode Either "out" (for downstream dependencies) or "in" (for upstream
#'   dependencies)
#' @param node_rm If `TRUE` (default), remove `node` from output
#'
#' @return A character vector of nodes that are upstream of `node`
#' @noRd
find_node_dependencies <- function(graph,
                                   node,
                                   mode = "out",
                                   node_rm = TRUE) {
  stopifnot(rlang::is_string(node))
  stopifnot(igraph::is.dag(graph))
  stopifnot(mode %in% c("out", "in"))

  result <- igraph::all_simple_paths(graph = graph,
                                     from = node,
                                     mode = mode) %>%
    purrr::map( ~ as.list(.x) %>%
                  purrr::imap( ~ .y)) %>%
    purrr::flatten() %>%
    as.character() %>%
    unique()

  if (node_rm) {
    result <- subset(result,!result == node)
  }

  result
}

#' Update `saved_queries`
#'
#' Add to `saved_queries` or update an existing saved query. Steps:
#'
#' - Determine whether a new query is being created, or whether an existing query is being updated.
#' - Save current query
#' - Recalculate DAG and dependency order
#' - If updating a previously saved query, recalculate downstream dependencies.
#'
#' @param query_result Reactive.
#' @param saved_queries Reactive.
#' @param query_options Reactive
#' @param query character
#' @param code_type character
#' @param ignore_existing_query logical. Set to `TRUE` if calling
#'   `update_saved_queries()` in a loop to update all saved queries in order.
#'
#' @return Called for side effects within an observer.
#' @noRd
update_saved_queries <- function(query,
                                 query_result,
                                 code_type,
                                 saved_queries,
                                 query_options,
                                 ignore_existing_query = FALSE) {
  stopifnot(is.reactive(query_result))
  stopifnot(is.reactive(saved_queries))
  stopifnot(is.reactive(query_options))

  if (!ignore_existing_query) {
    # Determine whether query is new or already exists in saved_queries
    existing_query <- query %in% saved_queries()$objects[[code_type]]
  } else {
    existing_query <- FALSE
  }

  # Save current query (results and metadata)

  ## results
  current_query_result <-
    query_result()$result[, 1:3] # save first 3 cols only

  new_saved_queries <- saved_queries()$results

  assign(query, current_query_result, envir = new_saved_queries)

  ## meta
  current_query_meta <- list(
    query = query_result()$query,
    result_summary = nrow(query_result()$result),
    qb = query_result()$qb
  )

  new_saved_queries_meta <- saved_queries()$results_meta

  assign(query, current_query_meta, envir = new_saved_queries_meta)

  ## dependencies
  nodes <- data.frame(id = query,
                      label = query,
                      group = code_type,
                      n = nrow(query_result()$result))
  nodes <- dplyr::bind_rows(saved_queries()$dag$nodes,
                            nodes)

  if (existing_query | ignore_existing_query) {
    nodes <- nodes %>%
      dplyr::distinct(.data[["id"]],
                      .keep_all = TRUE)
  }

  edges <-
    data.frame(from = get_qbr_saved_queries(query_result()$qb))

  if (nrow(edges) > 0) {
    edges$to <- query
    edges <- dplyr::bind_rows(saved_queries()$dag$edges,
                              edges)
  } else {
    edges <- saved_queries()$dag$edges
  }

  if (existing_query | ignore_existing_query) {
    edges <- edges %>%
      dplyr::distinct()
  }

  # determine DAG order - add to `nodes` df
  if (nrow(edges) > 0) {
    dag_igraph <- igraph::graph_from_data_frame(d = edges,
                                                directed = TRUE,
                                                vertices = nodes)

    dep_order <- igraph::topo_sort(graph = dag_igraph,
                                   mode = "out") %>%
      as.list() %>%
      purrr::imap( ~ .y) %>%
      as.character()

    dep_order <- data.frame(id = dep_order)
    dep_order$order <- as.integer(rownames(dep_order))

    nodes <- nodes %>%
      dplyr::select(-tidyselect::any_of("order")) %>%
      dplyr::full_join(dep_order,
                       by = "id")

    # if existing query, update downstream dependencies
    if (existing_query) {
      upstream_deps <- find_node_dependencies(
        graph = dag_igraph,
        node = query,
        mode = "out",
        node_rm = TRUE
      )

      query_options_for_code_type <- eval(query_options())
      query_options_for_code_type$codemapper.code_type <- code_type
      query_options_for_code_type$codemapper.map_to <- code_type

      for (x in upstream_deps) {
        updated_result <-
          withr::with_options(query_options_for_code_type,
                              eval(
                                get(x = x, envir = saved_queries()$results_meta)$query,
                                envir = new_saved_queries
                              ))

        assign(x, updated_result, envir = new_saved_queries)
      }
    }

  } else {
    nodes$order <- as.integer(rownames(nodes))
    edges <- saved_queries()$dag$edges
  }

  new_dependencies <- list(nodes = nodes,
                           edges = edges)

  # finally
  saved_queries(
    list(
      objects = get_objects_from_nodes(nodes),
      results = new_saved_queries,
      results_meta = new_saved_queries_meta,
      dag = new_dependencies
    )
  )
}

#' Remove selected queries from `saved_queries`
#'
#' Updates `saved_queries` (reactive value) and its corresponding check box
#' input.
#'
#' @param updated_dag Reactive. List with items 'new_dependencies',
#'   'nodes_to_remove' and 'dependencies'. Used to determine how `saved_queries`
#'   should be updated.
#' @param saved_queries Reactive.
#'
#' @return Called for side effects within an observer.
#' @noRd
remove_saved_queries <- function(updated_dag,
                                 saved_queries) {
  stopifnot(is.reactive(updated_dag))
  stopifnot(is.reactive(saved_queries))

  # update `saved_queries()`

  ## results
  new_saved_queries <- saved_queries()$results

  rm(list = updated_dag()$nodes_to_remove,
     envir = new_saved_queries)

  ## meta
  new_saved_queries_meta <- saved_queries()$results_meta

  rm(list = updated_dag()$nodes_to_remove,
     envir = new_saved_queries_meta)

  ## finally
  if (isTruthy(updated_dag()$new_dependencies$nodes$group)) {
    new_objects <-
      get_objects_from_nodes(updated_dag()$new_dependencies$nodes)
  } else {
    new_objects <- list("None")
  }

  saved_queries(
    list(
      objects = new_objects,
      results = new_saved_queries,
      results_meta = new_saved_queries_meta,
      dag = updated_dag()$new_dependencies
    )
  )
}

# return vector of saved query names from a qbr input
get_qbr_saved_queries <- function(x) {
  recurse <- function(x) {
    if (is.list(x) &
        identical(names(x),
                  c("id", "field", "type", "input", "operator", "value"))) {

      switch(
        x$id,
        "description" = NULL,
        "children" = NULL,
        "codes" = NULL,
        "saved_query" = x$value,
        "map_children" = NULL,
        "map_codes" = NULL,
        "sct_has_attributes" = x$value,
        "sct_get_attributes" = x$value,
        "sct_attribute_types_to" = x$value,
        "sct_attribute_types_from" = x$value,
        stop("Unrecognised filter!")
      )

    } else if (is.list(x)) {
      purrr::map(x, get_qbr_saved_queries)
    } else {
      NULL
    }
  }

  unlist(recurse(x))
}

# function to convert rules to expressions
convert_rules_to_expr <- function(x) {
  if (is.list(x) &
      identical(names(x),
                c("id", "field", "type", "input", "operator", "value"))) {

    switch(
      x$id,
      "description" = rlang::call2(.fn = "DESCRIPTION",
                                   x$value),
      "saved_query" = as.symbol(x$value),
      "children" = rlang::call2(.fn = "CHILDREN",
                                   x$value),
      "codes" = rlang::call2(.fn = "CODES",
                             x$value),
      "map_codes" = rlang::call2(.fn = "MAP",
                                 x$value,
                                 from = x$operator),
      "map_children" = rlang::call2(
        .fn = "MAP",
        rlang::call2(.fn = "CHILDREN",
                     x$value,
                     code_type = x$operator)
      ),
      "sct_has_attributes" = rlang::call2(.fn = "HAS_ATTRIBUTES",
                                          as.symbol(x$value[[1]]),
                                          relationship = as.symbol(x$value[[2]])),
      "sct_get_attributes" = rlang::call2(.fn = "GET_ATTRIBUTES",
                                          as.symbol(x$value[[1]]),
                                          relationship = as.symbol(x$value[[2]])),
      "sct_attribute_types_to" = rlang::call2(.fn = "ATTRIBUTE_TYPES_TO",
                                          as.symbol(x$value)),
      "sct_attribute_types_from" = rlang::call2(.fn = "ATTRIBUTE_TYPES_FROM",
                                          as.symbol(x$value)),
      stop("Unrecognised filter!")
    )

  } else if (is.list(x)) {
    purrr::map(x, convert_rules_to_expr)
  } else {
    x
  }
}



# function to apply set operations to rule sets
apply_setops <- function(x) {
  if (is.list(x) &
      "rules" %in% names(x)) {
    setop <- switch(
      x$condition,
      AND = "%AND%",
      OR = "%OR%",
      NOT = "%NOT%",
      ... = stop("Invalid operator")
    )

    x$rules %>%
      purrr::reduce( ~ rlang::call2(.fn = setop,
                                    apply_setops(.x),
                                    apply_setops(.y)))
  } else if (is.list(x)) {
    # stop("Invalid input")
    x[[1]]
  } else {
    x
  }
}

# function to process qbr rules
custom_qbr_translation <- function(qbr_rules) {
  # process qb rules
  qbr_rules %>%
    purrr::map(convert_rules_to_expr) %>%
    apply_setops()
}


# function to update code_type operator
update_qb_operator_code_type <- function(x, code_type) {
  if (is.list(x) &
      identical(names(x),
                c("id", "field", "type", "input", "operator", "value"))) {
    if (x$id %in% c("map_codes", "map_children")) {
      x$operator <- NULL
    } else if (x$id %in% c("sct_has_attributes", "sct_get_attributes")) {
      x$operator <- "sct_relationship"
    } else {
      x$operator <- code_type
    }

    x

  } else if (is.list(x)) {
    purrr::map(x, update_qb_operator_code_type, code_type)
  } else {
    x
  }
}

#' Helped for updating saved query reactive value objects
#'
#' @param nodes A data frame
#'
#' @return A named list for use with `choices` argument to `selectizeInput()`
#' @noRd
get_objects_from_nodes <- function(nodes) {
  nodes$group %>%
    unique() %>%
    purrr::set_names() %>%
    purrr::map( ~ nodes %>%
                  dplyr::filter(.data[["group"]] == !!.x) %>%
                  dplyr::pull(.data[["id"]]) %>%
                  as.list())
}

get_available_map_from_code_types <- function(available_maps, to) {
  available_maps %>%
    dplyr::filter(.data[["to"]] == !!to) %>%
    dplyr::pull(.data[["from"]]) %>%
    as.list()
}

#' Update jqbr filter code types
#'
#' @param input_code_type Character
#' @param available_maps Character vector
#' @param available_saved_queries Character vector
#' @param sct_attributes_filters List of filters
#'
#' @return A list
#' @noRd
update_qbr_filters <- function(input_code_type,
                               available_maps,
                               available_saved_queries) {
  new_description_contains_filter <- description_contains_filter
  new_description_contains_filter$operators <-
    list(input_code_type)

  new_codes_filter <- codes_filter
  new_codes_filter$operators <- list(input_code_type)

  new_map_codes_filter <- map_codes_filter
  new_map_codes_filter$operators <-
    get_available_map_from_code_types(available_maps = available_maps,
                                      to = input_code_type)

  new_map_children_filter <- map_children_filter
  new_map_children_filter$operators <-
    get_available_map_from_code_types(available_maps = available_maps,
                                      to = input_code_type)

  new_child_codes_filter <- child_codes_filter
  new_child_codes_filter$operators <- list(input_code_type)

  new_saved_query_filter <- empty_saved_query_filter
  new_saved_query_filter$values <- available_saved_queries
  new_saved_query_filter$operators <- list(input_code_type)

  new_filters <- list(
    new_saved_query_filter,
    new_description_contains_filter,
    new_codes_filter,
    new_child_codes_filter,
    new_map_codes_filter,
    new_map_children_filter
  )

  if (input_code_type == "sct") {
    new_sct_has_attributes_filter <- sct_has_attributes_filter
    new_sct_has_attributes_filter$values <- available_saved_queries

    new_sct_get_attributes_filter <- sct_get_attributes_filter
    new_sct_get_attributes_filter$values <- available_saved_queries

    new_sct_attribute_types_from_filter <- sct_attribute_types_from_filter
    new_sct_attribute_types_from_filter$values <- available_saved_queries

    new_sct_attribute_types_to_filter <- sct_attribute_types_to_filter
    new_sct_attribute_types_to_filter$values <- available_saved_queries

    new_filters <- c(new_filters,
                     list(new_sct_has_attributes_filter,
                          new_sct_get_attributes_filter,
                          new_sct_attribute_types_from_filter,
                          new_sct_attribute_types_to_filter))
  }

  new_filters
}

get_code_type_labels <- function(available_code_types,
                                 direction = "label_id") {

  match.arg(direction,
            c("label_id", "id_label"))

  cols <- switch(
    direction,
    label_id = c("code_label", "code"),
    id_label = c("code", "code_label")
  )

  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["code"]] %in% !!available_code_types) %>%
    dplyr::select(tidyselect::all_of(cols)) %>%
    tibble::deframe() %>%
    as.list()
}

recompute_all_queries <- function(saved_queries,
                                  query_options) {
  if (nrow(saved_queries()$dag$nodes) > 0) {
    # progress bar
    withProgress(message = "Updating saved queries...", {
      step <- 1
      for (x in saved_queries()$dag$nodes$id) {
        params <- saved_queries()$dag$nodes %>%
          dplyr::filter(.data[["id"]] == !!x)

        query <-
          get(x = params$id, envir = saved_queries()$results_meta)$query

        qb <-
          get(x = params$id, envir = saved_queries()$results_meta)$qb

        query_options_for_code_type <- eval(query_options())
        query_options_for_code_type$codemapper.code_type <- params$group
        query_options_for_code_type$codemapper.map_to <- params$group

        execute_query <-
          purrr::safely(\(x) withr::with_options(
            query_options_for_code_type,
            eval(query, envir = saved_queries()$results)
          ))

        query_result <- list(
          query = query,
          result = execute_query()$result,
          qb = qb,
          code_type = params$group
        )

        if (is.null(query_result$result)) {
          stop('Update `col_filters()` feature is not yet fully implemented to handle cases where errors arise when updating saved queries (e.g. no codes returned, or new filters cause `codemapper.unrecognised_codes_lookup = "error")` to raise an error')
        }

        update_saved_queries(
          query = x,
          query_result = reactive(query_result),
          saved_queries = saved_queries,
          code_type = params$group,
          query_options = query_options,
          ignore_existing_query = TRUE
        )

        incProgress(1 / length(step))
        step <- step + 1
      }
    })

    updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

    updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_save_query_input_show")

    updateTabsetPanel(inputId = "tabs_select_code_type",
                      selected = "tab_select_code_type_show")
  }
}

## jqbr filters and operators --------------------------------------------------------------------

### Filters ---------------------------------------------------------
empty_saved_query_filter <- list(
  id = "saved_query",
  label = "Saved query",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("read2"),
  description = "Previously saved queries will be listed here."
)

child_codes_filter <- list(
  id = "children",
  label = "Children",
  type = "string",
  operators = list("read2"),
  description = "Search for children codes. Multiple codes may be supplied separated by '|' e.g. 'E10 | E11' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E11 << T2DM >>'."
)

description_contains_filter <- list(
  id = "description",
  label = "Description",
  type = "string",
  operators = list("read2"),
  description = "Search for codes that match a regular expression (case insensitive). For example, '^a' will search for all codes starting with either 'A' or 'a'."
)

codes_filter <- list(
  id = "codes",
  label = "Codes",
  type = "string",
  operators = list("read2"),
  description = "Search for one or more codes separated by '|' e.g. 'E10 | E101' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E101 << T1DM with coma >>'."
)

map_codes_filter <- list(
  id = "map_codes",
  label = "Map codes",
  type = "string",
  operators = list(),
  description = "Map codes from one coding system to another. Multiple codes may be supplied separated by '|' e.g. 'E101 | E102' for ICD10. Comments may also be included between '<< >>' e.g. 'E101 << T1DM with coma >> | E102 << T1DM with renal complications >>'."
)

map_children_filter <- list(
  id = "map_children",
  label = "Map children",
  type = "string",
  operators = list(),
  description = "Map child codes from one coding system to another. Multiple codes may be supplied separated by '|' e.g. 'E10 | E11' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E11 << T2DM >>'."
)

sct_has_attributes_filter <- list(
  id = "sct_has_attributes",
  label = "Has attributes",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_relationship"),
  description = "Retrieve SNOMED CT codes which posess certain attributes. Accepts saved queries as inputs only: the first saved query should be a set of attributes (e.g. '106544002 << Family Enterobacteriaceae (organism) >>'), while the second should be one or more relationship types (e.g. '246075003 << Causative agent (attribute) >>')."
)

sct_get_attributes_filter <- list(
  id = "sct_get_attributes",
  label = "Get attributes",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_relationship"),
  description = "Retrieve attributes for a set of SNOMED CT codes. Accepts saved queries as inputs only: the first saved query should be a set of attributes (e.g. '106544002 << Family Enterobacteriaceae (organism) >>'), while the second should be one or more relationship types (e.g. '246075003 << Causative agent (attribute) >>')."
)

sct_attribute_types_to_filter <- list(
  id = "sct_attribute_types_to",
  label = "Attribute types to",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_attribute_types"),
  description = "Retrieve attribute types that point to a set of SNOMED CT codes. Accepts saved queries as inputs only."
)

sct_attribute_types_from_filter <- list(
  id = "sct_attribute_types_from",
  label = "Attribute types from",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_attribute_types"),
  description = "Retrieve attribute types that point from a set of SNOMED CT codes. Accepts saved queries as inputs only."
)

# sct_attributes_filter_template <- list(
#   id = "sct_attributes",
#   label = "Attributes 246075003 Causative agent (attribute)",
#   type = "string",
#   operators = list("has"),
#   description = "Retrieve SNOMED CT codes based on attributes."
# )

# TO DELETE

# get_sct_attributes_filter_options <- function(sct_saved_queries = NULL) {
#   sct_attributes_filter_saved_query_options <-
#     stringr::str_glue('<option value=\"{sct_saved_queries}\">{sct_saved_queries}</option>')
#
#   result <- c('<option value=\"-1\">-</option>',
#               sct_attributes_filter_saved_query_options) %>%
#     paste(sep = "", collapse = " ")
#
#   result
# }
#
# sct_attributes_filter <- list(
#     id = "sct_attributes",
#     label = "Attributes",
#     type = "string",
#     operators = list("has"),
#     input = "
#       function(rule, name) {
#       var $container = rule.$el.find('.rule-value-container');
#
#       $container.on('change', '[name='+ name +'_1]', function(){
#         var h = '';
#
#         switch ($(this).val()) {
#           case 'A':
#             h = '<option value=\"-1\">-</option> <option value=\"1\">1</option> <option value=\"2\">2</option>';
#             break;
#           case 'B':
#             h = '<option value=\"-1\">-</option> <option value=\"3\">3</option> <option value=\"4\">4</option>';
#             break;
#           case 'C':
#             h = '<option value=\"-1\">-</option> <option value=\"5\">5</option> <option value=\"6\">6</option>';
#             break;
#         }
#
#         $container.find('[name$=_2]')
#           .html(h).toggle(!!h)
#           .val('-1').trigger('change');
#       });
#
#       return '\\
#       <select name=\"'+ name +'_1\"> \\
#         <option value=\"-1\">-</option> \\
#         <option value=\"A\">A</option> \\
#         <option value=\"B\">B</option> \\
#         <option value=\"C\">C</option> \\
#       </select> \\
#       <select name=\"'+ name +'_2\" style=\"display:none;\"></select>';
#     }
#                               ",
#     valueGetter = "function(rule) {
#       return rule.$el.find('.rule-value-container [name$=_1]').val()
#         +'.'+ rule.$el.find('.rule-value-container [name$=_2]').val();
#     }",
#     valueSetter = "function(rule, value) {
#       if (rule.operator.nb_inputs > 0) {
#         var val = value.split('.');
#
#         rule.$el.find('.rule-value-container [name$=_1]').val(val[0]).trigger('change');
#         rule.$el.find('.rule-value-container [name$=_2]').val(val[1]).trigger('change');
#       }
#     }"
#   )

filters <- list(
  description_contains_filter,
  codes_filter,
  map_codes_filter,
  map_children_filter,
  child_codes_filter,
  empty_saved_query_filter,
  sct_has_attributes_filter,
  sct_get_attributes_filter,
  sct_attribute_types_to_filter,
  sct_attribute_types_from_filter
)

### Operators -----------------------------------------------------------------

code_type_operators <- CODE_TYPE_TO_LKP_TABLE_MAP %>%
  dplyr::pull(.data[["code"]]) %>%
  purrr::map(\(x) list(
    type = x,
    nb_inputs = 1,
    multiple = FALSE,
    apply_to = "string"
  ))

operators <- c(code_type_operators,
               list(
                 list(
                   type = "equals",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "from ICD-10",
                   optgroup = "Map",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "ALL",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "sct_relationship",
                   nb_inputs = 2,
                   multiple = FALSE,
                   input = "select",
                   values = list(""),
                   apply_to = "string"
                 ),
                 list(
                   type = "sct_attribute_types",
                   nb_inputs = 1,
                   multiple = FALSE,
                   input = "select",
                   values = list(""),
                   apply_to = "string"
                 )
               ))

## reactable ---------------------------------------------------------------

csvDownloadButton <-
  function(id,
           filename = paste0(Sys.Date(), "_", "codelist.csv"),
           label = "Download as CSV") {
    tags$button(
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }

app_reactable <- function(df) {
  reactable::reactable(
    df,
    filterable = TRUE,
    searchable = TRUE,
    resizable = TRUE,
    paginationType = "jump",
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100, 200),
    onClick = "select",
    selection = "multiple"
  )
}
