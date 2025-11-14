# OVERVIEW ----------------------------------------------------------------

# Functions to map between different clinical codes e.g. between Read2 and
# Read3, or Read3 and ICD-10. These rely on the code mapping file provided by UK
# Biobank (resource 592: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) and
# the BNF to SNOMED mapping file from the NHSBSA website
# (https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping)

# EXPORTED FUNCTIONS ------------------------------------------------------

# Exploring and mapping clinical codes ------------------------------------

#' Get SNOMED codes with a specific set of one or more attributes
#'
#' Optionally filtered for specific relationship types. See examples (credit to
#' [snomedizer](https://snomedizer.web.app/articles/snomedizer.html)).
#'
#' @param attribute_codes Character vector of SNOMED codes.
#' @param relationship_type Character vector of SNOMED codes.
#' @inheritParams CODES
#' @inheritParams CHILDREN
#'
#' @return A dataframe
#' @family Clinical code lookups and mappings
#' @export
#'
#' @examples
#' \dontrun{
#' # Conditions associated with multiple sclerosis
#' HAS_ATTRIBUTES("24700007", relationship_type = "42752001")
#'
#' # Medicines with active ingredient timolol maleate
#' HAS_ATTRIBUTES("75359005", relationship_type = "10362801000001104")
#'
#' # Medicines with active ingredient beta blocker
#' HAS_ATTRIBUTES(CHILDREN("373254001", code_type = "sct"), relationship_type = "10362801000001104")
#'
#' # Conditions that are caused by bacteria belonging to Enterobacteriaceae
#' HAS_ATTRIBUTES(CHILDREN("106544002", code_type = "sct"), relationship_type = "246075003")
#'
#' # Infectious conditions that are caused by bacteria belonging to Enterobacteriaceae
#' HAS_ATTRIBUTES(CHILDREN("106544002", code_type = "sct"), relationship_type = "246075003") %AND%
#'   CHILDREN("40733004", code_type = "sct")
#' }
HAS_ATTRIBUTES <- function(
  attribute_codes,
  relationship_type = NULL,
  preferred_description_only = TRUE
) {
  get_relatives_sct(
    codes = attribute_codes,
    filter_col = "destinationId",
    return_col = "sourceId",
    typeId = relationship_type,
    include_self = FALSE,
    recursive = FALSE,
    preferred_description_only = preferred_description_only
  )
}

#' Get attributes for a set of SNOMED codes
#'
#' See examples (credit to
#' [snomedizer](https://snomedizer.web.app/articles/snomedizer.html)).
#'
#' @inheritParams HAS_ATTRIBUTES
#'
#' @return A data frame
#' @family Clinical code lookups and mappings
#' @export
#'
#' @examples
#' \dontrun{
#' # Body sites that can be affected by Enterobacteriaceae infections
#' enterobacteriaceae_infections <- HAS_ATTRIBUTES(
#'     CHILDREN("106544002 << Family Enterobacteriaceae (organism) >>",
#'              code_type = "sct"),
#'     relationship_type = "246075003 << Causative agent (attribute) >>") %AND%
#'   CHILDREN("40733004 << Infectious disease (disorder) >>", code_type = "sct")
#'
#' GET_ATTRIBUTES(enterobacteriaceae_infections,
#'                relationship_type = "363698007 << Finding site (attribute) >>")
#' }
GET_ATTRIBUTES <- function(
  attribute_codes,
  relationship_type = NULL,
  preferred_description_only = TRUE
) {
  get_relatives_sct(
    codes = attribute_codes,
    filter_col = "sourceId",
    return_col = "destinationId",
    typeId = relationship_type,
    include_self = FALSE,
    recursive = FALSE,
    preferred_description_only = preferred_description_only
  )
}

#' Find attribute types that point from a set of codes
#'
#' Find attribute types that point from a set of codes
#'
#' @param codes Codes to get attribute types for
#' @inheritParams HAS_ATTRIBUTES
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' # sct for acute iritis
#' acute_iritis <- CODES("29050005", "sct")
#'
#' # all attributes for this code
#' summarise_attributes_sct(acute_iritis)
#'
#' # attribute types pointing to this code
#' attribute_types <- ATTRIBUTE_TYPES_FROM(acute_iritis)
#' print(attribute_types)
#'
#' # codes with acute iritis as an attribute, with one of these attribute types specifically
#' GET_ATTRIBUTES(acute_iritis, attribute_types[1, ])
#' }
ATTRIBUTE_TYPES_FROM <- function(
  codes,
  preferred_description_only = TRUE
) {
  get_relatives_sct(
    codes = codes,
    filter_col = "sourceId",
    return_col = "typeId",
    typeId = NULL,
    include_self = FALSE,
    recursive = FALSE,
    preferred_description_only = preferred_description_only
  )
}

#' Find attribute types that point to a set of codes
#'
#' Find attribute types that point to a set of codes
#'
#' @param codes Codes to get attribute types for
#' @inheritParams HAS_ATTRIBUTES
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' # sct for acute iritis
#' acute_iritis <- CODES("29050005", "sct")
#'
#' # attribute types pointing to this code
#' attribute_types <- ATTRIBUTE_TYPES_TO(acute_iritis)
#' print(attribute_types)
#'
#' # codes with acute iritis as an attribute, with these attributes type(s)
#' HAS_ATTRIBUTES(acute_iritis, attribute_types)
#' }
ATTRIBUTE_TYPES_TO <- function(
  codes,
  preferred_description_only = TRUE
) {
  get_relatives_sct(
    codes = codes,
    filter_col = "destinationId",
    return_col = "typeId",
    typeId = NULL,
    include_self = FALSE,
    recursive = FALSE,
    preferred_description_only = preferred_description_only
  )
}

summarise_attributes_sct <- function(
  codes,
  all_lkps_maps = NULL,
  col_filters = getOption("codeminer.col_filters")
) {
  output_codes <- filter_sct_relationship(
    codes = NULL,
    sourceId_filter = codes,
    destinationId_filter = NULL,
    typeId_filter = NULL,
    active_only = TRUE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps
  )

  output_descriptions <- output_codes %>%
    as.list() %>%
    purrr::map(\(x) {
      CODES(
        codes = x,
        code_type = "sct",
        preferred_description_only = TRUE
      ) %>%
        dplyr::select(-tidyselect::all_of("code_type"))
    }) %>%
    purrr::imap(\(x, idx) {
      x %>%
        dplyr::rename_with(
          ~ paste0(idx, "_description"),
          tidyselect::all_of("description")
        ) %>%
        dplyr::rename_with(~idx, tidyselect::all_of("code"))
    })

  result <- output_codes

  for (x in names(output_descriptions)) {
    result <- result %>%
      dplyr::full_join(output_descriptions[[x]], by = x)
  }

  result %>%
    dplyr::select(
      tidyselect::starts_with("sourceId"),
      tidyselect::starts_with("typeId"),
      tidyselect::starts_with("destinationId")
    )
}

get_attributes_sct <- function(
  codes,
  standardise_output = TRUE,
  all_lkps_maps = NULL,
  preferred_description_only = TRUE,
  col_filters = getOption("codeminer.col_filters")
) {
  output_codes <- filter_sct_relationship(
    codes = NULL,
    sourceId_filter = codes,
    destinationId_filter = NULL,
    typeId_filter = NULL,
    active_only = TRUE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps
  )

  ## then expand to include both primary and secondary descriptions
  result <- unique(output_codes$destinationId)

  CODES(
    codes = result,
    code_type = "sct",
    preferred_description_only = preferred_description_only
  )
}


# Utilities ---------------------------------------------------------------

#' Default filtering parameters for lookup and mapping tables.
#'
#' To be used as `col_filters` argument in 'Clinical code lookups and mappings'
#' functions. Returns a named list where each name in the list refers to the
#' name of a lookup or mapping table. Each item is also a named list, where the
#' names refer to column names in the corresponding table, and the items are
#' vectors of values to filter for.
#'
#' @return A named list.
#' @export
#'
#' @family Clinical code lookups and mappings
#' @examples
#' default_col_filters()
default_col_filters <- function() {
  get_col_filters(defaults_only = TRUE, selected_table = NULL)
}


# PRIVATE FUNCTIONS -------------------------------------------------------

#' Connect to duckdb
#'
#' If a valid path to a duckdb database is supplied, then will create *in the
#' calling function environment* (i) a connection object `con` and (ii) a list
#' of tbl objects `all_lkps_maps`.
#'
#' @inheritParams CODES
#'
#' @return `NULL`
#' @noRd
create_db_connection <- function(all_lkps_maps) {
  expr <- NULL

  # connect to database file path if `all_lkps_maps` is a string, or `NULL`
  if (rlang::is_string(all_lkps_maps)) {
    expr <- rlang::expr({
      md <- stringr::str_starts(all_lkps_maps, "md:")
      con <- check_all_lkps_maps_path(all_lkps_maps, md)
      all_lkps_maps <- db_tables_to_list(con, md)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    })
  } else if (is.null(all_lkps_maps)) {
    if (Sys.getenv("ALL_LKPS_MAPS_DB") != "") {
      # message(paste0("Attempting to connect to ", Sys.getenv("ALL_LKPS_MAPS_DB")))
      expr <- rlang::expr({
        all_lkps_maps <- Sys.getenv("ALL_LKPS_MAPS_DB")
        md <- stringr::str_starts(all_lkps_maps, "md:")
        con <-
          check_all_lkps_maps_path(Sys.getenv("ALL_LKPS_MAPS_DB"), md)
        all_lkps_maps <- db_tables_to_list(con, md)
        on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      })
    } else if (file.exists("all_lkps_maps.db")) {
      # message("Attempting to connect to all_lkps_maps.db in current working directory")
      expr <- rlang::expr({
        con <- check_all_lkps_maps_path("all_lkps_maps.db", FALSE)
        all_lkps_maps <- db_tables_to_list(con, FALSE)
        on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      })
    } else {
      stop(
        "No/invalid path supplied to `all_lkps_maps` and no file called 'all_lkps_maps.db' found in current working directory. See `?all_lkps_maps_to_db()`"
      )
    }
  }
  rlang::eval_bare(expr, env = rlang::caller_env())
}

db_tables_to_list <- function(con, md) {
  # use "db" schema if motherduck connection
  schema <- NULL
  if (md) {
    schema <- "db"
  }

  tables <- DBI::dbListTables(con, schema = md) |>
    setdiff(c("shared_with_me", "databases", "owned_shares"))

  tables_named <- purrr::set_names(tables)

  if (md) {
    result <- purrr::map(
      tables_named,
      \(.x) dplyr::tbl(con, DBI::Id(schema = schema, table = .x))
    )
  } else {
    result <- purrr::map(
      tables_named,
      \(.x) dplyr::tbl(con, .x)
    )
  }

  return(result)
}

#' Get all available SNOMED CT relationship types
#'
#' @inheritParams CODES
#'
#' @return A data frame
#' @noRd
get_all_sct_relation_types <- function(all_lkps_maps = NULL) {
  create_db_connection(all_lkps_maps)

  all_lkps_maps$sct_relationship %>%
    dplyr::select(tidyselect::all_of("typeId")) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull(.data[["typeId"]]) %>%
    CODES(
      code_type = "sct",
      preferred_description_only = TRUE
    )
}

#' Filter SNOMED CT relationships based on various criteria
#'
#' @param codes A vector of SNOMED CT codes to filter relationships by.
#' @param sourceId_filter A vector of SNOMED CT codes to filter relationships by source.
#' @param destinationId_filter A vector of SNOMED CT codes to filter relationships by destination.
#' @param typeId_filter A vector of SNOMED CT codes to filter relationships by type.
#' @param active_only Logical indicating whether to filter relationships by active status.
#' @param recursive Logical indicating whether to filter relationships recursively.
#' @param all_lkps_maps A list of lookup maps.
#'
#' @return A data frame of filtered SNOMED CT relationships.
#' @export
filter_sct_relationship <- function(
  codes = NULL,
  sourceId_filter = NULL,
  destinationId_filter = NULL,
  typeId_filter = NULL,
  active_only = TRUE,
  recursive = FALSE,
  all_lkps_maps = NULL
) {
  # validate args
  stopifnot(
    !is.null(sourceId_filter) |
      !is.null(destinationId_filter) |
      !is.null(typeId_filter)
  )

  # either `sourceId_filter` or `destinationId_filter` should be `NULL` for
  # recursion
  if (recursive) {
    stopifnot(is.null(sourceId_filter) | is.null(destinationId_filter))
  }

  create_db_connection(all_lkps_maps)

  check_table_exists_in_all_lkps_maps(
    all_lkps_maps = all_lkps_maps,
    table_name = "sct_relationship"
  )

  # get related codes, applying supplied filters
  related_codes <- all_lkps_maps$sct_relationship

  if (!is.null(destinationId_filter)) {
    related_codes <-
      all_lkps_maps$sct_relationship %>%
      dplyr::filter(.data[["destinationId"]] %in% !!destinationId_filter)
  }

  if (!is.null(sourceId_filter)) {
    related_codes <- related_codes %>%
      dplyr::filter(.data[["sourceId"]] %in% !!sourceId_filter)
  }

  if (!is.null(typeId_filter)) {
    related_codes <- related_codes %>%
      dplyr::filter(.data[["typeId"]] %in% !!typeId_filter)
  }

  if (active_only) {
    related_codes <- related_codes %>%
      dplyr::filter(.data[["active"]] == "1")
  }

  related_codes <- related_codes %>%
    dplyr::select(tidyselect::all_of(c(
      "destinationId",
      "sourceId",
      "typeId"
    ))) %>%
    dplyr::collect()

  result <- dplyr::bind_rows(codes, related_codes) %>%
    dplyr::distinct()

  if (recursive) {
    if (nrow(result) > nrow(codes)) {
      ## determine relationship direction
      if (!is.null(destinationId_filter)) {
        destinationId_filter <- unique(result[["sourceId"]])
      }

      if (!is.null(sourceId_filter)) {
        sourceId_filter <- unique(result[["destinationId"]])
      }

      return(
        filter_sct_relationship(
          codes = result,
          sourceId_filter = sourceId_filter,
          destinationId_filter = destinationId_filter,
          typeId_filter = typeId_filter,
          active_only = active_only,
          recursive = recursive,
          all_lkps_maps = all_lkps_maps
        )
      )
    } else {
      return(result)
    }
  } else {
    return(result)
  }
}

#' Get data frame of relatives and attributes for a set of SNOMED codes
#'
#' Low level function
#'
#' @inheritParams get_relatives_sct
#'
#' @return A data frame with column names 'code' and 'typeId'
#' @noRd
#'
#' @examples
#' # get children codes for "269823000" ("Hemoglobin A1C - ...")
#' get_relatives_sct(
#'   codes = "269823000",
#'   relationship = "116680003",
#'   relationship_direction = "child"
#'   )
get_relatives_attributes_sct <- function(
  codes = NULL,
  relationship = NULL,
  relationship_direction = "child",
  recursive = TRUE,
  active_only = FALSE,
  all_lkps_maps = NULL
) {
  # Note: does not check whether `codes` or `relationship` exist or not

  # Note: returns self and relatives

  # validate args
  match.arg(relationship_direction, choices = c("child", "parent"))

  ## determine relationship direction
  filter_col <- switch(
    relationship_direction,
    child = "destinationId",
    parent = "sourceId"
  )

  return_col <- switch(
    relationship_direction,
    child = "sourceId",
    parent = "destinationId"
  )

  stopifnot(!is.null(codes) | !is.null(relationship))

  # codes - can be character vector or data frame
  if (!is.null(codes)) {
    if (is.character(codes)) {
      codes <- tibble::tibble(code = codes, typeId = NA_character_)
    }
  }

  create_db_connection(all_lkps_maps)

  # TO DELETE - too slow (maybe)

  # # validate relationship
  # assertthat::assert_that(relationship %in% dplyr::pull(all_lkps_maps$sct_relationship, .data[["typeId"]]),
  #                         msg = paste0("Unrecognised relationship: '",
  #                                      relationship,
  #                                      "'"))

  check_table_exists_in_all_lkps_maps(
    all_lkps_maps = all_lkps_maps,
    table_name = "sct_relationship"
  )

  # get related codes
  related_codes <- all_lkps_maps$sct_relationship

  if (!is.null(codes$code)) {
    related_codes <-
      all_lkps_maps$sct_relationship %>%
      dplyr::filter(.data[[filter_col]] %in% !!codes$code)
  }

  if (!is.null(relationship)) {
    related_codes <- related_codes %>%
      dplyr::filter(.data[["typeId"]] %in% !!relationship)
  }

  if (active_only) {
    related_codes <- related_codes %>%
      dplyr::filter(.data[["active"]] == "1")
  }

  related_codes <- related_codes %>%
    dplyr::select(tidyselect::all_of(c(return_col, "typeId"))) %>%
    dplyr::collect()

  names(related_codes)[which(names(related_codes) == return_col)] <- "code"

  result <- dplyr::bind_rows(codes, related_codes) %>%
    dplyr::distinct()

  if (is.null(codes)) {
    return(result)
  }

  if (recursive) {
    if (nrow(result) > nrow(codes)) {
      return(
        get_relatives_attributes_sct(
          codes = result,
          relationship = relationship,
          relationship_direction = relationship_direction,
          recursive = recursive,
          active_only = active_only,
          all_lkps_maps = all_lkps_maps
        )
      )
    } else {
      return(result)
    }
  } else {
    return(result)
  }
}

#' Extract column filters from metadata tables
#'
#' @param defaults_only Logical
#' @param selected_table Optional. Name of lookup or mapping table
#'
#' @return List
#' @export
get_col_filters <- function(defaults_only = TRUE, selected_table = NULL) {
  # validate args
  stopifnot(is.logical(defaults_only))

  stopifnot(
    selected_table %in%
      CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table |
      selected_table %in% CLINICAL_CODE_MAPPINGS_MAP$mapping_table
  )

  # get filter_cols
  result <- list(
    lookup = CODE_TYPE_TO_LKP_TABLE_MAP %>%
      dplyr::rename("table" = "lkp_table"),
    map = CLINICAL_CODE_MAPPINGS_MAP %>%
      dplyr::rename("table" = "mapping_table")
  ) %>%
    dplyr::bind_rows(.id = "lookup_map") %>%
    dplyr::select(tidyselect::all_of(c("table", "filter_cols"))) %>%
    dplyr::filter(!is.na(.data[["filter_cols"]]))

  result <- rlang::set_names(result$filter_cols, nm = result$table) %>%
    purrr::map(\(x) x[[1]])

  # get either all values, or default selections only for filter_cols
  if (defaults_only) {
    result <- result %>%
      purrr::map(\(x) {
        x %>%
          purrr::map(\(x) subset(x, stringr::str_detect(x, "\\*")))
      })
  }

  # remove '*' (these are used to indicate default choices)
  result <- result %>%
    purrr::map(\(x) {
      x %>%
        purrr::map(stringr::str_remove_all, pattern = "\\*")
    })

  # select single table, if requested
  if (!is.null(selected_table)) {
    result <- result[[selected_table]]
  }

  result
}

#' Helper function for \code{\link{MAP}}
#'
#' Returns the requested value for a 'mapping_table' in
#' \code{CLINICAL_CODE_MAPPINGS_MAP}.
#'
#' @param mapping_table character
#' @param value character. column name from
#'   \code{CLINICAL_CODE_MAPPINGS_MAP} (apart from
#'   "mapping_table").
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_value_for_mapping_sheet <- function(mapping_table, value) {
  # validate args
  match.arg(
    arg = mapping_table,
    choices = CLINICAL_CODE_MAPPINGS_MAP[["mapping_table"]]
  )

  match.arg(
    arg = value,
    choices = subset(
      names(CLINICAL_CODE_MAPPINGS_MAP),
      subset = names(CLINICAL_CODE_MAPPINGS_MAP) != "mapping_table"
    )
  )

  # return specified `value`
  CLINICAL_CODE_MAPPINGS_MAP[
    CLINICAL_CODE_MAPPINGS_MAP[["mapping_table"]] == mapping_table,
  ][[value]]
}

#' Filter lookup/mapping table for specified values in columns
#'
#' Helper function that enables filtering of lookup/mapping tables for certain
#' values (e.g. filter a mapping table for only 'exact' code mappings). Uses an
#' `%in%` filter statement. Note that no error is currently raised if attempting
#' to filter for any values that do not exist in `df` columns.
#'
#' @param df Lookup or mapping df to be filtered
#' @param df_name Name of lookup/mapping df (e.g. "icd10_lkp").
#' @param col_filters Either `NULL` (default, in which case `df` is returned
#'   unchanged) or a named list. First level names are names of lookup/mapping
#'   tables. Each item is also a named list of vectors. Columns in `df` that
#'   match the list names are filtered for values in the corresponding vectors
#'   (using `%in%`). An error is raised
#'
#' @return A dataframe
#' @noRd
filter_cols <- function(df, df_name, col_filters = NULL) {
  # if `col_filters` is `NULL`, return `df` unchanged (exit early)
  if (is.null(col_filters)) {
    return(df)
  }

  # get relevant columns/filter values from `col_filters`
  col_filters <- col_filters[[df_name]]

  # if `df_name` is not present in `names(col_filters)` return `df` unchanged (exit early)
  if (is.null(col_filters)) {
    return(df)
  }

  # check that selected element of `col_filters` is a named list of vectors
  stopifnot(is.list(col_filters))
  if (is.null(names(col_filters))) {
    stop("Each item in `col_filters` must be named")
  }

  if (any(names(col_filters) == "")) {
    stop("Each item in `col_filters` must be named")
  }

  col_filters_item_types <- col_filters %>%
    purrr::map_lgl(\(x) is.vector(x) || is.null(x))

  assertthat::assert_that(
    sum(!col_filters_item_types) == 0,
    msg = "Each item in `col_filters` must be a vector"
  )

  # check that column names exist in df. Raise error is any are unrecognised.
  unrecognised_colnames <-
    subset(names(col_filters), !names(col_filters) %in% names(df))

  if (length(unrecognised_colnames) > 0) {
    stop(
      paste0(
        "The following ",
        length(unrecognised_colnames),
        " column names specified by `col_filters` are not present in `",
        df_name,
        "`: ",
        stringr::str_c(
          unrecognised_colnames,
          sep = "",
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # filter `df` for specified values in each column listed by `col_filters`
  for (i in names(col_filters)) {
    col_filter_values <- col_filters[[i]]

    if (!is.null(col_filter_values)) {
      # check that type matches
      df_col_class <- class(df[[i]])
      col_filter_values_class <- class(col_filter_values)
      assertthat::assert_that(
        df_col_class %in% col_filter_values_class,
        msg = paste0(
          "Cannot filter column ",
          i,
          " in ",
          df_name,
          " as classes do not match. Column `",
          i,
          "`` is class ",
          df_col_class,
          ", but filter values specified by `col_filters` are of class ",
          col_filter_values_class
        )
      )

      df <- df %>%
        dplyr::filter(.data[[i]] %in% !!col_filter_values)
    }
  }

  # return result
  return(df)
}

#' Get a mapping table for ICD10 codes in ALT_CODE format, with and without 'X'
#' appended for undivided 3 character codes
#'
#' @param icd10_lkp The lookup table for ICD10 codes, `icd10_lkp`.
#' @param undivided_3char_only If `TRUE` return only undivided 3 character ICD10
#'   codes. Default is `FALSE`.
#' @param as_named_list If `NULL`, returns a data frame with columns `ALT_CODE`
#'   and `ALT_CODE_minus_x`. If 'names_no_x' or 'names_with_x', returns a named
#'   list with either `ALT_CODE` or `ALT_CODE_minus_x` set as names (and the
#'   other as values) respectively.
#'
#' @return A data frame or a named list (see argument `as_named_list`).
#' @noRd
get_icd10_code_alt_code_x_map <- function(
  icd10_lkp,
  undivided_3char_only = FALSE,
  as_named_list = NULL
) {
  # validate args
  match.arg(as_named_list, choices = c("names_no_x", "names_with_x"))

  # make mapping df
  icd10_lkp_alt_x_map <- icd10_lkp %>%
    dplyr::select(tidyselect::all_of("ALT_CODE")) %>%
    dplyr::filter(!is.na(.data[["ALT_CODE"]])) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      "ALT_CODE_minus_x" = stringr::str_remove(
        .data[["ALT_CODE"]],
        pattern = "X$"
      )
    )

  # check that all codes are unique
  stopifnot(
    length(icd10_lkp_alt_x_map$ALT_CODE) ==
      dplyr::n_distinct(icd10_lkp_alt_x_map$ALT_CODE)
  )
  stopifnot(
    length(icd10_lkp_alt_x_map$ALT_CODE_minus_x) ==
      dplyr::n_distinct(icd10_lkp_alt_x_map$ALT_CODE_minus_x)
  )

  # check that all codes are the same comparing `ALT_CODE` and
  # `ALT_CODE_minus_x`, after removing the X in `ALT_CODE`
  stopifnot(all(
    icd10_lkp_alt_x_map$ALT_CODE_minus_x ==
      stringr::str_remove(icd10_lkp_alt_x_map$ALT_CODE, pattern = "X$")
  ))

  if (undivided_3char_only) {
    icd10_lkp_alt_x_map <- icd10_lkp_alt_x_map %>%
      dplyr::filter(.data[["ALT_CODE_minus_x"]] != .data[["ALT_CODE"]])
  }

  # convert to named list
  if (!is.null(as_named_list)) {
    icd10_lkp_alt_x_map <- switch(
      as_named_list,
      names_no_x = icd10_lkp_alt_x_map %>%
        tidyr::pivot_wider(
          names_from = "ALT_CODE_minus_x",
          values_from = "ALT_CODE"
        ) %>%
        as.list(),
      names_with_x = icd10_lkp_alt_x_map %>%
        tidyr::pivot_wider(
          names_from = "ALT_CODE",
          values_from = "ALT_CODE_minus_x"
        ) %>%
        as.list()
    )
  }

  # return result
  return(icd10_lkp_alt_x_map)
}

#' Get a vector of ICD10 codes in ALT_CODE format for a specified start/end
#' range of ICD10 codes
#'
#' Note that `start_icd10_code` and `end_icd10_code` must be of the same length,
#' unless one ends with 'X'. For example, expanding the range 'A80-A81' is
#' equivalent to expanding both 'A800-A809' and 'A810-A819'.
#'
#' @param start_icd10_code String
#' @param end_icd10_code String
#' @param icd10_lkp The ICD10 lookup table. Must have a `.rowid` column.
#'
#' @noRd
#' @return A character vector of
get_icd10_code_range <- function(start_icd10_code, end_icd10_code, icd10_lkp) {
  # validate args
  stopifnot(is.character(start_icd10_code))
  stopifnot(is.character(end_icd10_code))

  assertthat::assert_that(
    stringr::str_length(stringr::str_remove(
      start_icd10_code,
      "X$"
    )) ==
      stringr::str_length(stringr::str_remove(
        end_icd10_code,
        "X$"
      )),
    msg = paste0(
      "`start_icd10_code` and `end_icd10_code` must have the same number of characters. Start/end values provided: ",
      start_icd10_code,
      " and ",
      end_icd10_code
    )
  )

  check_rowid_col_present(
    df = icd10_lkp,
    df_name = "icd10_lkp"
  )

  check_codes_exist(
    codes = c(start_icd10_code, end_icd10_code),
    lkp_codes = icd10_lkp$ALT_CODE,
    code_type = "icd10",
    table_name = "icd10_lkp",
    return_unrecognised_codes = FALSE
  )

  # get start and end row indices
  start_rowid <- icd10_lkp %>%
    dplyr::filter(.data[["ALT_CODE"]] == !!start_icd10_code) %>%
    dplyr::pull(.data[[".rowid"]])

  end_rowid <- icd10_lkp %>%
    dplyr::filter(.data[["ALT_CODE"]] == !!end_icd10_code) %>%
    dplyr::pull(.data[[".rowid"]])

  # check start/end row indices are scalar, and create range of row index integers
  assertthat::is.number(start_rowid)
  assertthat::is.number(end_rowid)

  icd10_lkp_rowids <- start_rowid:end_rowid

  # filter for selected row index integers
  result <- icd10_lkp %>%
    dplyr::filter(.data[[".rowid"]] %in% icd10_lkp_rowids) %>%
    dplyr::pull(.data[["ALT_CODE"]])

  pattern <- stringr::str_c(paste0("^", result), sep = "", collapse = "|")

  # expand (e.g. for 'A80-A81', at this stage all 'A80' should be present
  # ('A800-A809'), but for 'A81', only 'A81' wil be present - needs expanding
  # to 'A801-A819')
  result <- icd10_lkp %>%
    dplyr::filter(stringr::str_detect(
      .data[["ALT_CODE"]],
      pattern = pattern
    )) %>%
    dplyr::pull(.data[["ALT_CODE"]])

  return(result)
}

## Validation helpers ---------------------------

check_all_lkps_maps_path <- function(file_path, md = FALSE) {
  # determine whether using motherduck
  if (md) {
    # connect - requires motherduck extension and `motherduck_token`
    # environmental variable
    con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)
    DBI::dbExecute(con, stringr::str_glue("ATTACH '{file_path}' AS db;"))
  } else {
    # check file exists
    assertthat::assert_that(
      file.exists(file_path),
      msg = paste0("Error! No file found at ", file_path)
    )

    # check file ends with '.db'
    assertthat::assert_that(stringr::str_detect(
      file_path,
      ".+\\.db"
    ))

    # connect
    con <- DBI::dbConnect(duckdb::duckdb(), file_path, read_only = TRUE)
  }
  # return con object if tests pass
  return(con)
}

check_rowid_col_present <- function(df, df_name) {
  assertthat::assert_that(
    ".rowid" %in% names(df),
    msg = paste0(
      "'.rowid' column not present in `",
      df_name,
      "`"
    )
  )
  assertthat::assert_that(
    class(df[[".rowid"]]) == "integer",
    msg = paste0(
      "'.rowid' column in `",
      df_name,
      "should be class 'integer', not '",
      class(df[[".rowid"]]),
      "'"
    )
  )
}

#' Helper function - raise error or warning if unrecognised codes are present
#'
#' Raises an error or warning for unrecognised codes.
#'
#' @param unrecognised_codes Either 'error' or 'warning'. Determines how to
#'   handle unrecognised codes
#' @param missing_codes character vector of unrecognised codes.
#' @param code_type The type of clinical coding system
#' @param table_name Name of lookup/mapping table from which codes are missing
#'
#' @return Called for side effects.
#' @noRd
#' @family Clinical code lookups and mappings
handle_unrecognised_codes <-
  function(unrecognised_codes, missing_codes, table_name, code_type) {
    if (is.null(unrecognised_codes)) {
      unrecognised_codes <- "error"
    }

    match.arg(unrecognised_codes, choices = c("error", "warning"))

    # make sure missing_codes are unique, if not already
    missing_codes <- unique(missing_codes)

    # only display first 25 codes
    if (length(missing_codes) > 25) {
      missing_codes_to_print <- utils::head(missing_codes, n = 25)
    } else {
      missing_codes_to_print <- missing_codes
    }

    missing_codes_to_print <- stringr::str_c(
      missing_codes_to_print,
      sep = "",
      collapse = "', '"
    )

    if (length(missing_codes) > 25) {
      missing_codes_to_print <- paste0(
        missing_codes_to_print,
        " (first 25 only shown)"
      )
    }

    if (length(missing_codes) > 0) {
      missing_codes_message <- paste0(
        "The following ",
        length(missing_codes),
        " codes were not found for '",
        code_type,
        "' in table '",
        table_name,
        "': '",
        stringr::str_c(
          missing_codes_to_print,
          sep = "",
          collapse = "', '"
        ),
        "'"
      )

      switch(
        unrecognised_codes,
        error = stop(missing_codes_message),
        warning = warning(missing_codes_message)
      )
    }
  }

#' Utility function - check if all of a set of codes are recognised
#'
#' @param codes Codes being checked
#' @param lkp_codes Character vector of lookup codes. Any `codes` not present in
#'   `lkp_codes` are considered to be unrecognised.
#' @param code_type String
#' @param return_unrecognised_codes If `TRUE`, return a character vector of
#'   unrecognised codes. Default is `FALSE`.check
#' @param unrecognised_codes Either 'error' or 'warning'. Determines how to
#'   handle unrecognised codes.
#' @param table_name Name of lookup/mapping table from which `lkp_codes` was
#'   obtained.
#'
#' @noRd
#' @return Return vector of unrecognised codes if `return_missing_codes` is
#'   `TRUE`, otherwise called for side effect (error if any unrecognised codes)
check_codes_exist <- function(
  codes,
  lkp_codes,
  table_name,
  code_type = getOption("codeminer.code_type"),
  return_unrecognised_codes = FALSE,
  unrecognised_codes = "error"
) {
  # check for unrecognised('missing') codes
  missing_codes <- subset(codes, !codes %in% lkp_codes)

  # return missing codes, if requested
  if (return_unrecognised_codes) {
    return(missing_codes)
  }

  # ...otherwise return error
  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = table_name,
    code_type = code_type
  )
}

check_table_exists_in_all_lkps_maps <- function(all_lkps_maps, table_name) {
  assertthat::assert_that(
    table_name %in% names(all_lkps_maps),
    msg = paste0("Required table ", table_name, " not found in `all_lkps_maps`")
  )
}
