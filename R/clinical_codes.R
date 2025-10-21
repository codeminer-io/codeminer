


# OVERVIEW ----------------------------------------------------------------

# Functions to map between different clinical codes e.g. between Read2 and
# Read3, or Read3 and ICD-10. These rely on the code mapping file provided by UK
# Biobank (resource 592: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592) and
# the BNF to SNOMED mapping file from the NHSBSA website
# (https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping)

# EXPORTED FUNCTIONS ------------------------------------------------------

# Exploring and mapping clinical codes ------------------------------------


#' Search for codes that match a description
#'
#' Returns a data frame with clinical codes with descriptions that match the
#' supplied regular expression. Ignores case by default.
#'
#' @param reg_expr a regular expression to search for
#' @inheritParams stringr::regex
#' @inheritParams lookup_codes
#' @param ignore_case If `TRUE` (default), ignore case in `reg_expr`.
#' @param codes_only bool. If \code{TRUE}, return a character vector of
#'   \emph{unique} codes. If \code{FALSE} (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#'
#' @return data frame by default, or a character vector of codes if
#'   \code{codes_only} is \code{TRUE}.
#' @export
#' @name code_descriptions_like
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' code_descriptions_like(
#'   reg_expr = "cyst",
#'   code_type = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
code_descriptions_like <- function(reg_expr,
                                   code_type = getOption("codeminer.code_type"),
                                   all_lkps_maps = NULL,
                                   ignore_case = TRUE,
                                   codes_only = FALSE,
                                   preferred_description_only = TRUE,
                                   standardise_output = TRUE,
                                   col_filters = getOption("codeminer.col_filters")) {
  # validate args
  assertthat::is.string(reg_expr)

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`"
  )

  match.arg(
    arg = code_type,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code and description columns for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "description_col"
    )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # search for codes

  ## first get all codes matching description. This may not capture the primary
  ## description though e.g. searching for 'QOF' won't capture the primary
  ## description 'Quality and Outcome...'

  # Note - it isn't possible to specify `ignore_case` when using dbplyr, so use
  # `tolower()`
  if (ignore_case) {
    result <- all_lkps_maps[[lkp_table]] %>%
      dplyr::filter(stringr::str_detect(
        string = tolower(.data[[description_col]]),
        pattern = tolower(reg_expr)
      )) %>%
      dplyr::collect()
  } else {
    # if `ignore_case` is `FALSE`, then same code will work for both data
    # frame/tbl_dbi object
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(stringr::str_detect(
      string = .data[[description_col]],
      pattern = reg_expr
    )) %>%
    dplyr::collect()
  }

  ## then expand to include both primary and secondary descriptions
  codes <- unique(result[[code_col]])

  codes <- subset(codes,
                  !is.na(codes))

  result <- lookup_codes(
    codes = codes,
    code_type = code_type,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )

  if (codes_only) {
    if (standardise_output) {
      return(result$code)
    } else {
      return(result[[code_col]])
    }
  } else {
    return(result)
  }
}

#' Search for codes matching a regular expression
#'
#' Returns a data frame with clinical codes that match the supplied regular
#' expression. Case is *not* ignored.
#'
#' @param reg_expr a regular expression to search for
#' @inheritParams stringr::regex
#' @inheritParams lookup_codes
#' @param codes_only bool. If \code{TRUE}, return a character vector of
#'   \emph{unique} codes. If \code{FALSE} (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#'
#' @return data frame by default, or a character vector of codes if
#'   \code{codes_only} is \code{TRUE}.
#' @export
#' @name codes_like
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' CODES_LIKE(
#'   reg_expr = "^E10.*",
#'   code_type = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
CODES_LIKE <- function(reg_expr,
                       code_type = getOption("codeminer.code_type"),
                       all_lkps_maps = NULL,
                       codes_only = FALSE,
                       preferred_description_only = TRUE,
                       standardise_output = TRUE,
                       col_filters = getOption("codeminer.col_filters")) {
  # validate args
  assertthat::is.string(reg_expr)

  assertthat::assert_that(!(codes_only & standardise_output), msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`")

  match.arg(arg = code_type, choices = CODE_TYPE_TO_LKP_TABLE_MAP$code)

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code and description columns for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_table, column = "code_col")

  description_col <-
    get_col_for_lookup_sheet(lookup_sheet = lkp_table, column = "description_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(lookup_sheet = lkp_table, column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # search for codes

  ## get all codes matching regex

  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(stringr::str_detect(string = .data[[code_col]], pattern = reg_expr)) %>%
    dplyr::collect()

  ## then expand, optionally including both primary and secondary descriptions
  codes <- unique(result[[code_col]])

  codes <- subset(codes, !is.na(codes))

  result <- lookup_codes(
    codes = codes,
    code_type = code_type,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )

  if (codes_only) {
    if (standardise_output) {
      return(result$code)
    } else {
      return(result[[code_col]])
    }
  } else {
    return(result)
  }
}

#' @rdname code_descriptions_like
#' @export
DESCRIPTION <- code_descriptions_like

#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup
#' @param code_type character. Type of clinical code system to be searched. One
#'   of `r knitr::combine_words(CODE_TYPE_TO_LKP_TABLE_MAP$code, and = "or ")`.
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns from the relevant look up table.
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised, then either an error or warning will be raised.
#' @param .return_unrecognised_codes If `TRUE`, return a vector of unrecognised
#'   codes only.
#' @param col_filters A named list where each name in the list refers to the
#'   name of a lookup or mapping table. Each item is also a named list, where
#'   the names refer to column names in the corresponding table, and the items
#'   are vectors of values to filter for. For example, `list(my_lookup_table =
#'   list(colA = c("A", "B"))` will result in `my_lookup_table` being filtered
#'   for rows where `colA` is either 'A' or 'B'. Uses `default_col_filters()` by
#'   default. Set to `NULL` to remove all filters.
#' @param preferred_description_only If `TRUE` (default), return only preferred
#'   descriptions for clinical codes with synonyms. Will only apply if
#'   \code{standardise_output} is also \code{TRUE}.
#' @param all_lkps_maps Either a named list of lookup and mapping tables
#'   (either data frames or `tbl_dbi` objects), or the path to a Duckdb database
#'   containing these tables. If `NULL`, will attempt to connect to a Duckdb
#'   database named 'all_lkps_maps.db' in the current working directory, or to
#'   a a Duckdb database specified by an environmental variable named
#'   'ALL_LKPS_MAPS_DB' (see
#'   [here](https://resources.numbat.space/using-rprofile-and-renviron.html#renviron)
#'   for how to set environment variables using a `.Renviron` file). The latter
#'   method will be used in preference.
#'
#' @return data frame
#' @export
#' @name lookup_codes
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # look up ICD10 codes
#' lookup_codes(
#'   codes = c("E10", "E11"),
#'   code_type = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
lookup_codes <- function(codes,
                         code_type = getOption("codeminer.code_type"),
                         all_lkps_maps = NULL,
                         preferred_description_only = TRUE,
                         standardise_output = TRUE,
                         unrecognised_codes = getOption("codeminer.unrecognised_codes_lookup"),
                         col_filters = getOption("codeminer.col_filters"),
                         .return_unrecognised_codes = FALSE) {

  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    code_type <- codes$code_type[1]
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  stopifnot(!is.null(code_type))

  match.arg(
    arg = code_type,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = lkp_table)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  # determine description column for lookup sheet
  description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "description_col"
    )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # lookup - filter lookup sheet for codes
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(.data[[code_col]] %in% codes) %>%
    dplyr::collect()

  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = lkp_table,
      col_filters = col_filters
    )
  }

  # check for unrecognised codes
  missing_codes <- subset(codes, !codes %in% result[[code_col]])

  if (.return_unrecognised_codes) {
    # optionally return vector of unrecognised codes only
    message(paste0(
      "Returning unrecognised codes only. N unrecognised: ",
      length(missing_codes)
    ))
    return(missing_codes)
  }

  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = lkp_table,
    code_type = code_type
  )

  # filter for preferred code descriptions only if requested
  if (preferred_description_only &
      !is.na(preferred_description_col)) {
    result <- result %>%
      dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
  }

  # standardise output if requested
  if (standardise_output) {
    result <- standardise_output_fn(
      result,
      lkp_table = lkp_table,
      code_col = code_col,
      description_col = description_col,
      code_type = code_type
    )
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found")
    return(result)
  } else {
    # return either unique codes only, or df including code descriptions
    return(result)
  }
}

#' @rdname lookup_codes
#' @export
CODES <- lookup_codes

#' Get descendents for a code
#'
#' Retrieves children codes for a given set of codes (including the codes
#' themselves). Note that currently it is not possible to retrieve children
#' codes for certain clinical coding systems, such as Read 3.
#'
#' @param codes character. A vector of code strings to retrieve child codes for.
#' @param codes_only bool. If \code{TRUE}, return a character vector of
#'   \emph{unique} codes. If \code{FALSE} (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#' @inheritParams lookup_codes
#'
#' @return A data frame
#' @name get_child_codes
#' @export
#'
#' @seealso [get_children_sct()]
#' @family Clinical code lookups and mappings
#' @examples
#' # TODO
get_child_codes <- function(codes,
                            code_type = getOption("codeminer.code_type"),
                            all_lkps_maps = NULL,
                            codes_only = FALSE,
                            preferred_description_only = TRUE,
                            standardise_output = TRUE,
                            unrecognised_codes = "error",
                            col_filters = getOption("codeminer.col_filters")) {

  # check codes exist
  codes <- lookup_codes(
    codes = codes,
    code_type = code_type,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = unrecognised_codes,
    col_filters = col_filters,
    .return_unrecognised_codes = FALSE
  )

  if (!is.null(codes)) {
    code_type <- codes$code_type[1]

    codes <- codes %>%
      dplyr::pull(tidyselect::all_of("code")) %>%
      unique()
  } else {
    return(codes)
  }

  # get child codes
  if (code_type == "sct") {
    get_children_sct(
      codes = codes,
      standardise_output = standardise_output,
      include_self = TRUE,
      include_descendants = TRUE,
      all_lkps_maps = all_lkps_maps,
      preferred_description_only = preferred_description_only,
      col_filters = col_filters
    )
  } else if (code_type %in% c(
    "bnf",
    "icd9",
    "icd10",
    "read2",
    "read2_drugs",
    "opcs4",
    "phecode"
  )) {
    codes_starting_with(codes = codes,
                        code_type = code_type,
                        all_lkps_maps = all_lkps_maps,
                        codes_only = codes_only,
                        preferred_description_only = preferred_description_only,
                        standardise_output = standardise_output,
                        col_filters = col_filters,
                        escape_dot = FALSE)
  } else {
    stop(paste0(
      "Currently codeminer is unable to retrieve child codes for ",
      code_type
    ))
  }

}


#' @rdname get_child_codes
#' @export
CHILDREN <- get_child_codes

#' Get children for SNOMED codes
#'
#' @param codes Character vector of SNOMED codes.
#' @param standardise_output If `TRUE` (default) return a data frame with columns
#'   'code', 'description' and 'code_type'.
#' @param include_self If `TRUE` (default) include input codes in the result.
#' @param include_descendants If `TRUE` (default) return all descendant codes,
#'   as well as immediate children.
#' @inheritParams lookup_codes
#' @inheritParams get_child_codes
#'
#' @return A dataframe
#' @seealso [get_child_codes()], [get_parents_sct()]
#' @family Clinical code lookups and mappings
#' @export
get_children_sct <- function(codes,
                            standardise_output = TRUE,
                            include_self = TRUE,
                            include_descendants = TRUE,
                            all_lkps_maps = NULL,
                            preferred_description_only = TRUE,
                            col_filters = getOption("codeminer.col_filters")) {

  get_relatives_sct(
    codes = codes,
    filter_col = "destinationId",
    return_col = "sourceId",
    typeId = "116680003",
    standardise_output = standardise_output,
    include_self = include_self,
    recursive = include_descendants,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )
}

#' Get parents for SNOMED codes
#'
#' @param codes Character vector of SNOMED codes.
#' @param standardise_output If `TRUE` (default) return a data frame with columns
#'   'code', 'description' and 'code_type'.
#' @param include_self If `TRUE` (default) include input codes in the result.
#' @param include_ancestors If `TRUE` (default) return all ancestor codes,
#'   as well as immediate parents.
#' @inheritParams lookup_codes
#' @inheritParams get_child_codes
#'
#' @return A dataframe
#' @seealso [get_child_codes()], [get_children_sct()]
#' @family Clinical code lookups and mappings
#' @export
get_parents_sct <- function(codes,
                            standardise_output = TRUE,
                            include_self = TRUE,
                            include_ancestors = TRUE,
                            all_lkps_maps = NULL,
                            preferred_description_only = TRUE,
                            col_filters = getOption("codeminer.col_filters")) {
  get_relatives_sct(
    codes = codes,
    filter_col = "sourceId",
    return_col = "destinationId",
    typeId = "116680003",
    standardise_output = standardise_output,
    include_self = include_self,
    recursive = include_ancestors,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )
}

#' Get SNOMED codes with a specific set of one or more attributes
#'
#' Optionally filtered for specific relationship types. See examples (credit to
#' [snomedizer](https://snomedizer.web.app/articles/snomedizer.html)).
#'
#' @param attribute_codes Character vector of SNOMED codes.
#' @param relationship_type Character vector of SNOMED codes.
#' @inheritParams lookup_codes
#' @inheritParams get_child_codes
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
HAS_ATTRIBUTES <- function(attribute_codes,
                          relationship_type = NULL,
                          standardise_output = TRUE,
                          all_lkps_maps = NULL,
                          preferred_description_only = TRUE,
                          col_filters = getOption("codeminer.col_filters")) {
  get_relatives_sct(
    codes = attribute_codes,
    filter_col = "destinationId",
    return_col = "sourceId",
    typeId = relationship_type,
    standardise_output = standardise_output,
    include_self = FALSE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
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
GET_ATTRIBUTES <- function(attribute_codes,
                          relationship_type = NULL,
                          standardise_output = TRUE,
                          all_lkps_maps = NULL,
                          preferred_description_only = TRUE,
                          col_filters = getOption("codeminer.col_filters")) {
  get_relatives_sct(
    codes = attribute_codes,
    filter_col = "sourceId",
    return_col = "destinationId",
    typeId = relationship_type,
    standardise_output = standardise_output,
    include_self = FALSE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
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
ATTRIBUTE_TYPES_FROM <- function(codes,
                                 standardise_output = TRUE,
                                 all_lkps_maps = NULL,
                                 preferred_description_only = TRUE,
                                 col_filters = getOption("codeminer.col_filters")) {
  get_relatives_sct(
    codes = codes,
    filter_col = "sourceId",
    return_col = "typeId",
    typeId = NULL,
    standardise_output = standardise_output,
    include_self = FALSE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
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
ATTRIBUTE_TYPES_TO <- function(codes,
                                 standardise_output = TRUE,
                                 all_lkps_maps = NULL,
                                 preferred_description_only = TRUE,
                                 col_filters = getOption("codeminer.col_filters")) {
  get_relatives_sct(
    codes = codes,
    filter_col = "destinationId",
    return_col = "typeId",
    typeId = NULL,
    standardise_output = standardise_output,
    include_self = FALSE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    col_filters = col_filters
  )
}

get_relatives_sct <- function(codes = NULL,
                              filter_col = "destinationId",
                              return_col = "sourceId",
                              typeId = "116680003",
                              standardise_output = TRUE,
                              include_self = TRUE,
                              recursive = TRUE,
                              all_lkps_maps = NULL,
                              preferred_description_only = TRUE,
                              col_filters = getOption("codeminer.col_filters")) {


  if (!is.null(codes)) {
    # TODO - create df and string methods; validate codes df
    if (is.data.frame(codes)) {
      code_type <- unique(codes$code_type)
      codes <- codes$code
    }

    # validate args
    check_codes(codes)

    if (length(codes) == 1) {
      codes <- codes_string_to_vector(codes)
    }
  }

  if (!is.null(typeId)) {
    # TODO - create df and string methods; validate typeId df
    if (is.data.frame(typeId)) {
      code_type <- unique(typeId$code_type)
      typeId <- typeId$code
    }

    # validate args
    check_codes(typeId)

    if (length(typeId) == 1) {
      typeId <- codes_string_to_vector(typeId)
    }
  }

  match.arg(filter_col,
            choices = c("sourceId", "destinationId"))

  # set up
  sourceId_filter <- destinationId_filter <- NULL

  if (filter_col == "sourceId") {
    sourceId_filter <- codes
  }

  if (filter_col == "destinationId") {
    destinationId_filter <- codes
  }

  # get codes from relationship table
  input_codes <- filter_sct_relationship(codes = NULL,
                                         sourceId_filter = sourceId_filter,
                                         destinationId_filter = destinationId_filter,
                                         typeId_filter = typeId,
                                         active_only = TRUE,
                                         recursive = FALSE,
                                         all_lkps_maps = all_lkps_maps)

  sourceId_filter <- destinationId_filter <- NULL

  if (filter_col == "sourceId") {
    sourceId_filter <- unique(input_codes$destinationId)
  }

  if (filter_col == "destinationId") {
    destinationId_filter <- unique(input_codes$sourceId)
  }

  # perform search recursively
  if (recursive) {
  output_codes <- filter_sct_relationship(codes = input_codes,
                                          sourceId_filter = sourceId_filter,
                                          destinationId_filter = destinationId_filter,
                                          typeId_filter = typeId,
                                          active_only = TRUE,
                                          recursive = recursive,
                                          all_lkps_maps = all_lkps_maps)
  } else {
    output_codes <- input_codes
  }

  # lookup results
  result <- unique(output_codes[[return_col]])

  if (include_self) {
    result <- c(codes, result)
  }

  lookup_codes(
    codes = result,
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )
}

summarise_attributes_sct <- function(codes,
                                     all_lkps_maps = NULL,
                                     col_filters = getOption("codeminer.col_filters")) {
  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    code_type <- unique(codes$code_type)
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  output_codes <- filter_sct_relationship(codes = NULL,
                                          sourceId_filter = codes,
                                          destinationId_filter = NULL,
                                          typeId_filter = NULL,
                                          active_only = TRUE,
                                          recursive = FALSE,
                                          all_lkps_maps = all_lkps_maps)

  typeID_descriptions <- lookup_codes(
    codes = output_codes$typeId,
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )

  output_descriptions <- output_codes %>%
    as.list() %>%
    purrr::map(\(x) lookup_codes(
      codes = x,
      code_type = "sct",
      all_lkps_maps = all_lkps_maps,
      preferred_description_only = TRUE,
      standardise_output = TRUE,
      col_filters = col_filters,
      unrecognised_codes = "error",
      .return_unrecognised_codes = FALSE
    ) %>%
      dplyr::select(-tidyselect::all_of("code_type"))) %>%
    purrr::imap(\(x, idx) x %>%
                  dplyr::rename_with(~ paste0(idx, "_description"),
                                     tidyselect::all_of("description")) %>%
                  dplyr::rename_with(~ idx,
                                     tidyselect::all_of("code")))

  result <- output_codes

  for (x in names(output_descriptions)) {
    result <- result %>%
      dplyr::full_join(output_descriptions[[x]],
                       by = x)
  }

  result %>%
    dplyr::select(tidyselect::starts_with("sourceId"),
                  tidyselect::starts_with("typeId"),
                  tidyselect::starts_with("destinationId"))
}

get_attributes_sct <- function(codes,
                               standardise_output = TRUE,
                               all_lkps_maps = NULL,
                               preferred_description_only = TRUE,
                               col_filters = getOption("codeminer.col_filters")) {

  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    code_type <- unique(codes$code_type)
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  output_codes <- filter_sct_relationship(codes = NULL,
                                          sourceId_filter = codes,
                                          destinationId_filter = NULL,
                                          typeId_filter = NULL,
                                          active_only = TRUE,
                                          recursive = FALSE,
                                          all_lkps_maps = all_lkps_maps)

  ## then expand to include both primary and secondary descriptions
  result <- unique(output_codes$destinationId)

  lookup_codes(
    codes = result,
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )
}

get_children_sct_old <- function(codes,
                             standardise_output = TRUE,
                             active_only = FALSE,
                             include_self = TRUE,
                             include_descendants = TRUE,
                             all_lkps_maps = NULL,
                             codes_only = FALSE,
                             preferred_description_only = TRUE,
                             col_filters = getOption("codeminer.col_filters")) {

  # get child codes
  out_codes <- get_relatives_sct_old(
    codes = codes,
    relationship = "116680003",
    relationship_direction = "child",
    active_only = active_only,
    recursive = include_descendants,
    all_lkps_maps = all_lkps_maps
  )

  out_codes <- out_codes$code

  create_db_connection(all_lkps_maps)

  if (!include_self) {
    out_codes <- subset(out_codes,
                        !out_codes %in% codes)
  }

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = "sct")

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = lkp_table)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  # determine description column for lookup sheet
  description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "description_col"
    )

  # get descriptions
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(.data[[code_col]] %in% !!out_codes)

  # adapt output according to user input
  if (active_only) {
    inactive_codes <- result %>%
      dplyr::filter(.data[["active"]] == "0") %>%
      dplyr::pull(.data[[code_col]])

    if (!rlang::is_empty(inactive_codes)) {
      warning(paste0(length(inactive_codes),
                     " inactive codes returned from `sct_description` table, despite being marked as active in `sct_relationship` table, including: ",
                     paste(utils::head(inactive_codes),
                           sep = "",
                           collapse = ", ")))
    }
  }

  # collect and return result
  result <- dplyr::collect(result)

  ## then expand to include both primary and secondary descriptions
  result <- lookup_codes(
    codes = unique(result[[code_col]]),
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = preferred_description_only,
    standardise_output = standardise_output,
    col_filters = col_filters,
    unrecognised_codes = "error",
    .return_unrecognised_codes = FALSE
  )

  if (codes_only) {
    if (standardise_output) {
      return(result$code)
    } else {
      return(result[[code_col]])
    }
  } else {
    return(result)
  }
}

#' Get related SNOMED codes
#'
#' Low level function for querying related SNOMED codes.
#'
#' @param codes Character vector of SNOMED codes
#' @param relationship Character vector of SNOMED codes defining the types of
#'   relationship. If `NULL` (default), returns all types of relationship.
#' @param relationship_direction Either 'child' (default) or 'parent'.
#' @param recursive If `TRUE` (default), will recursively search for related
#'   codes e.g. find all descendants code instead of just the immediate child
#'   codes.
#' @param active_only If `FALSE` (default), return all relationships, even if
#'   currently inactive.
#' @inheritParams lookup_codes
#' @family Clinical code lookups and mappings
#' @name get_relatives_sct_old
#' @noRd
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' # get children codes for "269823000" ("Hemoglobin A1C - ...")
#' get_relatives_sct_old(
#'   codes = "269823000",
#'   relationship = "116680003",
#'   relationship_direction = "child"
#'   )
#'
#' # get parent codes
#' get_relatives_sct_old(
#'   codes = "269823000",
#'   relationship = "116680003",
#'   relationship_direction = "parent"
#'   )
#'
#' # get all codes for which "386868003" ("Bisoprolol (substance)") is an attribute.
#' # Note, includes "293967003" ("Allergy to bisoprolol (finding)")
#' get_relatives_sct_old(
#'   codes = "386868003",
#'   relationship = NULL,
#'   recursive = FALSE,
#'   relationship_direction = "child"
#'   )
#'
#' # more results (i.e. bisoprolol-containing medications) are returned with `recursive = TRUE`
#' get_relatives_sct_old(
#'   codes = "386868003",
#'   relationship = NULL,
#'   recursive = TRUE,
#'   relationship_direction = "child"
#'   )
#' }
get_relatives_sct_old <- function(codes,
                              relationship = NULL,
                              relationship_direction = "child",
                              recursive = TRUE,
                              active_only = FALSE,
                              col_filters = getOption("codeminer.col_filters"),
                              all_lkps_maps = NULL) {

  result <- get_relatives_attributes_sct(codes = codes,
                               relationship = relationship,
                               relationship_direction = relationship_direction,
                               recursive = recursive,
                               active_only = active_only,
                               all_lkps_maps = all_lkps_maps)

  lookup_codes(
    codes = result$code,
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = "error",
    col_filters = col_filters,
    .return_unrecognised_codes = FALSE
  )
}

#' Get attributes for related SNOMED codes
#'
#' Low level function for querying related SNOMED codes, returning attributes
#' types.
#'
#' @param codes Character vector of SNOMED codes
#' @param relationship Character vector of SNOMED codes defining the types of
#'   relationship. If `NULL` (default), returns all types of relationship.
#' @param relationship_direction Either 'child' (default) or 'parent'.
#' @param recursive If `TRUE` (default), will recursively search for related
#'   codes e.g. find all descendants code instead of just the immediate child
#'   codes.
#' @param active_only If `FALSE` (default), return all relationships, even if
#'   currently inactive.
#' @inheritParams lookup_codes
#' @family Clinical code lookups and mappings
#' @name get_attributes_sct
#' @export
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' # get children codes for "269823000" ("Hemoglobin A1C - ...")
#' get_attributes_sct(
#'   codes = "269823000",
#'   relationship = "116680003",
#'   relationship_direction = "child"
#'   )
#'
#' # get parent codes
#' get_attributes_sct(
#'   codes = "269823000",
#'   relationship = "116680003",
#'   relationship_direction = "parent"
#'   )
#'
#' # get all codes for which "386868003" ("Bisoprolol (substance)") is an attribute.
#' # Note, includes "293967003" ("Allergy to bisoprolol (finding)")
#' get_attributes_sct(
#'   codes = "386868003",
#'   relationship = NULL,
#'   recursive = FALSE,
#'   relationship_direction = "child"
#'   )
#'
#' # more results (i.e. bisoprolol-containing medications) are returned with `recursive = TRUE`
#' get_attributes_sct(
#'   codes = "386868003",
#'   relationship = NULL,
#'   recursive = TRUE,
#'   relationship_direction = "child"
#'   )
#' }
get_attributes_sct_old <- function(codes,
                              relationship = NULL,
                              relationship_direction = "child",
                              recursive = TRUE,
                              active_only = FALSE,
                              col_filters = getOption("codeminer.col_filters"),
                              all_lkps_maps = NULL) {

  result <- get_relatives_attributes_sct(codes = codes,
                                         relationship = relationship,
                                         relationship_direction = relationship_direction,
                                         recursive = recursive,
                                         active_only = active_only,
                                         all_lkps_maps = all_lkps_maps)

  result <- result %>%
    dplyr::filter(!is.na(.data[["typeId"]]))

  lookup_codes(
    codes = result$typeId,
    code_type = "sct",
    all_lkps_maps = all_lkps_maps,
    preferred_description_only = TRUE,
    standardise_output = TRUE,
    unrecognised_codes = "error",
    col_filters = col_filters,
    .return_unrecognised_codes = FALSE
  )
}

# #' @rdname get_relatives_sct
# #' @export
# RELATIVES <- get_relatives_sct_old

#' Map clinical codes from one coding system to another
#'
#' Uses the code mapping file provided by UK Biobank
#' (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}).
#'
#' The values for arguments \code{from} and \code{to} must be one of
#' \code{read2}, \code{read3}, \code{icd9}, \code{icd10}, \code{bnf},
#' \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#'
#' @param codes A character vector of codes to be mapped.
#' @param from Coding system that \code{codes} belong to.
#' @param to Coding system to map \code{codes} to.
#' @param unrecognised_codes Either 'error' (default) or 'warning'. If any input
#'   `codes` are unrecognised for the coding system being mapped from, then
#'   either an error or warning will be raised.
#' @param reverse_mapping If 'error' (default), an error raised if attempting to
#'   map between coding systems for which a mapping table does not exist. If
#'   'warning', will raise a warning and attempt to use an existing mapping
#'   table in the opposite direction (for example, a mapping from ICD10 to Read
#'   3 would be attempted using the Read 3-to-ICD10 mapping table).
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns from the relevant mapping table. Note
#'   that this may or may not include code descriptions.
#' @inheritParams get_child_codes
#' @inheritParams lookup_codes
#'
#' @name map_codes
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # map codes from Read 2 to ICD10
#' map_codes(
#'   codes = "G20..",
#'   from = "read2",
#'   to = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
map_codes <- function(codes,
                      to = getOption("codeminer.map_to"),
                      from = getOption("codeminer.map_from"),
                      all_lkps_maps = NULL,
                      codes_only = FALSE,
                      standardise_output = TRUE,
                      unrecognised_codes = getOption("codeminer.unrecognised_codes_mapped"),
                      preferred_description_only = TRUE,
                      reverse_mapping = getOption("codeminer.reverse_mapping"),
                      col_filters = getOption("codeminer.col_filters")) {

  # TODO - create df and string methods; validate codes df
  if (is.data.frame(codes)) {
    from <- unique(codes$code_type)
    codes <- codes$code
  }

  # validate args
  check_codes(codes)

  if (length(codes) == 1) {
    codes <- codes_string_to_vector(codes)
  }

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'"
  )

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`"
  )

  # check mapping args and get required details - mapping_table, from_col and
  # to_col
  mapping_params <- check_mapping_args(
    from = from,
    to = to,
    reverse_mapping = reverse_mapping
  )

  from_col <- mapping_params$from_col
  to_col <- mapping_params$to_col
  mapping_table <- mapping_params$mapping_table

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = mapping_table)

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_value_for_mapping_sheet(
      mapping_table = mapping_table,
      value = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "preferred_code"
      )
  }

  # do mapping
  result <- all_lkps_maps[[mapping_table]] %>%
    dplyr::filter(.data[[from_col]] %in% codes) %>%
    dplyr::filter(!is.na(.data[[to_col]])) %>%
    dplyr::collect()

  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = mapping_table,
      col_filters = col_filters
    )
  }

  # check for unrecognised codes
  missing_codes <- subset(codes, !codes %in% result[[from_col]])

  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = mapping_table,
    code_type = from
  )

  # return result
  if (nrow(result) == 0) {
    message("No codes found after mapping.")
    return(result)
  } else {
    # return either unique codes only, or df including descriptions
    if (codes_only) {
      result <- unique(result[[to_col]])

      return(result)
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
      # `standardise_output` is `TRUE`

      codes <- unique(result[[to_col]])

      return(
        lookup_codes(
          codes = codes,
          code_type = to,
          all_lkps_maps = all_lkps_maps,
          preferred_description_only = preferred_description_only,
          unrecognised_codes = unrecognised_codes
        )
      )
    } else {
      return(result)
    }
  }
}

#' @rdname map_codes
#' @export
MAP <- map_codes

#' Get a 'from-to' mapping data frame
#'
#' Returns a data frame with 'from' and 'to' columns for a specified pair of
#' coding systems.
#'
#' @param from A clinical coding system to map from.
#' @param to A clinical coding system to map to.
#' @inheritParams get_child_codes
#' @inheritParams map_codes
#' @param rename_from_to Optionally supply a named vector to rename the 'from'
#'   and 'to' columns. For example `c(from = "original_codes", to =
#'   "new_codes")`. By default, the columns will be named using the values for
#'   `from` and `to` arguments.
#' @param na.rm If `TRUE` (default), remove any rows with `NA` from the returned
#'   mapping data frame. The mapping tables may sometimes include `NA` values to
#'   explicitly show which 'from' codes have not been mapped.
#'
#' @return A data frame with column names 'from' and 'to'.
#' @export
#'
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # get mapping data frame for Read 2 to ICD10
#' get_mapping_df(
#'   from = "read3",
#'   to = "icd10",
#'   all_lkps_maps = all_lkps_maps_dummy
#' )
get_mapping_df <- function(to = getOption("codeminer.map_to"),
                           from = getOption("codeminer.map_from"),
                           all_lkps_maps = NULL,
                           rename_from_to = NULL,
                           na.rm = TRUE,
                           reverse_mapping = getOption("codeminer.reverse_mapping"),
                           col_filters = getOption("codeminer.col_filters")) {
  # validate args

  # get mapping sheet, from and to cols check mapping args and get required
  # details - mapping_table, from_col and to_col
  mapping_params <- check_mapping_args(
    from = from,
    to = to,
    reverse_mapping = reverse_mapping
  )

  from_col <- mapping_params$from_col
  to_col <- mapping_params$to_col
  mapping_table <- mapping_params$mapping_table

  # rename_from_to
  rename_from_to_error_msg <-
    "Error! `rename_from_to` should be a named character vector of length 2, with names 'from' and 'to'"

  if (!is.null(rename_from_to)) {
    assertthat::assert_that(is.character(rename_from_to) &&
                              (length(rename_from_to) == 2) &&
                              all(c("from", "to") %in% names(rename_from_to)),
                            msg = rename_from_to_error_msg
    )
  }

  create_db_connection(all_lkps_maps)

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = mapping_table)

  # get just distinct combinations of from_col and to_col for mapping_table
  from_to_cols <- c(
    from_col,
    to_col
  )

  result <- all_lkps_maps[[mapping_table]] %>%
    dplyr::collect()


  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = mapping_table,
      col_filters = col_filters
    )
  }

  # keep required columns only
  result <- result %>%
    dplyr::select(tidyselect::all_of(from_to_cols))

  # remove rows with `NA` values
  if (na.rm) {
    result <- tidyr::drop_na(result)
  }

  # distinct rows only (e.g. read 2 'J5310' maps to both primary and secondary
  # descriptions for read 3 'J5311')
  result <- result %>%
    dplyr::distinct(dplyr::across(tidyselect::everything()),
                    .keep_all = TRUE
    )

  # rename
  if (!is.null(rename_from_to)) {
    new_from_to_cols <- c(
      rename_from_to["from"],
      rename_from_to["to"]
    )
  } else {
    new_from_to_cols <- c(
      from,
      to
    )
  }

  result <- result %>%
    ukbwranglr:::rename_cols(
      old_colnames = from_to_cols,
      new_colnames = new_from_to_cols
    )

  return(result)
}

# Utilities ---------------------------------------------------------------

#' Reformat ICD-10 codes
#'
#' The lookup sheet in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UKB resource 592}
#' for ICD-10 ("icd10_lkp") has a column called `ALT_CODE`, which is an
#' alternative format for ICD-10 codes. This is the format used in the mapping
#' sheets for this resource, as well as in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270}{Field ID 41270}.
#' *Note however that in contrast to the ICD10 lookup and mapping sheets,
#' undivided 3 character codes do not have an 'X' appended in the UK Biobank
#' dataset*. This function converts from one format to the other, with the
#' option to strip the final 'X' from undivided 3 character codes if converting
#' to `ALT_CODE` format.
#'
#' @param icd10_codes character vector of ICD-10 codes
#' @param input_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @param output_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @inheritParams get_child_codes
#' @inheritParams lookup_codes
#' @param strip_x If `TRUE` and converting to `ALT_CODE` format, 'X' is removed
#'   from the end of undivided 3 character codes (default is `FALSE`).
#'
#' @return character vector of ICD-10 codes, reformatted as specified by
#'   \code{output_icd10_format}.
#' @export
#' @family Clinical code lookups and mappings
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # reformat from ICD10_CODE to ALT_CODE
#' reformat_icd10_codes(
#'   icd10_codes = c("E10.9"),
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   input_icd10_format = "ICD10_CODE",
#'   output_icd10_format = "ALT_CODE"
#' )
#'
#' # reformat from ALT_CODE to ICD10_CODE
#' reformat_icd10_codes(
#'   icd10_codes = c("E109"),
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   input_icd10_format = "ALT_CODE",
#'   output_icd10_format = "ICD10_CODE"
#' )
reformat_icd10_codes <- function(icd10_codes,
                                 all_lkps_maps = NULL,
                                 input_icd10_format = "ICD10_CODE",
                                 output_icd10_format = "ALT_CODE",
                                 unrecognised_codes = "error",
                                 strip_x = FALSE) {
  # validate args
  match.arg(
    arg = input_icd10_format,
    choices = c("ICD10_CODE", "ALT_CODE")
  )

  match.arg(
    arg = output_icd10_format,
    choices = c("ICD10_CODE", "ALT_CODE")
  )


  assertthat::assert_that(input_icd10_format != output_icd10_format,
    msg = "Error for `reformat_icd10_codes()`! Input and output icd10 formats cannot be the same"
  )

  if (strip_x & (output_icd10_format == "ICD10_CODE")) {
    stop("`strip_x` can only be `TRUE` if `output_icd10_format` is 'ALT_CODE'")
  }

  create_db_connection(all_lkps_maps)

  validate_all_lkps_maps()

  icd10_mapping_df <- all_lkps_maps$icd10_lkp %>%
    dplyr::filter(.data[[input_icd10_format]] %in% icd10_codes) %>%
    dplyr::select(tidyselect::all_of(c(
      "ICD10_CODE",
      "ALT_CODE"
    ))) %>%
    dplyr::collect()

  # handle any unrecognised codes
  missing_codes <-
    subset(
      icd10_codes,
      !icd10_codes %in% icd10_mapping_df[[input_icd10_format]]
    ) %>%
    unique()

  handle_unrecognised_codes(
    unrecognised_codes = unrecognised_codes,
    missing_codes = missing_codes,
    table_name = "icd10_lkp",
    code_type = input_icd10_format
  )

  # Note: some ICD10_CODE values have multiple associated ALT_CODEs - these
  # include a modifier description in `MODIFIER_5` (e.g. ICD-10 codes M00,
  # M77, M07, M72, M65, S52 or S72). The number of output codes may therefore
  # be larger/smaller than the input number. Raise informative message if this
  # is is the case.

  non_unique_icd10_codes <- icd10_mapping_df %>%
    dplyr::count(.data[["ICD10_CODE"]]) %>%
    dplyr::filter(.data[["n"]] > 1) %>%
    dplyr::pull(.data[["ICD10_CODE"]])

  if (length(non_unique_icd10_codes) > 0) {
    input_icd10_not_1_to_1_mapping <- icd10_mapping_df %>%
      dplyr::filter(.data[["ICD10_CODE"]] %in% non_unique_icd10_codes) %>%
      dplyr::pull(.data[[input_icd10_format]]) %>%
      unique()

    more_or_fewer_returned_codes <-
      ifelse(input_icd10_format == "ICD10_CODE",
        yes = "*more*",
        no = "*fewer*"
      )

    message(
      "The following ",
      length(input_icd10_not_1_to_1_mapping),
      " input ICD10 codes do not have a 1-to-1 ICD10_CODE-to-ALT_CODE mapping: '",
      stringr::str_c(
        input_icd10_not_1_to_1_mapping,
        sep = "",
        collapse = "', '"
      ),
      "'. There will therefore be ",
      more_or_fewer_returned_codes,
      " output than input codes"
    )
  }

  # get requested icd10 format
  result <- unique(icd10_mapping_df[[output_icd10_format]])

  # optionally remove appended 'X' for undivided 3 character codes in `ALT_CODE` format
  if (strip_x & (output_icd10_format == "ALT_CODE")) {
    message("Removing 'X' from any undivided 3 character ICD10 codes")
    result <- stringr::str_remove(
      result,
      "X$"
    )
  }

  # return result
  return(result)
}

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
  get_col_filters(defaults_only = TRUE,
                  selected_table = NULL)
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
  if(md) {
    schema <- "db"
  }

  tables <- DBI::dbListTables(con, schema = md) |>
    setdiff(c("shared_with_me", "databases", "owned_shares"))

  tables_named <- purrr::set_names(tables)

  if (md) {
    result <-  purrr::map(
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
#' @inheritParams lookup_codes
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
    lookup_codes(code_type = "sct",
                 all_lkps_maps = all_lkps_maps,
                 preferred_description_only = TRUE,
                 standardise_output = TRUE,
                 unrecognised_codes = "error",
                 col_filters = NULL,
                 .return_unrecognised_codes = FALSE)
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
filter_sct_relationship <- function(codes = NULL,
                                    sourceId_filter = NULL,
                                    destinationId_filter = NULL,
                                    typeId_filter = NULL,
                                    active_only = TRUE,
                                    recursive = FALSE,
                                    all_lkps_maps = NULL) {

  # validate args
  stopifnot(!is.null(sourceId_filter) | !is.null(destinationId_filter) | !is.null(typeId_filter))

  # either `sourceId_filter` or `destinationId_filter` should be `NULL` for
  # recursion
  if (recursive) {
    stopifnot(is.null(sourceId_filter) | is.null(destinationId_filter))
  }

  create_db_connection(all_lkps_maps)

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = "sct_relationship")

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
    dplyr::select(tidyselect::all_of(c("destinationId", "sourceId", "typeId"))) %>%
    dplyr::collect()

  result <- dplyr::bind_rows(codes,
                             related_codes) %>%
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
get_relatives_attributes_sct <- function(codes = NULL,
                                         relationship = NULL,
                                         relationship_direction = "child",
                                         recursive = TRUE,
                                         active_only = FALSE,
                                         all_lkps_maps = NULL) {

  # Note: does not check whether `codes` or `relationship` exist or not

  # Note: returns self and relatives

  # validate args
  match.arg(relationship_direction,
            choices = c("child", "parent"))

  ## determine relationship direction
  filter_col <- switch(relationship_direction,
                       child = "destinationId",
                       parent = "sourceId")

  return_col <- switch(relationship_direction,
                       child = "sourceId",
                       parent = "destinationId")

  stopifnot(!is.null(codes) | !is.null(relationship))

  # codes - can be character vector or data frame
  if (!is.null(codes)) {
    if (is.character(codes)) {
      check_codes(codes)

      if (length(codes) == 1) {
        codes <- codes_string_to_vector(codes)
      }

      codes <- tibble::tibble(code = codes,
                              typeId = NA_character_)
    }
  }

  if (!is.null(relationship)) {
    check_codes(relationship)

    if (length(relationship) == 1) {
      relationship <- codes_string_to_vector(relationship)
    }
  }

  create_db_connection(all_lkps_maps)

  # TO DELETE - too slow (maybe)

  # # validate relationship
  # assertthat::assert_that(relationship %in% dplyr::pull(all_lkps_maps$sct_relationship, .data[["typeId"]]),
  #                         msg = paste0("Unrecognised relationship: '",
  #                                      relationship,
  #                                      "'"))

  check_table_exists_in_all_lkps_maps(all_lkps_maps = all_lkps_maps,
                                      table_name = "sct_relationship")

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

  result <- dplyr::bind_rows(codes,
                             related_codes) %>%
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
get_col_filters <- function(defaults_only = TRUE,
                            selected_table = NULL) {

  # validate args
  stopifnot(is.logical(defaults_only))

  stopifnot(
    selected_table %in% CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table |
      selected_table %in% CLINICAL_CODE_MAPPINGS_MAP$mapping_table
  )

  # get filter_cols
  result <- list(lookup = CODE_TYPE_TO_LKP_TABLE_MAP %>%
                   dplyr::rename("table" = "lkp_table"),
                 map = CLINICAL_CODE_MAPPINGS_MAP %>%
                   dplyr::rename("table" = "mapping_table")) %>%
    dplyr::bind_rows(.id = "lookup_map") %>%
    dplyr::select(tidyselect::all_of(c("table", "filter_cols"))) %>%
    dplyr::filter(!is.na(.data[["filter_cols"]]))

  result <- rlang::set_names(result$filter_cols,
                             nm = result$table) %>%
    purrr::map(\(x) x[[1]])

  # get either all values, or default selections only for filter_cols
  if (defaults_only) {
    result <- result %>%
      purrr::map(\(x) x %>%
                   purrr::map(\(x) subset(x,
                                          stringr::str_detect(x, "\\*"))))
  }

  # remove '*' (these are used to indicate default choices)
  result <- result %>%
    purrr::map(\(x) x %>%
                 purrr::map(stringr::str_remove_all,
                            pattern = "\\*"))

  # select single table, if requested
  if (!is.null(selected_table)) {
    result <- result[[selected_table]]
  }

  result
}

#' Get codes that start with...
#'
#' This is case \emph{sensitive} (important for read codes especially).
#'
#' @param escape_dot If `TRUE`, escape any '.' characters in `codes`. Default is
#'   `FALSE`
#' @inheritParams lookup_codes
#' @inheritParams get_child_codes
#' @noRd
#' @family Clinical code lookups and mappings
codes_starting_with <- function(codes,
                                code_type = getOption("codeminer.code_type"),
                                all_lkps_maps = NULL,
                                codes_only = FALSE,
                                preferred_description_only = TRUE,
                                standardise_output = TRUE,
                                col_filters = getOption("codeminer.col_filters"),
                                escape_dot = FALSE) {
  # validate args
  match.arg(
    arg = code_type,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )

  create_db_connection(all_lkps_maps)

  check_codes(codes)

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'"
  )

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`"
  )

  # TODO check all sheets are present
  validate_all_lkps_maps()

  # determine relevant lookup sheet
  lkp_table <- get_lookup_sheet(code_type = code_type)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(
    lookup_sheet = lkp_table,
    column = "code_col"
  )

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <-
    get_col_for_lookup_sheet(
      lookup_sheet = lkp_table,
      column = "preferred_synonym_col"
    )

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <-
      get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_table)
  }

  # reformat codes - prefix with anchor '^', optionally escape '.'
  codes <- paste0("^", codes)

  if (escape_dot) {
    codes <-
      stringr::str_replace_all(codes, pattern = "\\.", replacement = "\\\\.")
  }

  # combine into single string, separated by "|"
  codes <- stringr::str_c(codes, sep = "", collapse = "|")

  # get children (filter for codes which match ANY of those in `codes` arg)
  result <- all_lkps_maps[[lkp_table]] %>%
    dplyr::filter(stringr::str_detect(.data[[code_col]],
                                      pattern = codes)) %>%
    dplyr::collect()

  # filter on `col_filters` parameters
  if (!is.null(col_filters)) {
    result <- filter_cols(
      df = result,
      df_name = lkp_table,
      col_filters = col_filters
    )
  }

  # filter for preferred code descriptions only if requested
  if (preferred_description_only &
      !is.na(preferred_description_col)) {
    result <- result %>%
      dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
  }

  # return result
  if (nrow(result) == 0) {
    message("No matching codes found.")
    return(result)
  } else {
    # return either unique codes only, or df including code descriptions
    if (codes_only) {
      return(unique(result[[code_col]]))
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
      # `standardise_output` is `TRUE`
      codes <- unique(result[[code_col]])

      return(
        lookup_codes(
          codes = codes,
          code_type = code_type,
          all_lkps_maps = all_lkps_maps,
          preferred_description_only = preferred_description_only,
          unrecognised_codes = "error"
        )
      )
    } else {
      return(result)
    }
  }
}

#' Helper function for \code{\link{map_codes}}
#'
#' Returns name of the appropriate mapping sheet from the UKB code mappings
#' excel file (resource 592) for mapping from one clinical coding system to
#' another.
#'
#' @param from character
#' @param to character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_from_to_mapping_sheet <- function(from, to) {
  CLINICAL_CODE_MAPPINGS_MAP[(CLINICAL_CODE_MAPPINGS_MAP[["from"]] == from &
    CLINICAL_CODE_MAPPINGS_MAP[["to"]] == to), ][["mapping_table"]]
}

#' Helper function for \code{\link{map_codes}}
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
get_value_for_mapping_sheet <- function(mapping_table,
                                        value) {
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
  CLINICAL_CODE_MAPPINGS_MAP[CLINICAL_CODE_MAPPINGS_MAP[["mapping_table"]] == mapping_table, ][[value]]
}

#' Get name of lookup sheet for a clinical code system
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param code_type character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_lookup_sheet <- function(code_type) {
  # validate args
  rlang::arg_match(code_type,
    values = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )

  # get lookup sheet
  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["code"]] == code_type) %>%
    .$lkp_table
}

#' Get name of code, description or preferred synonym column for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param lookup_sheet character
#' @param column character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_col_for_lookup_sheet <- function(lookup_sheet,
                                     column) {
  # validate args
  match.arg(
    arg = lookup_sheet,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table
  )

  match.arg(
    arg = column,
    choices = c("code_col", "description_col", "preferred_synonym_col")
  )

  # get column name for lookup sheet
  CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["lkp_table"]] == lookup_sheet) %>%
    .[[column]]
}

#' Get preferred description code for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{codes_starting_with}}
#'
#' @param lookup_sheet character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_preferred_description_code_for_lookup_sheet <-
  function(lookup_sheet) {
    # validate args
    match.arg(
      arg = lookup_sheet,
      choices = CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table
    )

    # get preferred description code for lookup sheet
    CODE_TYPE_TO_LKP_TABLE_MAP %>%
      dplyr::filter(.data[["lkp_table"]] == lookup_sheet) %>%
      .[["preferred_code"]]
  }

#' Reformat a dataframe of clinical codes to work with
#' \code{\link[ukbwranglr]{extract_phenotypes}}
#'
#' A utility function that helps reformat the output from \code{\link{map_codes}}
#' or \code{\link{lookup_codes}} to work with
#' \code{\link[ukbwranglr]{extract_phenotypes}}. See also output
#' from \code{\link[ukbwranglr]{example_clinical_codes}} for an example of
#' the format that this function will output.
#'
#' @param standardised_codelist a data frame with column names "code",
#'   "description", "code_type".
#' @param code_type character (scalar). The clinical code type e.g. "read2"
#' @param disease character (scalar), e.g. "Secondary polycythaemia"
#' @param disease_category character (scalar). The subcategory of \code{disease}
#'   that these codes belong to e.g. "Diagnosis of Secondary polycythaemia".
#' @param author character (scalar), e.g. "caliber".
#'
#' @return A data frame with the following column names: 'disease',
#'   'description', 'category', 'code_type', 'code' and 'author'.
#' @noRd
reformat_standardised_codelist <- function(standardised_codelist,
                                           code_type = getOption("codeminer.code_type"),
                                           disease,
                                           disease_category,
                                           author) {
  # validate args
  assertthat::assert_that(is.data.frame(standardised_codelist),
    msg = "Error! standardised_codelist must be a data frame (or tibble/data table"
  )

  assertthat::is.string(code_type)
  assertthat::is.string(disease)
  assertthat::is.string(disease_category)
  assertthat::is.string(author)

  match.arg(code_type,
    choices = ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding
  )

  assertthat::assert_that(all(
    names(standardised_codelist) == c("code", "description", "code_type")
  ),
  msg = "Error! `standardised_codelist` must be a data frame with the following headings: 'code', 'description', 'code_type'"
  )

  assertthat::assert_that(
    all(
      standardised_codelist$code_type %in% unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding)
    ),
    msg = paste0(
      "Error! `standardised_codelist$code_type` contains unrecognised code types. Recognised code types: ",
      stringr::str_c(
        unique(ukbwranglr:::CLINICAL_EVENTS_SOURCES$data_coding),
        sep = "",
        collapse = ", "
      )
    )
  )

  # reformat to work with `extract_phenotypes()`
  standardised_codelist <- standardised_codelist %>%
    dplyr::mutate(
      "code_type" = code_type,
      "disease" = disease,
      "category" = disease_category,
      "author" = author,
    ) %>%
    dplyr::select(tidyselect::all_of(
      c(
        "disease",
        "description",
        "category",
        "code_type",
        "code",
        "author"
      )
    ))

  return(standardised_codelist)
}

standardise_output_fn <-
  function(df,
           lkp_table,
           code_col,
           description_col,
           code_type) {
    names(df)[which(names(df) == code_col)] <- "code"
    names(df)[which(names(df) == description_col)] <- "description"

    # return code, description and code_type cols only
    df <- df[c("code", "description")]
    df[["code_type"]] <- code_type

    return(df)
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
filter_cols <- function(df,
                        df_name,
                        col_filters = NULL) {
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

  assertthat::assert_that(sum(!col_filters_item_types) == 0,
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

#' Remove or extract appended ICD10 letters
#'
#' @param icd10_codes Character vector of ICD10 codes
#' @param rm_extract Either 'rm' (remove 'D'/'X'/'A' from end of codes) or
#'   'extract' (return these characters only, or `NA` if the code does not end
#'   with letter characters).
#'
#' @return Character vector
#' @noRd
#' @examples
#' rm_or_extract_appended_icd10_letters(c(
#'   "A00",
#'   "A408",
#'   "A390D",
#'   "A38X",
#'   "G01XA"
#' ))
rm_or_extract_appended_icd10_dxa <- function(icd10_codes,
                                             keep_x = TRUE,
                                             rm_extract = "rm") {
  # validate args
  match.arg(rm_extract,
    choices = c("rm", "extract")
  )

  stopifnot(is.character(icd10_codes))

  # either remove or extract appended 'D'/'X'/'A' from `icd10_codes`
  pattern <- ifelse(keep_x,
    yes = "[D|A]*$",
    no = "[D|X|A]*$"
  )

  switch(rm_extract,
    rm = stringr::str_remove(icd10_codes,
      pattern = pattern
    ),
    extract = stringr::str_extract(icd10_codes,
      pattern = pattern
    ) %>%
      ifelse(. == "",
        yes = NA_character_,
        no = .
      )
  )
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
get_icd10_code_alt_code_x_map <- function(icd10_lkp,
                                          undivided_3char_only = FALSE,
                                          as_named_list = NULL) {

  # validate args
  match.arg(as_named_list,
    choices = c("names_no_x", "names_with_x")
  )

  # make mapping df
  icd10_lkp_alt_x_map <- icd10_lkp %>%
    dplyr::select(tidyselect::all_of("ALT_CODE")) %>%
    dplyr::filter(!is.na(.data[["ALT_CODE"]])) %>%
    dplyr::collect() %>%
    dplyr::mutate("ALT_CODE_minus_x" = stringr::str_remove(.data[["ALT_CODE"]],
      pattern = "X$"
    ))

  # check that all codes are unique
  stopifnot(
    length(icd10_lkp_alt_x_map$ALT_CODE) == dplyr::n_distinct(icd10_lkp_alt_x_map$ALT_CODE)
  )
  stopifnot(
    length(icd10_lkp_alt_x_map$ALT_CODE_minus_x) == dplyr::n_distinct(icd10_lkp_alt_x_map$ALT_CODE_minus_x)
  )

  # check that all codes are the same comparing `ALT_CODE` and
  # `ALT_CODE_minus_x`, after removing the X in `ALT_CODE`
  stopifnot(all(
    icd10_lkp_alt_x_map$ALT_CODE_minus_x == stringr::str_remove(icd10_lkp_alt_x_map$ALT_CODE,
      pattern = "X$"
    )
  ))

  if (undivided_3char_only) {
    icd10_lkp_alt_x_map <- icd10_lkp_alt_x_map %>%
      dplyr::filter(.data[["ALT_CODE_minus_x"]] != .data[["ALT_CODE"]])
  }

  # convert to named list
  if (!is.null(as_named_list)) {
    icd10_lkp_alt_x_map <- switch(as_named_list,
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
get_icd10_code_range <- function(start_icd10_code,
                                 end_icd10_code,
                                 icd10_lkp) {
  # validate args
  stopifnot(is.character(start_icd10_code))
  stopifnot(is.character(end_icd10_code))

  assertthat::assert_that(
    stringr::str_length(stringr::str_remove(
      start_icd10_code,
      "X$"
    )) == stringr::str_length(stringr::str_remove(
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

  pattern <- stringr::str_c(paste0("^", result),
    sep = "",
    collapse = "|"
  )

  # expand (e.g. for 'A80-A81', at this stage all 'A80' should be present
  # ('A800-A809'), but for 'A81', only 'A81' wil be present - needs expanding
  # to 'A801-A819')
  result <- icd10_lkp %>%
    dplyr::filter(stringr::str_detect(.data[["ALT_CODE"]],
      pattern = pattern
    )) %>%
    dplyr::pull(.data[["ALT_CODE"]])

  return(result)
}

#' Expand a `read_v2_icd10` data frame containing code ranges in the
#' `icd10_code` column
#'
#' To be used within `reformat_read_v2_icd10()`.
#'
#' @param read_v2_icd10 Data frame
#' @param icd10_lkp Data frame
#' @param icd10_lkp_alt_x_map A named list of undivided 3 character ICD10 codes,
#'   where the names do not have an 'X' appended and the values do.
#'
#' @return A data frame
#' @noRd
expand_icd10_ranges <- function(read_v2_icd10,
                                icd10_lkp,
                                icd10_lkp_alt_x_map) {
  # validate args
  assertthat::assert_that(all(
    c(
      "read_code",
      "icd10_code",
      "icd10_code_def"
    ) %in% names(read_v2_icd10)
  ),
  msg = "Unexpected column names in `read_v2_icd10`"
  )

  # separate by '-'
  read_v2_icd10 <- read_v2_icd10 %>%
    tidyr::separate(
      .data[["icd10_code"]],
      into = c("start_icd10_code", "end_icd10_code"),
      sep = "-",
      remove = FALSE,
      fill = "right"
    )

  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("start_icd10_code" = ifelse(is.na(.data[["end_icd10_code"]]),
      yes = NA_character_,
      no = .data[["start_icd10_code"]]
    ))

  # strip any appended 'D/X/A' (last character(s) e.g. 'A89X' and 'A170D' become
  # 'A89' and 'A170'. 'G01XA' would become 'G01', although note that this code
  # does not appear together with a '-')
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(c(
        "start_icd10_code",
        "end_icd10_code"
      )),
      ~ rm_or_extract_appended_icd10_dxa(
        icd10_codes = .x,
        keep_x = TRUE,
        rm_extract = "rm"
      )
    ))

  # Make sure undivided 3 character ICD10 codes have an 'X' appended
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(c(
        "start_icd10_code",
        "end_icd10_code"
      )),
      ~ dplyr::recode(
        .x,
        !!!icd10_lkp_alt_x_map
      )
    ))

  # expand range
  read_v2_icd10 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(icd10_range_new = ifelse(
      is.na(.data[["start_icd10_code"]]),
      yes = list(NA_character_),
      no = list(
        get_icd10_code_range(
          start_icd10_code = .data[["start_icd10_code"]],
          end_icd10_code = .data[["end_icd10_code"]],
          icd10_lkp = icd10_lkp
        )
      )
    )) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = "icd10_range_new") %>%
    dplyr::mutate("icd10_code" = ifelse(is.na(.data[["icd10_range_new"]]),
      yes = .data[["icd10_code"]],
      no = .data[["icd10_range_new"]]
    )) %>%
    dplyr::select(-tidyselect::all_of(c(
      "start_icd10_code",
      "end_icd10_code",
      "icd10_range_new"
    )))
}

codes_string_to_vector <- function(codes) {
  codes %>%
    stringr::str_split_1(pattern = "\\|") %>%
    stringr::str_remove_all(pattern = "\\n") %>%
    stringr::str_remove(pattern = "<<.*>>") %>%
    stringr::str_trim(side = "both")
}

## Validation helpers ---------------------------

check_codes <- function(codes) {
  assertthat::assert_that(is.character(codes),
                          msg = "Error! `codes` must be a character vector")

  assertthat::assert_that(sum(is.na(codes)) == 0,
                          msg = "Error! `codes` cannot contain `NA` values")
}

check_mapping_args <- function(from,
                               to,
                               reverse_mapping = "error") {

  if (identical(reverse_mapping, NULL)) {
    reverse_mapping <- "error"
  }

  match.arg(reverse_mapping,
            choices = c("error", "warning")
  )

  if (identical(from, NULL)) {
    from <- ""
  }

  if (identical(to, NULL)) {
    to <- ""
  }

  match.arg(
    arg = from,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )
  # choices = CLINICAL_CODE_MAPPINGS_MAP$from)

  match.arg(
    arg = to,
    choices = CODE_TYPE_TO_LKP_TABLE_MAP$code
  )
  # choices = CLINICAL_CODE_MAPPINGS_MAP$to)

  assertthat::assert_that(!identical(from, to),
                          msg = "Error! `from` and `to` args cannot be the same"
  )

  # get appropriate mapping sheet
  swap_mapping_cols <- FALSE
  mapping_table <- get_from_to_mapping_sheet(from = from, to = to)

  # if above returns `character(0)`, try to map the other way
  if (rlang::is_empty(mapping_table)) {
    swap_mapping_cols <- TRUE
    mapping_table <- get_from_to_mapping_sheet(from = to, to = from)
  }

  # if still returns `character(0)`, error
  if (rlang::is_empty(mapping_table)) {
    stop("Error! Invalid (or unavailable) code mapping request")
  } else if (swap_mapping_cols) {
    switch(reverse_mapping,
           error = stop("No mapping sheet available for this request"),
           warning = warning(
             "No mapping sheet available for this request. Attempting to map anyway using: ",
             mapping_table
           )
    )
  }

  # get from_col and to_col column names for mapping sheet
  # swap if appropriate
  if (swap_mapping_cols) {
    from_col <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "to_col"
      )
    to_col <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "from_col"
      )
  } else {
    from_col <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "from_col"
      )
    to_col <-
      get_value_for_mapping_sheet(
        mapping_table = mapping_table,
        value = "to_col"
      )
  }

  # return result
  return(list(
    from_col = from_col,
    to_col = to_col,
    mapping_table = mapping_table
  ))
}

check_all_lkps_maps_path <- function(file_path, md = FALSE) {

  # determine whether using motherduck
  if (md) {
    # connect - requires motherduck extension and `motherduck_token`
    # environmental variable
    con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)
    DBI::dbExecute(con, stringr::str_glue("ATTACH '{file_path}' AS db;"))
  } else {
    # check file exists
    assertthat::assert_that(file.exists(file_path),
                            msg = paste0("Error! No file found at ", file_path))

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

check_rowid_col_present <- function(df,
                                    df_name) {
  assertthat::assert_that(".rowid" %in% names(df),
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
  function(unrecognised_codes,
           missing_codes,
           table_name,
           code_type) {

    if (is.null(unrecognised_codes)) {
      unrecognised_codes <- "error"
    }

    match.arg(unrecognised_codes,
              choices = c("error", "warning"))

    # make sure missing_codes are unique, if not already
    missing_codes <- unique(missing_codes)

    # only display first 25 codes
    if (length(missing_codes) > 25) {
      missing_codes_to_print <- utils::head(missing_codes,
                                            n = 25)
    } else {
      missing_codes_to_print <- missing_codes
    }

    missing_codes_to_print <- stringr::str_c(missing_codes_to_print,
                                             sep = "",
                                             collapse = "', '")

    if (length(missing_codes) > 25) {
      missing_codes_to_print <- paste0(missing_codes_to_print,
                                       " (first 25 only shown)")
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

      switch(unrecognised_codes,
             error = stop(missing_codes_message),
             warning = warning(missing_codes_message))
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
check_codes_exist <- function(codes,
                              lkp_codes,
                              table_name,
                              code_type = getOption("codeminer.code_type"),
                              return_unrecognised_codes = FALSE,
                              unrecognised_codes = "error") {
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

check_table_exists_in_all_lkps_maps <- function(all_lkps_maps,
                                                table_name) {
  assertthat::assert_that(
    table_name %in% names(all_lkps_maps),
    msg = paste0("Required table ",
                 table_name,
                 " not found in `all_lkps_maps`")
  )
}
