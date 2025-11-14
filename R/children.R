# TODO: to be implemented as part of
# https://github.com/codeminer-io/codeminer/issues/44

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
#' @inheritParams CODES
#'
#' @return A data frame
#' @name CHILDREN
#'
#' @seealso [get_children_sct()]
#' @family Clinical code lookups and mappings
#' @examples
#' # TODO
CHILDREN <- function(
  codes,
  code_type = getOption("codeminer.code_type"),
  version = getOption("codeminer.version", default = "latest"),
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  cli::cli_abort("Not implemented yet.")
  # # check codes exist
  # codes <- CODES(
  #   codes = codes,
  #   code_type = code_type,
  #   all_lkps_maps = all_lkps_maps,
  #   preferred_description_only = TRUE,
  #   standardise_output = TRUE,
  #   unrecognised_codes = unrecognised_codes,
  #   col_filters = col_filters,
  #   .return_unrecognised_codes = FALSE
  # )
  # if (!is.null(codes)) {
  #   code_type <- codes$code_type[1]
  #   codes <- codes %>%
  #     dplyr::pull(tidyselect::all_of("code")) %>%
  #     unique()
  # } else {
  #   return(codes)
  # }
  # # get child codes
  # if (code_type == "sct") {
  #   get_children_sct(
  #     codes = codes,
  #     standardise_output = standardise_output,
  #     include_self = TRUE,
  #     include_descendants = TRUE,
  #     all_lkps_maps = all_lkps_maps,
  #     preferred_description_only = preferred_description_only,
  #     col_filters = col_filters
  #   )
  # } else if (
  #   code_type %in%
  #     c(
  #       "bnf",
  #       "icd9",
  #       "icd10",
  #       "read2",
  #       "read2_drugs",
  #       "opcs4",
  #       "phecode"
  #     )
  # ) {
  #   codes_starting_with(
  #     codes = codes,
  #     code_type = code_type,
  #     all_lkps_maps = all_lkps_maps,
  #     codes_only = codes_only,
  #     preferred_description_only = preferred_description_only,
  #     standardise_output = standardise_output,
  #     col_filters = col_filters,
  #     escape_dot = FALSE
  #   )
  # } else {
  #   stop(paste0(
  #     "Currently codeminer is unable to retrieve child codes for ",
  #     code_type
  #   ))
  # }
}

#' Get children for SNOMED codes
#'
#' @param codes Character vector of SNOMED codes.
#' @param standardise_output If `TRUE` (default) return a data frame with columns
#'   'code', 'description' and 'code_type'.
#' @param include_self If `TRUE` (default) include input codes in the result.
#' @param include_descendants If `TRUE` (default) return all descendant codes,
#'   as well as immediate children.
#' @inheritParams CODES
#' @inheritParams CHILDREN
#'
#' @return A dataframe
#' @seealso [CHILDREN()], [get_parents_sct()]
#' @family Clinical code lookups and mappings
#' @export
get_children_sct <- function(
  codes,
  standardise_output = TRUE,
  include_self = TRUE,
  include_descendants = TRUE,
  preferred_description_only = TRUE
) {
  get_relatives_sct(
    codes = codes,
    filter_col = "destinationId",
    return_col = "sourceId",
    typeId = "116680003",
    standardise_output = standardise_output,
    include_self = include_self,
    recursive = include_descendants,
    preferred_description_only = preferred_description_only
  )
}

get_relatives_sct <- function(
  codes = NULL,
  filter_col = "destinationId",
  return_col = "sourceId",
  typeId = "116680003",
  standardise_output = TRUE,
  include_self = TRUE,
  recursive = TRUE,
  all_lkps_maps = NULL,
  preferred_description_only = TRUE,
  col_filters = getOption("codeminer.col_filters")
) {
  match.arg(filter_col, choices = c("sourceId", "destinationId"))

  # set up
  sourceId_filter <- destinationId_filter <- NULL

  if (filter_col == "sourceId") {
    sourceId_filter <- codes
  }

  if (filter_col == "destinationId") {
    destinationId_filter <- codes
  }

  # get codes from relationship table
  input_codes <- filter_sct_relationship(
    codes = NULL,
    sourceId_filter = sourceId_filter,
    destinationId_filter = destinationId_filter,
    typeId_filter = typeId,
    active_only = TRUE,
    recursive = FALSE,
    all_lkps_maps = all_lkps_maps
  )

  sourceId_filter <- destinationId_filter <- NULL

  if (filter_col == "sourceId") {
    sourceId_filter <- unique(input_codes$destinationId)
  }

  if (filter_col == "destinationId") {
    destinationId_filter <- unique(input_codes$sourceId)
  }

  # perform search recursively
  if (recursive) {
    output_codes <- filter_sct_relationship(
      codes = input_codes,
      sourceId_filter = sourceId_filter,
      destinationId_filter = destinationId_filter,
      typeId_filter = typeId,
      active_only = TRUE,
      recursive = recursive,
      all_lkps_maps = all_lkps_maps
    )
  } else {
    output_codes <- input_codes
  }

  # lookup results
  result <- unique(output_codes[[return_col]])

  if (include_self) {
    result <- c(codes, result)
  }

  CODES(
    codes = result,
    code_type = "sct",
    preferred_description_only = preferred_description_only
  )
}
