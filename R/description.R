#' Search for codes that match a description
#'
#' Returns a data frame with clinical codes that match the provided description.
#' The description can be a regular expression or a string.
#'
#' @param description
#' @inheritParams stringr::regex
#' @inheritParams CODES
#' @param ignore_case If `TRUE` (default), ignore case in `description`.
#' @param codes_only bool. If `TRUE`, return a character vector of
#'   \emph{unique} codes. If `FALSE` (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#'
#' @return The result of [CODES()] for codes that match the description, or a character vector of codes if
#'   `codes_only` is `TRUE`.
#' @export
#' @examples
#' # build dummy all_lkps_maps
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' DESCRIPTION(
#'   reg_expr = "cyst",
#'   code_type = "icd10"
#' )
DESCRIPTION <- function(
  description,
  code_type = getOption("codeminer.code_type"),
  version = "latest",
  ignore_case = TRUE,
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  # validate args
  assertthat::is.string(description)

  assertthat::assert_that(
    !(codes_only & standardise_output),
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
        pattern = tolower(description)
      )) %>%
      dplyr::collect()
  } else {
    # if `ignore_case` is `FALSE`, then same code will work for both data
    # frame/tbl_dbi object
    result <- all_lkps_maps[[lkp_table]] %>%
      dplyr::filter(stringr::str_detect(
        string = .data[[description_col]],
        pattern = description
      )) %>%
      dplyr::collect()
  }

  ## then expand to include both primary and secondary descriptions
  codes <- unique(result[[code_col]])

  codes <- subset(codes, !is.na(codes))

  result <- CODES(
    codes = codes,
    code_type = code_type
    # preferred_description_only = preferred_description_only
  )

  if (codes_only) {
    return(result$code)
  }
  return(result)
}
