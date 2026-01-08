#' Search for codes that match a description
#'
#' Returns a data frame with clinical codes that match the provided description pattern.
#'
#' @param pattern The description to search for. See [stringr::str_detect()] for details.
#' @param ignore_case If `TRUE` (default), ignore case in `description`.
#' @param codes_only `logical`. If `TRUE`, return a character vector of
#'   \emph{unique} codes. If `FALSE` (default), return a data frame of all
#'   results including code descriptions (useful for manual validation).
#' @inheritParams CODES
#'
#' @return The result of [CODES()] for codes that match the description, or a character vector of codes if
#'   `codes_only` is `TRUE`.
#'
#' @export
#' @examples
#' # build dummy database
#' create_dummy_database()
#'
#' # lookup ICD10 code descriptions matching 'cyst'
#' DESCRIPTION("cyst", code_type = "ICD-10")
DESCRIPTION <- function(
  pattern,
  code_type = getOption("codeminer.code_type"),
  lookup_version = getOption("codeminer.lookup_version", default = "latest"),
  ignore_case = TRUE,
  codes_only = FALSE,
  preferred_description_only = TRUE
) {
  if (!rlang::is_string(pattern)) {
    codeminer_abort(
      "{.arg pattern} must be a length 1 string, not {typeof(pattern)} with length {length(pattern)}"
    )
  }

  con <- connect_to_db()
  lkp_table <- get_lookup_table(
    con,
    code_type = code_type,
    lookup_version = lookup_version
  )
  code_col <- "code"
  description_col <- "description"

  # Note - it isn't possible to specify `ignore_case` when using dbplyr, so use `tolower()`
  if (ignore_case) {
    filtered <- dplyr::filter(
      lkp_table,
      stringr::str_detect(
        string = tolower(.data[[description_col]]),
        pattern = tolower(pattern)
      )
    )
  } else {
    filtered <- dplyr::filter(
      lkp_table,
      stringr::str_detect(
        string = .data[[description_col]],
        pattern = pattern
      )
    )
  }
  filtered <- dplyr::collect(filtered)

  ## then expand to include both primary and secondary descriptions
  codes <- unique(filtered[[code_col]])
  codes <- codes[!is.na(codes)]

  result <- CODES(
    codes,
    code_type = code_type,
    lookup_version = lookup_version,
    preferred_description_only = preferred_description_only
  )

  if (codes_only) {
    return(result$code)
  }
  return(result)
}
