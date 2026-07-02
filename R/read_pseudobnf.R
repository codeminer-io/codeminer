#' Read NHS BSA BNF code information (pseudo-BNF) into lookup and relationship tables
#'
#' Reads the NHS Business Services Authority (NHSBSA) "BNF Code Information" CSV
#' (the full BNF code hierarchy, commonly called the pseudo-BNF classification)
#' and returns a BNF lookup table plus a parent-child relationship table for use
#' with CodeMiner.
#'
#' The lookup mirrors the shape of the BNF lookup historically derived from UK
#' Biobank resource 592 (one row per code at each level of the BNF hierarchy,
#' with higher-level name columns populated and deeper ones `NA`), so it is a
#' drop-in replacement. Unlike that approach, each level's code is taken directly
#' from the portal's explicit `*_CODE` columns rather than sliced from the
#' presentation code.
#'
#' @param path Path to the NHS BSA BNF code information CSV. Default uses
#'   [get_pseudobnf()] to download the latest release.
#' @param version Character string for the version label. If `NULL` (default),
#'   derived from the file name.
#' @param source Character string for the data source URL or description.
#'
#' @return A named list with element `bnf_lkp`, containing:
#'   * `lookup`: a list with `table` (data frame) and `metadata` (list)
#'   * `relationship`: a list with `table` (data frame) and `metadata` (list)
#'
#' @seealso [add_pseudobnf()], [get_pseudobnf()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_pseudobnf()
#' result <- read_pseudobnf(path)
#' result$bnf_lkp$lookup$table
#' result$bnf_lkp$relationship$table
#' }
read_pseudobnf <- function(
  path = get_pseudobnf(),
  version = NULL,
  source = "https://opendata.nhsbsa.net/dataset/bnf-code-information-current-year"
) {
  if (is.null(version)) {
    # Use the full resource file name (incl. extension) so the version label
    # matches the file published on the NHS BSA portal.
    version <- basename(path)
  }

  raw <- readr::read_csv(
    path,
    progress = FALSE,
    col_types = readr::cols(.default = "c")
  )

  # The BNF hierarchy levels, shallowest to deepest. The portal CSV provides an
  # explicit code + name column for each level, so the lookup is built directly
  # from these pairs (no need to slice a presentation code). The level labels and
  # output column names match the UK Biobank resource 592 BNF lookup.
  levels <- tibble::tribble(
    ~level                        ,
    ~code_col                     ,
    ~name_col                     ,
    ~out_name_col                 ,
    "chapter"                     ,
    "BNF_CHAPTER_CODE"            ,
    "BNF_CHAPTER"                 ,
    "BNF_Chapter"                 ,
    "section"                     ,
    "BNF_SECTION_CODE"            ,
    "BNF_SECTION"                 ,
    "BNF_Section"                 ,
    "paragraph"                   ,
    "BNF_PARAGRAPH_CODE"          ,
    "BNF_PARAGRAPH"               ,
    "BNF_Paragraph"               ,
    "subparagraph"                ,
    "BNF_SUBPARAGRAPH_CODE"       ,
    "BNF_SUBPARAGRAPH"            ,
    "BNF_Subparagraph"            ,
    "chemical_substance"          ,
    "BNF_CHEMICAL_SUBSTANCE_CODE" ,
    "BNF_CHEMICAL_SUBSTANCE"      ,
    "BNF_Chemical_Substance"      ,
    "product_name"                ,
    "BNF_PRODUCT_CODE"            ,
    "BNF_PRODUCT"                 ,
    "BNF_Product"                 ,
    "full"                        ,
    "BNF_PRESENTATION_CODE"       ,
    "BNF_PRESENTATION"            ,
    "BNF_Presentation"
  )

  check_pseudobnf_columns(raw, levels)

  # One frame per level: the code and name at that level, plus the ancestor name
  # columns (levels deeper than the current one are NA).
  lookup <- purrr::map(seq_len(nrow(levels)), function(i) {
    ancestor_cols <- purrr::set_names(
      purrr::map(seq_len(nrow(levels)), function(j) {
        if (j <= i) raw[[levels$name_col[[j]]]] else NA_character_
      }),
      levels$out_name_col
    )

    tibble::tibble(
      BNF_Code = raw[[levels$code_col[[i]]]],
      BNF_Code_Level = levels$level[[i]],
      !!!ancestor_cols,
      Description = raw[[levels$name_col[[i]]]]
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::distinct(.data[["BNF_Code"]], .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(
      "BNF_Code",
      "BNF_Code_Level",
      "BNF_Chapter",
      "BNF_Section",
      "BNF_Paragraph",
      "BNF_Subparagraph",
      "BNF_Chemical_Substance",
      "BNF_Product",
      "BNF_Presentation",
      "Description"
    )))

  relationship <- build_prefix_hierarchy_len(lookup$BNF_Code)

  list(
    bnf_lkp = list(
      lookup = list(
        table = lookup,
        metadata = lookup_metadata(
          code_type = "BNF",
          lookup_version = version,
          lookup_code_col = "BNF_Code",
          lookup_description_col = "Description",
          lookup_category_col = "BNF_Chapter",
          lookup_source = source
        )
      ),
      relationship = list(
        table = relationship,
        metadata = relationship_metadata(
          code_type = "BNF",
          relationship_version = version,
          from_col = "from",
          to_col = "to",
          relationship_source = source
        )
      )
    )
  )
}

# Check the portal CSV contains every expected level code/name column, erroring
# clearly if the published schema changes.
check_pseudobnf_columns <- function(raw, levels, call = rlang::caller_env()) {
  expected <- c(levels$code_col, levels$name_col)
  missing <- setdiff(expected, names(raw))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "x" = "The BNF code information CSV is missing expected column{?s}:
               {.field {missing}}.",
        "i" = "The published dataset schema may have changed."
      ),
      call = call
    )
  }
  invisible(raw)
}
