read_ukb_resource_592 <- function(
  path = get_ukb_resource_592(),
  sheets = c(
    "bnf_lkp",
    "dmd_lkp",
    "icd9_lkp",
    "icd10_lkp",
    "icd9_icd10",
    "read_v2_lkp",
    "read_v2_drugs_lkp",
    "read_v2_drugs_bnf",
    "read_v2_icd9",
    "read_v2_icd10",
    "read_v2_opcs4",
    "read_v2_read_ctv3",
    "read_ctv3_lkp",
    "read_ctv3_icd9",
    "read_ctv3_icd10",
    "read_ctv3_opcs4",
    "read_ctv3_read_v2"
  ),
  ukb_version = "UKB v4",
  ukb_source = "https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592"
) {
  # validate args
  if (!file.exists(path)) {
    codeminer_abort(
      c(
        "x" = "{.arg {path}} not found.",
        "i" = "UKB Resource 592 can be downloaded from {.url https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}"
      )
    )
  }

  rlang::arg_match(
    sheets,
    values = names(ukb_592_processors()),
    multiple = TRUE
  )

  # Auto-include dependencies for tables that need extension
  sheets <- add_ukb592_dependencies(sheets)

  # read selected sheets
  cli::cli_inform(
    "Reading {length(sheets)} selected table{?s} from UKB Resource 592"
  )

  result <- sheets |>
    purrr::set_names() |>
    purrr::map(
      \(.x) {
        read_ukb_resource_592_sheet(
          path = path,
          sheet = .x,
          ukb_version = ukb_version,
          ukb_source = ukb_source
        )
      },
      .progress = TRUE
    )

  # Force new line after progress bar
  cli::cli_inform("")

  # Post-process: extend read_v2_drugs_bnf if all required tables are present
  if (
    all(
      c("read_v2_drugs_bnf", "bnf_lkp", "read_v2_drugs_lkp") %in% names(result)
    )
  ) {
    cli::cli_inform(
      "Extending {.field read_v2_drugs_bnf} with BNF hierarchy and descriptions"
    )
    result$read_v2_drugs_bnf <- extend_read_v2_drugs_bnf_from_ukb592(result)
  }

  # Post-process: extend read_v2_icd10 if all required tables are present
  if (all(c("read_v2_icd10", "icd10_lkp") %in% names(result))) {
    cli::cli_inform(
      "Extending {.field read_v2_icd10} by expanding ICD-10 code ranges"
    )
    result$read_v2_icd10 <- extend_read_v2_icd10_from_ukb592(result)
  }

  result
}

read_ukb_resource_592_sheet <- function(
  path,
  sheet,
  ukb_version,
  ukb_source
) {
  # Read excel sheet as type character
  .df <- readxl::read_excel(path = path, sheet = sheet, col_types = "text")

  # Remove metadata from footer rows. Metadata for each table in UKB resource
  # 592 is recorded in one or more footer rows (in column 1), separated from the
  # main table by an empty row.
  empty_row <- max(which(is.na(.df[[1]])))

  if (rlang::is_empty(empty_row)) {
    cli::cli_abort(c(
      x = "No empty row detected before footer in UKB Resource 592 sheet {sheet}.",
      i = paste0(
        "Check that all sheets have an empty row followed by a footer ",
        "with metdata in {.file {path}}."
      )
    ))
  }

  .df <- .df[1:(empty_row - 1), ]

  # Process - must return a named list with at least one of items "lookup",
  # "relationship" and/or "mapping"
  ukb_592_processors()[[sheet]](.df, ukb_version, ukb_source)
}

# Process_ functions ------------------------------------------------------

ukb_592_processors <- function() {
  list(
    bnf_lkp = process_bnf_lkp,
    dmd_lkp = process_dmd_lkp,
    icd9_lkp = process_icd9_lkp,
    icd10_lkp = process_icd10_lkp,
    icd9_icd10 = process_icd9_icd10,
    read_v2_lkp = process_read_v2_lkp,
    read_v2_drugs_lkp = process_read_v2_drugs_lkp,
    read_v2_drugs_bnf = process_read_v2_drugs_bnf,
    read_v2_icd9 = process_read_v2_icd9,
    read_v2_icd10 = process_read_v2_icd10,
    read_v2_opcs4 = process_read_v2_opcs4,
    read_v2_read_ctv3 = process_read_v2_read_ctv3,
    read_ctv3_lkp = process_read_ctv3_lkp,
    read_ctv3_icd9 = process_read_ctv3_icd9,
    read_ctv3_icd10 = process_read_ctv3_icd10,
    read_ctv3_opcs4 = process_read_ctv3_opcs4,
    read_ctv3_read_v2 = process_read_ctv3_read_v2
  )
}

process_bnf_lkp <- function(.df, ukb_version, ukb_source) {
  # Extend lookup table so that code column ("BNF_Code") includes BNF chapters,
  # sections, paragraphs etc
  lookup <- .df |>
    dplyr::mutate(
      "code_chapter" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 2
      ),
      "code_section" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 4
      ),
      "code_paragraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 6
      ),
      "code_subparagraph" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 7
      ),
      "code_chemical_substance" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 9
      ),
      "code_product_name" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 11
      ),
      "code_further_info" = stringr::str_sub(
        string = .data[["BNF_Presentation_Code"]],
        start = 1,
        end = 13
      )
    ) |>
    dplyr::rename("code_full" = dplyr::all_of("BNF_Presentation_Code")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("code"),
      names_to = "BNF_Code_Level",
      values_to = "BNF_Code"
    ) |>
    dplyr::select(
      dplyr::all_of("BNF_Code_Level"),
      dplyr::all_of("BNF_Code"),
      dplyr::everything()
    ) |>
    dplyr::mutate(
      "BNF_Code_Level" = stringr::str_remove(
        .data[["BNF_Code_Level"]],
        "code_"
      )
    ) |>
    dplyr::distinct(.data[["BNF_Code"]], .keep_all = TRUE) |>
    dplyr::mutate(
      "Description" = dplyr::case_when(
        .data[["BNF_Code_Level"]] == "chapter" ~ .data[["BNF_Chapter"]],
        .data[["BNF_Code_Level"]] == "section" ~ .data[["BNF_Section"]],
        .data[["BNF_Code_Level"]] == "paragraph" ~ .data[["BNF_Paragraph"]],
        .data[["BNF_Code_Level"]] == "subparagraph" ~
          .data[[
            "BNF_Subparagraph"
          ]],
        .data[["BNF_Code_Level"]] == "chemical_substance" ~
          .data[[
            "BNF_Chemical_Substance"
          ]],
        .data[["BNF_Code_Level"]] == "product_name" ~ .data[["BNF_Product"]],
        .data[["BNF_Code_Level"]] == "further_info" ~
          .data[[
            "BNF_Presentation"
          ]],
        .data[["BNF_Code_Level"]] == "full" ~ .data[["BNF_Presentation"]]
      )
    ) |>
    dplyr::mutate(
      "BNF_Presentation" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Presentation"]]
      ),
      "BNF_Product" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info",
            "product_name"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Product"]]
      ),
      "BNF_Chemical_Substance" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info",
            "product_name",
            "chemical_substance"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Chemical_Substance"]]
      ),
      "BNF_Subparagraph" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info",
            "product_name",
            "chemical_substance",
            "subparagraph"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Subparagraph"]]
      ),
      "BNF_Paragraph" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info",
            "product_name",
            "chemical_substance",
            "subparagraph",
            "paragraph"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Paragraph"]]
      ),
      "BNF_Section" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in%
          c(
            "full",
            "further_info",
            "product_name",
            "chemical_substance",
            "subparagraph",
            "paragraph",
            "section"
          ) ~
          as.character(NA),
        TRUE ~ .data[["BNF_Section"]]
      ),
    ) |>
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

  # Create relationship table
  relationship <- lookup$BNF_Code |>
    build_prefix_hierarchy_len()

  # Return with metadata
  list(
    lookup = list(
      table = lookup,
      metadata = lookup_metadata(
        code_type = "BNF",
        lookup_version = ukb_version,
        lookup_code_col = "BNF_Code",
        lookup_description_col = "Description",
        preferred_description_col = NA_character_,
        preferred_description_indicator = NA_character_,
        lookup_source = ukb_source,
      )
    ),
    relationship = list(
      table = relationship,
      metadata = relationship_metadata(
        code_type = "BNF",
        relationship_version = ukb_version,
        from_col = "from",
        to_col = "to",
        type_col = "type",
        child_parent_relationship_code = "is a",
        relationship_source = ukb_source
      )
    )
  )
}

process_dmd_lkp <- function(.df, ukb_version, ukb_source) {
  list(
    lookup = list(
      table = .df,
      metadata = lookup_metadata(
        code_type = "DM+D",
        lookup_version = ukb_version,
        lookup_code_col = "concept_id",
        lookup_description_col = "term",
        preferred_description_col = NA_character_,
        preferred_description_indicator = NA_character_,
        lookup_source = ukb_source,
      )
    )
  )
}

process_icd9_lkp <- function(.df, ukb_version, ukb_source) {
  # Create relationship table
  relationship <- .df$ICD9 |>
    build_prefix_hierarchy_len()

  list(
    lookup = list(
      table = .df,
      metadata = lookup_metadata(
        code_type = "ICD-9",
        lookup_version = ukb_version,
        lookup_code_col = "ICD9",
        lookup_description_col = "DESCRIPTION_ICD9",
        preferred_description_col = NA_character_,
        preferred_description_indicator = NA_character_,
        lookup_source = ukb_source,
      )
    ),
    relationship = list(
      table = relationship,
      metadata = relationship_metadata(
        code_type = "ICD-9",
        relationship_version = ukb_version,
        from_col = "from",
        to_col = "to",
        type_col = "type",
        child_parent_relationship_code = "is a",
        relationship_source = ukb_source
      )
    )
  )
}

process_icd10_lkp <- function(.df, ukb_version, ukb_source) {
  # Some ICD-10 descriptions include a modifier e.g. "E10" = "Type 1 diabetes
  # mellitus", whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With
  # coma" is contained in the modifier columns "MODIFIER-4". See 'S27' for an
  # example code where additional description is contained in the "MODIFER-5"
  # column. There are no codes with a modifier description in
  # both "MODIFIER_4" and "MODIFIER_5".
  lookup <- .df |>
    dplyr::mutate(
      "DESCRIPTION" = dplyr::case_when(
        !is.na(.data$MODIFIER_4) ~ paste(.data$DESCRIPTION, .data$MODIFIER_4),
        !is.na(.data$MODIFIER_5) ~ paste(.data$DESCRIPTION, .data$MODIFIER_5),
        TRUE ~ .data$DESCRIPTION
      )
    ) |>
    # Remove 'X' suffix from undivided ICD10 codes (e.g. "A38" has no child
    # codes)
    dplyr::mutate(
      "ALT_CODE" = stringr::str_remove(
        .data[["ALT_CODE"]],
        pattern = "X$"
      )
    )

  # Create relationship table
  relationship <- lookup$ALT_CODE |>
    build_prefix_hierarchy_len()

  # Return with metadata
  list(
    lookup = list(
      table = lookup,
      metadata = lookup_metadata(
        code_type = "ICD-10",
        lookup_version = ukb_version,
        lookup_code_col = "ALT_CODE",
        lookup_description_col = "DESCRIPTION",
        preferred_description_col = NA_character_,
        preferred_description_indicator = NA_character_,
        lookup_source = ukb_source,
      )
    ),
    relationship = list(
      table = relationship,
      metadata = relationship_metadata(
        code_type = "ICD-10",
        relationship_version = ukb_version,
        from_col = "from",
        to_col = "to",
        type_col = "type",
        child_parent_relationship_code = "is a",
        relationship_source = ukb_source
      )
    )
  )
}

process_icd9_icd10 <- function(.df, ukb_version, ukb_source) {
  # For ICD9 codes without an equivalent ICD10 code, the ICD10 code is recorded
  # as 'UNDEF', with `NA` for the description (and vice versa). This function
  # converts values of 'UNDEF' in the `ICD9` and `ICD10` columns to `NA`.

  # convert 'UNDEF' ICD9/10 codes to `NA`
  mapping <- .df |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c(
        "ICD9",
        "ICD10"
      )),
      \(.x) ifelse(.x == "UNDEF", yes = NA_character_, no = .x)
    ))

  list(
    mapping = list(
      table = mapping,
      metadata = mapping_metadata(
        from_code_type = "ICD-9",
        to_code_type = "ICD-10",
        map_version = ukb_version,
        from_col = "ICD9",
        to_col = "ICD10",
        map_source = ukb_source
      )
    )
  )
}

process_read_v2_lkp <- function(.df, ukb_version, ukb_source) {
  relationship <- .df$read_code |>
    # Removes only trailing padding dots from Read 2 codes, while preserving
    # leading or internal dots.
    stringr::str_replace("\\.+$", "") |>
    build_prefix_hierarchy_len() |>
    # Replace trailing dots
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("from", "to")),
      \(x) stringr::str_pad(x, 5, pad = ".", side = "right")
    ))

  list(
    lookup = list(
      table = .df,
      metadata = lookup_metadata(
        code_type = "Read 2",
        lookup_version = ukb_version,
        lookup_code_col = "read_code",
        lookup_description_col = "term_description",
        preferred_description_col = "term_code",
        preferred_description_indicator = "00",
        lookup_source = ukb_source
      )
    ),
    relationship = list(
      table = relationship,
      metadata = relationship_metadata(
        code_type = "Read 2",
        relationship_version = ukb_version,
        from_col = "from",
        to_col = "to",
        type_col = "type",
        child_parent_relationship_code = "is a",
        relationship_source = ukb_source
      )
    )
  )
}

process_read_v2_drugs_lkp <- function(.df, ukb_version, ukb_source) {
  list(
    lookup = list(
      table = .df,
      metadata = lookup_metadata(
        code_type = "Read 2, drugs",
        lookup_version = ukb_version,
        lookup_code_col = "read_code",
        lookup_description_col = "term_description",
        preferred_description_col = NA_character_,
        preferred_description_indicator = NA_character_,
        lookup_source = ukb_source
      )
    )
  )
}

process_read_v2_drugs_bnf <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 2, drugs",
        to_code_type = "BNF",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "bnf_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_v2_icd9 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 2",
        to_code_type = "ICD-9",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "icd9_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_v2_icd10 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 2",
        to_code_type = "ICD-10",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "icd10_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_v2_opcs4 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 2",
        to_code_type = "OPCS4",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "opcs_4.2_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_v2_read_ctv3 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 2",
        to_code_type = "Read 3",
        map_version = ukb_version,
        from_col = "READV2_CODE",
        to_col = "READV3_CODE",
        map_source = ukb_source
      )
    )
  )
}

process_read_ctv3_lkp <- function(.df, ukb_version, ukb_source) {
  list(
    lookup = list(
      table = .df,
      metadata = lookup_metadata(
        code_type = "Read 3",
        lookup_version = ukb_version,
        lookup_code_col = "read_code",
        lookup_description_col = "term_description",
        preferred_description_col = "description_type",
        preferred_description_indicator = "P",
        lookup_source = ukb_source
      )
    )
  )
}

process_read_ctv3_icd9 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 3",
        to_code_type = "ICD-9",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "icd9_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_ctv3_icd10 <- function(.df, ukb_version, ukb_source) {
  # Remove 'D' and 'A' from the ends of ICD10 codes, and separate these into a
  # separate column called `icd10_dagger_asterisk`. The 'D' and 'A' indicate
  # whether the code is a 'dagger' or 'asterisk' respectively. However, these
  # codes are listed without the appended 'D'/'A' in the `icd10_lkp` table.
  .df <- .df |>
    dplyr::mutate(
      "icd10_dagger_asterisk" = stringr::str_extract(
        .data[["icd10_code"]],
        pattern = icd10_dxa_pattern()
      )
    ) |>
    dplyr::mutate(
      "icd10_dagger_asterisk" = dplyr::if_else(
        .data[["icd10_dagger_asterisk"]] == "",
        NA,
        .data[["icd10_dagger_asterisk"]]
      )
    ) |>
    dplyr::mutate(
      "icd10_code" = stringr::str_remove(
        .data[["icd10_code"]],
        pattern = icd10_dxa_pattern()
      )
    )

  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 3",
        to_code_type = "ICD-10",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "icd10_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_ctv3_opcs4 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 3",
        to_code_type = "OPCS4",
        map_version = ukb_version,
        from_col = "read_code",
        to_col = "opcs4_code",
        map_source = ukb_source
      )
    )
  )
}

process_read_ctv3_read_v2 <- function(.df, ukb_version, ukb_source) {
  list(
    mapping = list(
      table = .df,
      metadata = mapping_metadata(
        from_code_type = "Read 3",
        to_code_type = "Read 2",
        map_version = ukb_version,
        from_col = "READV3_CODE",
        to_col = "READV2_CODE",
        map_source = ukb_source
      )
    )
  )
}

# Helper functions --------------------------------------------------------

#' Extend read_v2_drugs_bnf mapping table with BNF hierarchy and descriptions
#'
#' Adds Read code descriptions and BNF hierarchy information (chapter, section,
#' paragraph, subparagraph) to the read_v2_drugs_bnf mapping table.
#'
#' @param ukb592_result Named list of results from read_ukb_resource_592, must
#'   contain `read_v2_drugs_bnf`, `read_v2_drugs_lkp`, and `bnf_lkp`.
#'
#' @return Extended mapping table with same structure as input but with
#'   additional columns.
#' @noRd
extend_read_v2_drugs_bnf_from_ukb592 <- function(ukb592_result) {
  # Extract the mapping table
  read_v2_drugs_bnf <- ukb592_result$read_v2_drugs_bnf$mapping$table

  # Extract the lookup tables
  read_v2_drugs_lkp <- ukb592_result$read_v2_drugs_lkp$lookup$table
  bnf_lkp_extended <- ukb592_result$bnf_lkp$lookup$table

  # Extend `read_v2_drugs_bnf`
  expected_nrow <- nrow(read_v2_drugs_bnf)

  extended_table <- read_v2_drugs_bnf |>
    # add read code descriptions
    dplyr::left_join(read_v2_drugs_lkp, by = "read_code") |>
    # extract bnf chapter, section etc from `bnf_code` col in `read_v2_drugs_bnf`
    dplyr::mutate(
      "bnf_chapter_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 2
      ),
      "bnf_section_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 4
      ),
      "bnf_paragraph_code" = stringr::str_sub(
        stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
        start = 1,
        end = 6
      ),
      "bnf_subparagraph_code" = paste0(
        .data[["bnf_paragraph_code"]],
        stringr::str_sub(
          stringr::str_remove_all(.data[["bnf_code"]], pattern = "\\."),
          start = 8,
          end = 8
        )
      )
    ) |>
    # add BNF details
    dplyr::left_join(
      bnf_lkp_extended[, c("BNF_Code", "BNF_Chapter")],
      by = c("bnf_chapter_code" = "BNF_Code")
    ) |>
    dplyr::left_join(
      bnf_lkp_extended[, c("BNF_Code", "BNF_Section")],
      by = c("bnf_section_code" = "BNF_Code")
    ) |>
    dplyr::left_join(
      bnf_lkp_extended[, c("BNF_Code", "BNF_Paragraph")],
      by = c("bnf_paragraph_code" = "BNF_Code")
    ) |>
    dplyr::left_join(
      bnf_lkp_extended[, c("BNF_Code", "BNF_Subparagraph")],
      by = c("bnf_subparagraph_code" = "BNF_Code")
    )

  # check nrows remains the same
  if (expected_nrow != nrow(extended_table)) {
    cli::cli_abort(
      "Error! Unexpected number of rows when extending `read_v2_drugs_bnf`"
    )
  }

  # Return with same structure but updated table
  list(
    mapping = list(
      table = extended_table,
      metadata = ukb592_result$read_v2_drugs_bnf$mapping$metadata
    )
  )
}


#' Extend read_v2_icd10 mapping table by expanding ICD-10 code ranges
#'
#' Converts values in the `icd10_code` column to 'ALT_CODE' format ICD10 codes
#' that are recognized in the `icd10_lkp` lookup table. This involves dividing
#' cells containing more than one ICD10 code over multiple rows (e.g.
#' 'A414+J038' becomes 2 rows), and removing appended 'D'/'A' characters (which
#' indicate dagger/asterisk codes) to a separate column called
#' `icd10_dagger_asterisk` (e.g.'A010D I398A' becomes 'A010' and 'I398' under
#' `icd10_code`, with 'D' and 'A' recorded under `icd10_dagger_asterisk`).
#'
#' **NOTE:** A number of undivided 3 character ICD10 codes appear (incorrectly)
#' without an 'X' appended in this mapping table. For example, 'A64X' appears
#' (incorrectly) as 'A50-A64' in 2 rows. 'A65X' appears as 'A65-A69', 'A70X' as
#' 'A70-A74', 'A89X' as 'A80-A89', 'A99X' as 'A92-A99' etc. This function
#' appends 'X' to these codes to match how they appear in the `icd10_lkp` table.
#'
#' @param ukb592_result Named list of results from read_ukb_resource_592, must
#'   contain `read_v2_icd10` and `icd10_lkp`.
#'
#' @return Extended mapping table with same structure as input but with
#'   expanded and cleaned ICD-10 codes.
#' @noRd
extend_read_v2_icd10_from_ukb592 <- function(ukb592_result) {
  # Extract the mapping table
  read_v2_icd10 <- ukb592_result$read_v2_icd10$mapping$table

  # Extract the lookup table
  icd10_lkp <- ukb592_result$icd10_lkp$lookup$table

  # replace spaces and '+' with commas
  .df <- read_v2_icd10 |>
    dplyr::mutate(
      "icd10_code" = stringr::str_replace_all(
        .data[["icd10_code"]],
        pattern = "[\\s|\\+]",
        replacement = ","
      )
    )

  # split by comma, then unnest
  .df <- .df |>
    dplyr::mutate(
      "icd10_code" = stringr::str_split(.data[["icd10_code"]], pattern = ",")
    ) |>
    tidyr::unnest(cols = "icd10_code")

  # remove 'D' and 'A' final characters from ICD10 codes, and place in separate
  # column `icd10_dagger_asterisk`. Also remove 'X', which is appended to
  # undivided codes e.g. 'A38X' becomes 'A38'
  .df <- .df |>
    dplyr::mutate(
      "icd10_dagger_asterisk" = stringr::str_extract(
        .data[["icd10_code"]],
        pattern = icd10_dxa_pattern()
      )
    ) |>
    dplyr::mutate(
      "icd10_dagger_asterisk" = stringr::str_remove(
        .data[["icd10_dagger_asterisk"]],
        pattern = "X"
      )
    ) |>
    dplyr::mutate(
      "icd10_dagger_asterisk" = dplyr::if_else(
        .data[["icd10_dagger_asterisk"]] == "",
        NA,
        .data[["icd10_dagger_asterisk"]]
      )
    ) |>
    dplyr::mutate(
      "icd10_code" = stringr::str_remove(
        .data[["icd10_code"]],
        pattern = icd10_dxa_pattern()
      )
    )

  # expand icd10 code ranges, which are flagged as '2' under `icd10_code_def`
  # (e.g. 'E100-E109')
  .df <- .df |>
    tidyr::separate(
      .data[["icd10_code"]],
      into = c("start_icd10_code", "end_icd10_code"),
      sep = "-",
      remove = FALSE,
      fill = "right"
    ) |>
    dplyr::mutate(
      "start_icd10_code" = dplyr::if_else(
        is.na(.data[["end_icd10_code"]]),
        true = NA_character_,
        false = .data[["start_icd10_code"]]
      )
    )

  # strip any appended 'D/X/A' (last character(s) e.g. 'A89X' and 'A170D' become
  # 'A89' and 'A170'. 'G01XA' would become 'G01', although note that this code
  # does not appear together with a '-')
  .df <- .df |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(c(
        "start_icd10_code",
        "end_icd10_code"
      )),
      \(.x) {
        stringr::str_remove(
          .x,
          pattern = icd10_dxa_pattern()
        )
      }
    ))

  # expand ranges
  extended_table <- .df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      icd10_range_new = ifelse(
        is.na(.data[["start_icd10_code"]]),
        yes = list(NA_character_),
        no = list(
          expand_icd10_code_range(
            start_icd10_code = .data[["start_icd10_code"]],
            end_icd10_code = .data[["end_icd10_code"]],
            icd10_lkp = icd10_lkp
          )
        )
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = "icd10_range_new") |>
    dplyr::mutate(
      "icd10_code" = ifelse(
        is.na(.data[["icd10_range_new"]]),
        yes = .data[["icd10_code"]],
        no = .data[["icd10_range_new"]]
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "start_icd10_code",
        "end_icd10_code",
        "icd10_range_new"
      ))
    )

  # Return with same structure but updated table
  list(
    mapping = list(
      table = extended_table,
      metadata = ukb592_result$read_v2_icd10$mapping$metadata
    )
  )
}


#' Build Prefix-Based Code Hierarchy
#'
#' @description
#' Constructs a parent-child hierarchy from a lookup table of codes using
#' a **longest-valid-prefix rule**. A parent is defined as the longest
#' shorter prefix of a child that exists as a real code in the dataset.
#'
#' Suitable for:
#' - **BNF codes**
#' - **ICD-10 ALT format** (e.g. `E109 -> E10`)
#' - **Read 2 codes** (after removing trailing padding dots)
#'
#' @param codes A character vector of codes
#'
#' @return A tibble with three columns:
#' - `from`: Child code
#' - `to`: Nearest valid parent code
#' - `type`: Type of relationship connecting `from` and `to`. Always "is a".
#'
#' @examples
#' \dontrun{
#' build_prefix_hierarchy_len(bnf_lookup, "code")
#' build_prefix_hierarchy_len(icd10alt_lookup, "code")
#' }
#'
#' @noRd
build_prefix_hierarchy_len <- function(codes) {
  codes <- tibble::tibble(code = unique(codes)) |>
    dplyr::mutate(len = nchar(code))

  lens <- sort(unique(codes$len))

  all_pairs <- purrr::map_dfr(seq_along(lens)[-1], \(i) {
    child_len <- lens[i]
    parent_len <- lens[(i - 1)]

    children <- codes[codes$len == child_len, ]
    parents <- codes[codes$len == parent_len, ]

    if (nrow(children) == 0 || nrow(parents) == 0) {
      stop()
    }

    children |>
      dplyr::mutate(parent_code = substr(.data$code, 1, parent_len)) |>
      dplyr::inner_join(
        parents |>
          dplyr::select("parent_code" = dplyr::all_of("code")),
        by = "parent_code"
      ) |>
      dplyr::select(
        "from" = dplyr::all_of("code"),
        "to" = dplyr::all_of("parent_code")
      )
  })

  all_pairs |>
    dplyr::mutate("type" = "is a")
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
expand_icd10_code_range <- function(
  start_icd10_code,
  end_icd10_code,
  icd10_lkp
) {
  # validate args
  stopifnot(is.character(start_icd10_code))
  stopifnot(is.character(end_icd10_code))

  stopifnot(
    stringr::str_length(stringr::str_remove(
      start_icd10_code,
      "X$"
    )) ==
      stringr::str_length(stringr::str_remove(
        end_icd10_code,
        "X$"
      ))
  )

  stopifnot(all(c(start_icd10_code, end_icd10_code) %in% icd10_lkp$ALT_CODE))

  # get start and end row indices
  icd10_lkp <- icd10_lkp |>
    dplyr::arrange(ALT_CODE) |>
    tibble::rowid_to_column(".rowid")

  start_rowid <- icd10_lkp |>
    dplyr::filter(.data[["ALT_CODE"]] == .env$start_icd10_code) |>
    dplyr::pull(.data[[".rowid"]])

  end_rowid <- icd10_lkp |>
    dplyr::filter(.data[["ALT_CODE"]] == .env$end_icd10_code) |>
    dplyr::pull(.data[[".rowid"]])

  # Create range of row index integers
  icd10_lkp_rowids <- start_rowid:end_rowid

  # filter for selected row index integers
  result <- icd10_lkp |>
    dplyr::filter(.data[[".rowid"]] %in% .env$icd10_lkp_rowids) |>
    dplyr::pull(.data[["ALT_CODE"]])

  # expand (e.g. for 'A80-A81', at this stage all 'A80' should be present
  # ('A800-A809'), but for 'A81', only 'A81' wil be present - needs expanding
  # to 'A801-A819')
  result <- icd10_lkp |>
    dplyr::filter(stringr::str_detect(
      .data[["ALT_CODE"]],
      pattern = stringr::str_c(paste0("^", result), sep = "", collapse = "|")
    )) |>
    dplyr::pull(.data[["ALT_CODE"]])

  return(result)
}

#' Add required dependencies for UKB Resource 592 sheets
#'
#' Automatically includes dependency sheets required for extension processing.
#' If a sheet requires other sheets for post-processing (e.g., read_v2_drugs_bnf
#' needs bnf_lkp and read_v2_drugs_lkp), this function adds them to the sheets
#' vector and optionally displays an informative message.
#'
#' @param sheets Character vector of sheet names to read
#' @param inform Logical indicating whether to show CLI messages (default TRUE)
#'
#' @return Character vector with dependencies added (duplicates removed)
#' @noRd
add_ukb592_dependencies <- function(sheets, inform = TRUE) {
  # Define dependencies for sheets that need extension
  dependencies <- list(
    read_v2_drugs_bnf = c("bnf_lkp", "read_v2_drugs_lkp"),
    read_v2_icd10 = c("icd10_lkp")
  )

  # Check each sheet that has dependencies
  for (sheet in names(dependencies)) {
    if (sheet %in% sheets) {
      required <- dependencies[[sheet]]
      missing <- setdiff(required, sheets)

      if (length(missing) > 0) {
        sheets <- unique(c(sheets, missing))

        if (inform) {
          cli::cli_inform(
            "Adding {.field {missing}} (required for extending {.field {sheet}})"
          )
        }
      }
    }
  }

  sheets
}

icd10_dxa_pattern <- function() {
  "[D|X|A]*$"
}
