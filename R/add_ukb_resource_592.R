add_ukb_resource_592 <- function(
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
  ukb_resource_592 <- read_ukb_resource_592(
    path = path,
    sheets = sheets,
    ukb_version = ukb_version,
    ukb_source = ukb_source
  )

  .call <- rlang::current_call()

  cli::cli_inform("Adding tables to database")
  ukb_resource_592 |>
    purrr::iwalk(
      \(ukb_sheet, sheet_name) {
        ukb_sheet |>
          purrr::iwalk(\(ukb_table, table_type) {
            if (identical(table_type, "lookup")) {
              add_lookup_table(
                table = ukb_table$table,
                metadata = ukb_table$metadata
              )
            } else if (identical(table_type, "relationship")) {
              add_relationship_table(
                table = ukb_table$table,
                metadata = ukb_table$metadata
              )
            } else if (identical(table_type, "mapping")) {
              add_mapping_table(
                table = ukb_table$table,
                metadata = ukb_table$metadata
              )
            } else {
              cli::cli_abort(
                "Invalid format detected for {.arg {sheet_name}}",
                .envir = rlang::current_env(),
                call = .call
              )
            }
          })
      }
    )

  invisible(ukb_resource_592)
}
