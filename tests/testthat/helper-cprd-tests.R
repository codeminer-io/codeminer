# Helper for creating a dummy CPRD Aurum code browser release in tests.
#
# All values are placeholders: no real CPRD medcodes, prodcodes, terms or
# identifiers. The files are tab-separated with CRLF line endings, and include
# an empty Release column and a term containing a double quote, to test parsing.

create_dummy_cprd_aurum_dir <- function(
  .envir = parent.frame(),
  dir_name = "CPRD_CodeBrowser_209901_Aurum"
) {
  root <- withr::local_tempdir(.local_envir = .envir)
  dir <- file.path(root, dir_name)
  dir.create(dir)

  write_crlf <- function(rows, file) {
    lines <- purrr::map_chr(rows, \(x) paste(x, collapse = "\t"))
    con <- file(file.path(dir, file), open = "wb")
    on.exit(close(con))
    writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), con)
  }

  write_crlf(
    list(
      c(
        "MedCodeId",
        "Observations",
        "OriginalReadCode",
        "CleansedReadCode",
        "Term",
        "SnomedCTConceptId",
        "SnomedCTDescriptionId",
        "Release",
        "EmisCodeCategoryId"
      ),
      c(
        "MED001",
        "10",
        "DUM01",
        "DUM0100",
        "Dummy term A",
        "SCT001",
        "DSC001",
        "",
        "1"
      ),
      c(
        "MED002",
        "0",
        "DUM01-1",
        "DUM0111",
        "Dummy term \"B\"",
        "SCT001",
        "DSC002",
        "",
        "1"
      ),
      c(
        "MED003",
        "5",
        "^ESCTDUMMY",
        "",
        "Dummy term C",
        "SCT002",
        "DSC003",
        "",
        "2"
      )
    ),
    "CPRDAurumMedical.txt"
  )

  write_crlf(
    list(
      c(
        "ProdCodeId",
        "dmdid",
        "Term from EMIS",
        "ProductName",
        "Formulation",
        "RouteOfAdministration",
        "DrugSubstanceName",
        "SubstanceStrength",
        "BNFChapter",
        "DrugIssues"
      ),
      c(
        "PROD001",
        "DMD001",
        "Dummy product A",
        "Dummy product A",
        "Dummy form 1",
        "Dummy route 1",
        "Dummy substance A",
        "1mg",
        "99000000",
        "100"
      ),
      c(
        "PROD002",
        "DMD002",
        "Dummy product B",
        "Dummy product B",
        "Dummy form 2",
        "Dummy route 1/Dummy route 2",
        "Dummy substance A/ Dummy substance B",
        "1mg + 2mg",
        "",
        "20"
      ),
      c("PROD003", "", "Dummy product C", "", "", "", "", "", "", "3")
    ),
    "CPRDAurumProduct.txt"
  )

  dir
}
