# Helper functions for creating dummy TRUD release directories in tests.
# Each function creates a scoped temp directory with minimal valid content
# for the corresponding coding system.

create_dummy_icd10_dir <- function(.envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .envir)
  content_dir <- file.path(dir, "Content")
  dir.create(content_dir)

  # Row 1: no modifier (DESCRIPTION unchanged)
  # Row 2: MODIFIER_4 present (appended to DESCRIPTION)
  # Row 3: MODIFIER_5 present (appended to DESCRIPTION)
  writeLines(
    c(
      paste(
        c(
          "CODE",
          "ALT_CODE",
          "USAGE",
          "USAGE_UK",
          "DESCRIPTION",
          "MODIFIER_4",
          "MODIFIER_5",
          "QUALIFIERS",
          "GENDER_MASK",
          "MIN_AGE",
          "MAX_AGE",
          "TREE_DESCRIPTION"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "E10",
          "E10",
          "M",
          "N",
          "Type 1 diabetes mellitus",
          "",
          "",
          "",
          "0",
          "0",
          "999",
          "Endocrine"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "E11",
          "E11",
          "M",
          "N",
          "Type 2 diabetes mellitus",
          "With renal complications",
          "",
          "",
          "0",
          "0",
          "999",
          "Endocrine"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "E12",
          "E12",
          "M",
          "N",
          "Malnutrition-related diabetes",
          "",
          "With coma",
          "",
          "0",
          "0",
          "999",
          "Endocrine"
        ),
        collapse = "\t"
      )
    ),
    file.path(content_dir, "ICD10_Edition5_CodesAndTitlesAndMetadata_GB.txt")
  )

  dir
}

create_dummy_opcs4_dir <- function(.envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .envir)

  # Tab-separated, NO header, 2 columns: opcs4_code, description
  writeLines(
    c(
      "A01\tExcision of gallbladder",
      "A02\tTotal excision of liver",
      "A03\tPartial excision of liver"
    ),
    file.path(dir, "OPCS411 CodesAndTitles Nov 2025 V1.0.txt")
  )

  dir
}

create_dummy_read3_dir <- function(.envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .envir)
  v3_dir <- file.path(dir, "V3")
  dir.create(v3_dir)

  # Columns: code, status, type, related_code (C = active, R = retired)
  writeLines(
    c("X40J5|C|P|", "X40J6|C|P|", "X40J7|R|P|"),
    file.path(v3_dir, "Concept.v3")
  )

  # Columns: code, description_id, desc_type (P = preferred, S = synonym)
  writeLines(
    c("X40J5|D001|P", "X40J5|D002|S", "X40J6|D003|P", "X40J7|D004|P"),
    file.path(v3_dir, "Descrip.v3")
  )

  # Columns: description_id, term_type, term, alt1, alt2 (C = clinical, O = obsolete)
  writeLines(
    c(
      "D001|C|Disorder of organ||",
      "D002|O|Old term for disorder||",
      "D003|C|Other disorder||",
      "D004|C|Retired disorder||"
    ),
    file.path(v3_dir, "Terms.v3")
  )

  # Columns: child_code, parent_code, relationship_type
  writeLines(
    c("X40J5|X40J6|01", "X40J6|X40J7|01"),
    file.path(v3_dir, "V3hier.v3")
  )

  dir
}

create_dummy_read2_dir <- function(.envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .envir)

  assured_dir <- file.path(
    dir,
    "Mapping Tables",
    "Updated",
    "Clinically Assured"
  )
  not_assured_dir <- file.path(
    dir,
    "Mapping Tables",
    "Updated",
    "Not Clinically Assured"
  )
  dir.create(assured_dir, recursive = TRUE)
  dir.create(not_assured_dir, recursive = TRUE)

  # rctermsctmap: ReadCode|Term|ConceptId|MapId (tab-sep with header)
  writeLines(
    c(
      paste(c("ReadCode", "Term", "ConceptId", "MapId"), collapse = "\t"),
      paste(c("1234", "Diabetes mellitus", "12345678", "1"), collapse = "\t"),
      paste(
        c("1234", "DM (diabetes mellitus)", "12345678", "2"),
        collapse = "\t"
      ),
      paste(c("5678", "Hypertension", "87654321", "1"), collapse = "\t")
    ),
    file.path(not_assured_dir, "rctermsctmap_uk_20200401000001.txt")
  )

  # rctctv3map: Read V2 -> CTV3 (tab-sep with header)
  writeLines(
    c(
      paste(
        c("V2_CONCEPTID", "CTV3_CONCEPTID", "MAPPING_TYPE"),
        collapse = "\t"
      ),
      paste(c("1234", "X40J5", "P"), collapse = "\t"),
      paste(c("5678", "X40J6", "P"), collapse = "\t")
    ),
    file.path(assured_dir, "rctctv3map_uk_20200401000001.txt")
  )

  # ctv3rctmap: CTV3 -> Read V2 (tab-sep with header)
  writeLines(
    c(
      paste(
        c("CTV3_CONCEPTID", "V2_CONCEPTID", "MAPPING_TYPE"),
        collapse = "\t"
      ),
      paste(c("X40J5", "1234", "P"), collapse = "\t"),
      paste(c("X40J6", "5678", "P"), collapse = "\t")
    ),
    file.path(assured_dir, "ctv3rctmap_uk_20200401000002.txt")
  )

  dir
}

# Shared mock metadata factory for get_* tests
mock_trud_metadata <- function(archive_name) {
  list(
    releases = list(
      list(id = "release_1", archiveFileName = archive_name),
      list(id = "release_2", archiveFileName = paste0("old_", archive_name))
    )
  )
}
