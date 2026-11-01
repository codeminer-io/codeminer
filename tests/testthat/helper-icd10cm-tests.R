# Helpers for building minimal ICD-10-CM order files in tests. The layout
# matches the fixed-width file published by NCHS; codes are real-shaped but the
# descriptions are made up.

# One order-file line: 1-5 order number, 7-13 code, 15 billable flag,
# 17-76 short description, 78+ long description.
icd10cm_order_line <- function(order, code, billable, short, long) {
  sprintf("%05d %-7s %s %-60s %s", order, code, billable, short, long)
}

# Rows chosen to exercise: a header/billable mix; a code whose immediate
# prefixes are not codes (T07XXXA -> T07); alphanumeric categories (C4A, O9A,
# QA1); a category outside the WHO ICD-10 ranges (K95); and a code that only
# exists at depth (E113211 under E11321).
dummy_icd10cm_rows <- function() {
  # fmt: skip
  tibble::tribble(
    ~code     , ~billable , ~short                    , ~long                                       ,
    "C4A"     , "0"       , "Dummy neoplasm category" , "Dummy neoplasm category with letter"       ,
    "C4A0"    , "1"       , "Dummy neoplasm, site 0"  , "Dummy neoplasm of site zero"               ,
    "E11"     , "0"       , "Dummy diabetes"          , "Dummy type 2 diabetes mellitus"            ,
    "E113"    , "0"       , "Dummy diab w ophth"      , "Dummy diabetes with ophthalmic complications",
    "E1132"   , "0"       , "Dummy diab w mild rtnop" , "Dummy diabetes with mild retinopathy"      ,
    "E11321"  , "0"       , "Dummy diab w mild rtnop" , "Dummy diabetes with mild retinopathy, oedema",
    "E113211" , "1"       , "Dummy diab, right eye"   , "Dummy diabetes with mild retinopathy, right eye",
    "K95"     , "0"       , "Dummy bariatric compl"   , "Dummy complications of bariatric procedures",
    "K950"    , "1"       , "Dummy gastric band"      , "Dummy complications of gastric band"       ,
    "O9A"     , "0"       , "Dummy maternal care"     , "Dummy maternal care for other conditions"  ,
    "QA1"     , "0"       , "Dummy genetic disorder"  , "Dummy genetic disorder category"           ,
    "QA10"    , "1"       , "Dummy genetic, type 0"   , "Dummy genetic disorder, type zero"         ,
    "T07"     , "0"       , "Dummy unsp injuries"     , "Dummy unspecified multiple injuries"       ,
    "T07XXXA" , "1"       , "Dummy unsp inj, init"    , "Dummy unspecified multiple injuries, initial encounter"
  )
}

dummy_icd10cm_lines <- function(rows = dummy_icd10cm_rows()) {
  icd10cm_order_line(
    seq_len(nrow(rows)),
    rows$code,
    rows$billable,
    rows$short,
    rows$long
  )
}

# Write a dummy order file to a scoped temp directory and return its path.
# `name` is the file name without extension; with `zip = TRUE` the order file
# (plus an addenda file, which must be ignored) is wrapped in `<name>.zip`
# under a subdirectory, as in the NCHS zips.
create_dummy_icd10cm_file <- function(
  zip = FALSE,
  name = "icd10cm_order_20261001",
  lines = dummy_icd10cm_lines(),
  sep = "\n",
  .envir = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = .envir)

  if (!zip) {
    path <- file.path(dir, paste0(name, ".txt"))
    writeLines(lines, path, sep = sep)
    return(path)
  }

  inner_dir <- file.path(dir, "Code Descriptions")
  dir.create(inner_dir)
  writeLines(lines, file.path(inner_dir, "icd10cm_order_2027.txt"), sep = sep)
  writeLines(
    "Add:         1 Z999    Dummy addendum",
    file.path(inner_dir, "icd10cm_order_addenda_2027.txt")
  )

  zip_path <- file.path(dir, paste0(name, ".zip"))
  withr::with_dir(
    dir,
    utils::zip(
      zipfile = basename(zip_path),
      files = basename(inner_dir),
      flags = "-r9Xq"
    )
  )
  zip_path
}
