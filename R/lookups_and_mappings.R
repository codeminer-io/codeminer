# EXPORTED ----------------------------------------------------------------

#' Build a Duckdb database of clinical code look up and mapping tables
#'
#' Write the output from \code{\link{build_all_lkps_maps}} to a Duckdb database.
#'
#' @param all_lkps_maps A named list of look up and mapping tables, created
#'   by \code{\link{build_all_lkps_maps}}.
#' @param db_path Where the database will be created. If an Duckdb database file
#'   already exists here, then the lookup and mapping tables will be added to
#'   this. If \code{NULL} (default), then no database will be created/modified.
#' @param overwrite If \code{TRUE}, overwrite tables in the database if they
#'   already exist. Default value is \code{FALSE}.
#'
#' @return Returns \code{db_path} invisibly
#' @seealso [build_all_lkps_maps()]
#' @export
#' @examples
#' # build dummy all_lkps_maps resource (supressing warning messages)
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#'
#' # write to Duckdb database file
#' db_path <- suppressMessages(
#'   all_lkps_maps_to_db(
#'     all_lkps_maps = all_lkps_maps_dummy,
#'     db_path = tempfile()
#'   )
#' )
#'
#' # connect to Duckdb database
#' con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
#' on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
#'
#' # create named list of tbl_dbi objects
#' all_lkps_maps_dummy_db <- ukbwranglr::db_tables_to_list(con)
#'
#' head(all_lkps_maps_dummy_db$icd10_lkp)
#'
#' # import to R with dplyr::collect()
#' dplyr::collect(all_lkps_maps_dummy_db$icd10_lkp)
all_lkps_maps_to_db <- function(all_lkps_maps = build_all_lkps_maps(),
                                db_path = "all_lkps_maps.db",
                                overwrite = FALSE) {
  # If database already exists at db_path, check if tables to be written are
  # already present
  if (file.exists(db_path)) {
    message(paste0(
      "Existing file found at ",
      db_path
    ))

    check_tables_do_not_already_exist <- TRUE
  } else {
    check_tables_do_not_already_exist <- FALSE
  }

  # connect to db
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  if (check_tables_do_not_already_exist) {
    tables_to_be_written <- c(
      CLINICAL_CODE_MAPPINGS_MAP$mapping_table,
      CODE_TYPE_TO_LKP_TABLE_MAP$lkp_table
    )

    tables_already_in_db <- DBI::dbListTables(con)

    tables_already_present_in_db <- subset(
      tables_to_be_written,
      tables_to_be_written %in% tables_already_in_db
    )

    if (!overwrite) {
      assertthat::assert_that(
        rlang::is_empty(tables_already_present_in_db),
        msg = paste0(
          "Error! The following tables are already present in the database at ",
          db_path,
          ": ",
          stringr::str_c(
            tables_already_present_in_db,
            sep = "",
            collapse = ", "
          )
        )
      )
    } else if (overwrite &
      !rlang::is_empty(tables_already_present_in_db)) {
      warning(
        "The following tables are already present in the database at ",
        db_path,
        " and will be overwritten: ",
        stringr::str_c(
          tables_already_present_in_db,
          sep = "",
          collapse = ", "
        )
      )
    }
  }

  # write to db
  message(paste0(
    "Writing lookup and mapping tables to Duckdb database at ",
    db_path
  ))
  for (table_name in names(all_lkps_maps)) {
    message(table_name)
    DBI::dbWriteTable(
      conn = con,
      name = table_name,
      value = all_lkps_maps[[table_name]],
      # ensure table is not inadvertently overwritten/appended to
      overwrite = overwrite,
      append = FALSE
    )
  }

  message(paste0(
    "Success! Connect to database with `con <- DBI::dbConnect(duckdb::duckdb(), '",
    db_path,
    "', read_only = TRUE)`, then access all tables with `all_lkps_maps <- ukbwranglr::db_tables_to_list(con)`"
  ))
  invisible(db_path)
}

#' Build named list of clinical code look up and mapping tables
#'
#' Downloads the lookup and mapping tables from
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UK Biobank resource
#' 592} as well as the NHSBSA BNF-SNOMED mapping table (available
#' \href{https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping}{here})
#' and OPCS4, self-reported medical conditions/medications/operations from the
#' UK Biobank codings file (available
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{here}).
#'
#' @param all_lkps_maps UK Biobank resource 592, as returned by
#'   \code{\link{get_ukb_all_lkps_maps}}.
#' @param ukb_codings The UK Biobank codings file, as returned by
#'   \code{\link[ukbwranglr]{get_ukb_codings}}.
#' @param bnf_dmd Optional: path to the NHSBSA BNF-SNOMED mapping table (see
#'   [get_nhsbsa_snomed_bnf()]).
#' @param self_report_med_to_atc_map Optional: path to a UK Biobank
#'   self-reported medication to ATC map (see
#'   [get_ukb_self_report_med_to_atc_map()]).
#' @param snomed_ct_uk_monolith Optional: path to the unzipped [SNOMED CT UK
#'   Monolith
#'   Edition](https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases).
#' @param phecode_1_2_lkp Optional: path to the phecode v1.2 lookup file (see
#'   [get_phecode_definitions()]).
#' @param icd10_phecode_1_2 Optional: path to the phecode v1.2 to ICD10 mapping
#'   file (see [get_phecode_icd10_map()]).
#' @param icd9_phecode_1_2 Optional: path to the phecode v1.2 to ICD10 mapping
#'   file (see [get_phecode_icd9_map()]).
#' @param snomed_ct_nhs_data_migration Optional: path to the unzipped [NHS Data
#'   Migration](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases).
#' @param cprd_aurum_codebrowser_dir Optional: path to the unzipped CPRD Aurum
#'   CodeBrowser folder. The folder name should contain the release date e.g.
#'   "CPRD_CodeBrowser_202412_Aurum".
#'
#' @return Returns a named list of data frames.
#' @seealso [all_lkps_maps_to_db()]
#' @export
#' @examples
#' # build dummy all_lkps_maps using just UKB resource 592 and UKB codings file
#' build_all_lkps_maps(
#'   all_lkps_maps = read_all_lkps_maps_dummy(),
#'   ukb_codings = read_ukb_codings_dummy(),
#'   bnf_dmd = NULL,
#'   self_report_med_to_atc_map = NULL,
#'   snomed_ct_uk_monolith = NULL,
#'   phecode_1_2_lkp = NULL,
#'   icd10_phecode_1_2 = NULL,
#'   icd9_phecode_1_2 = NULL
#' )
build_all_lkps_maps <-
  function(all_lkps_maps = read_all_lkps_maps(),
           ukb_codings = ukbwranglr::get_ukb_codings(),
           bnf_dmd = get_nhsbsa_snomed_bnf(),
           self_report_med_to_atc_map = get_ukb_self_report_med_to_atc_map(),
           phecode_1_2_lkp = get_phecode_definitions(),
           icd10_phecode_1_2 = get_phecode_icd10_map(),
           icd9_phecode_1_2 = get_phecode_icd9_map(),
           snomed_ct_uk_monolith = NULL,
           snomed_ct_nhs_data_migration = NULL,
           cprd_aurum_codebrowser_dir = NULL) {
    # ukb resource 592 ----------------

    ## reformat tables individually ---------------

    ### icd10 ------------------------

    # Some ICD-10 descriptions include a modifier e.g. "E10" = "Type 1 diabetes
    # mellitus", whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With
    # coma" is contained in the modifier columns "MODIFIER-4". See 'S27' for an
    # example code where additional description is contained in the "MODIFER-5"
    # column. There are no codes with a modifier description in
    # both "MODIFIER_4" and "MODIFIER_5".

    all_lkps_maps$icd10_lkp <- all_lkps_maps$icd10_lkp %>%
      dplyr::mutate("DESCRIPTION" = dplyr::case_when(
        !is.na(MODIFIER_4) ~ paste(DESCRIPTION, MODIFIER_4),
        !is.na(MODIFIER_5) ~ paste(DESCRIPTION, MODIFIER_5),
        TRUE ~ DESCRIPTION
      ))

    ### icd9_icd10 -------------------

    all_lkps_maps$icd9_icd10 <- reformat_icd9_icd10(all_lkps_maps$icd9_icd10)

    ### read2_icd10 ------------------------

    all_lkps_maps$read_v2_icd10 <- reformat_read_v2_icd10(all_lkps_maps$read_v2_icd10,
      icd10_lkp = all_lkps_maps$icd10_lkp
    )

    ### read3_icd10 ------------------------

    all_lkps_maps$read_ctv3_icd10 <- reformat_read_ctv3_icd10(all_lkps_maps$read_ctv3_icd10)

    ## extend tables -----------------
    message("Extending tables in UKB resource 592")
    all_lkps_maps$read_v2_drugs_bnf <-
      extend_read_v2_drugs_bnf(all_lkps_maps)
    all_lkps_maps$bnf_lkp <-
      extend_bnf_lkp(all_lkps_maps)

    # opcs4 -------------------------

    ## from ukb codings file
    opcs4_lkp <- make_lkp_from_ukb_codings(
      ukb_codings = ukb_codings,
      Coding = "240",
      Value_col_new_name = "opcs4_code"
    )

    # UKB self-reported medical conditions/medications/operations ------------

    ## from ukb codings file
    self_report_cancer <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "3",
        Value_col_new_name = "data_coding_3"
      )

    self_report_medication <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "4",
        Value_col_new_name = "data_coding_4"
      )

    self_report_operation <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "5",
        Value_col_new_name = "data_coding_5"
      )

    self_report_non_cancer <-
      make_lkp_from_ukb_codings(
        ukb_codings = ukb_codings,
        Coding = "6",
        Value_col_new_name = "data_coding_6"
      )

    # Add 'extra' tables -------------------------

    ## NHSBSA-SNOMED BNF -----------------------------

    if (!is.null(bnf_dmd)) {
      message("Reformatting bnf_dmd map")
      # read file and tidy names
      bnf_dmd_map_unzipped_dir <- file.path(tempdir(), "bnf_dmd")

      utils::unzip(bnf_dmd,
        files = NULL,
        exdir = bnf_dmd_map_unzipped_dir
      )

      bnf_dmd_file <- list.files(bnf_dmd_map_unzipped_dir)
      assertthat::assert_that(length(bnf_dmd_file) == 1,
        msg = "Error! Unexpected number of files after unzipping NHBSA BNF-SNOMED file"
      )

      bnf_dmd <-
        readxl::read_excel(file.path(bnf_dmd_map_unzipped_dir, bnf_dmd_file))

      names(bnf_dmd) <-
        ukbwranglr:::remove_special_characters_and_make_lower_case(names(bnf_dmd)) %>%
        stringr::str_remove_all("plus_")
    }

    ## Self-reported med to ATC map ------------

    if (!is.null(self_report_med_to_atc_map)) {
      message("Reformatting self_report_med_to_atc_map")
      # read file
      self_report_med_to_atc_map <- readxl::read_excel(
        self_report_med_to_atc_map,
        skip = 2,
        col_names = c(
          "self_report_medication",
          "data_coding_4",
          "atc_code",
          "drug_name",
          "rm_1",
          "rm_2"
        )
      )

      # drop redundant cols
      self_report_med_to_atc_map <- self_report_med_to_atc_map[, -c(5, 6)]

      # append drug_name in brackets
      self_report_med_to_atc_map$self_report_medication <-
        paste0(
          self_report_med_to_atc_map$self_report_medication,
          " (",
          self_report_med_to_atc_map$drug_name,
          ")"
        )
    }

    ## SNOMED CT UK Monolith Edition ---------
    if (!is.null(snomed_ct_uk_monolith)) {

      snomed_ct_uk_monolith_full <-
        read_snomed_ct_uk_monolith(snomed_ct_uk_monolith_dir = snomed_ct_uk_monolith)


      # TODO - tidy when more familiar with this dataset
      snomed_monolith_terminology <- snomed_ct_uk_monolith_full$snomed_monolith_terminology
      snomed_monolith_refset_Map <- snomed_ct_uk_monolith_full$snomed_monolith_refset$Map

      # rename - remove edition date from filenames
      names(snomed_monolith_terminology) <-
        stringr::str_replace(string = names(snomed_monolith_terminology),
                             pattern = "_GB_[0-9]+\\.txt",
                             ".txt")

      names(snomed_monolith_refset_Map) <-
        stringr::str_replace(string = names(snomed_monolith_refset_Map),
                             pattern = "_GB_[0-9]+\\.txt",
                             ".txt")
    }

    ## SNOMED CT NHS Data Migration ------

    if (!is.null(snomed_ct_nhs_data_migration)) {

      nhs_data_migration_mapping_tables <-
        read_snomed_ct_nhs_data_migration_mapping_tables(nhs_data_migration_dir = snomed_ct_nhs_data_migration)
    }

    ## CPRD Aurum --------------------

    if (!is.null(cprd_aurum_codebrowser_dir)) {
      cprd_aurum_medcodes_and_prodcodes <- read_cprd_aurum_codebrowser_dir(cprd_aurum_codebrowser_dir)
    }

    ## Phecode lookup ----------------
    if (!is.null(phecode_1_2_lkp)) {
      phecode_lkp <- readr::read_csv(phecode_1_2_lkp,
        progress = FALSE,
        col_types = readr::cols(.default = "c")
      )
    }

    ## Phecode to ICD10 map ---------------------
    if (!is.null(icd10_phecode_1_2)) {
      message("Reformatting ICD10-Phecode map")
      icd10_phecode <- readr::read_csv(icd10_phecode_1_2,
        progress = FALSE,
        col_types = readr::cols(.default = "c")
      )

      icd10_phecode <- reformat_icd10_phecode_map_1_2(icd10_phecode,
        all_lkps_maps = all_lkps_maps
      )
    }

    ## Phecode to ICD9 map ------------------
    message("Reformatting ICD9-Phecode map")
    if (!is.null(icd9_phecode_1_2)) {
      icd9_phecode <- readr::read_csv(icd9_phecode_1_2) %>%
        dplyr::mutate("icd9" = stringr::str_remove(.data[["icd9"]],
          pattern = "\\."
        ))
    }

    # Combine -----------------------
    all_lkps_maps <- c(
      all_lkps_maps,
      list(
        opcs4_lkp = opcs4_lkp,
        self_report_cancer = self_report_cancer,
        self_report_medication = self_report_medication,
        self_report_operation = self_report_operation,
        self_report_non_cancer = self_report_non_cancer
      )
    )

    # Append 'extra' lookup/mapping tables
    if (!is.null(bnf_dmd)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(bnf_dmd = bnf_dmd)
      )
    }

    if (!is.null(self_report_med_to_atc_map)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(self_report_med_to_atc_map = self_report_med_to_atc_map)
      )
    }

    if (!is.null(snomed_ct_uk_monolith)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(
          # merge description and concept tables - these indicate whether the
          # description and code respectively are currently active
          sct_description = snomed_monolith_terminology$`sct2_Description_MONOSnapshot-en.txt` %>%
            dplyr::rename_with(\(x) paste0(x, "_description")) %>%
            dplyr::full_join(
              snomed_monolith_terminology$sct2_Concept_MONOSnapshot.txt %>%
                dplyr::rename_with(\(x) paste0(x, "_concept")),
              by = c("conceptId_description" = "id_concept")
            ) %>%
            dplyr::rename("conceptId" = "conceptId_description"),
          sct_relationship = snomed_monolith_terminology$sct2_Relationship_MONOSnapshot.txt,
          sct_icd10 = snomed_monolith_refset_Map$der2_iisssciRefset_ExtendedMapMONOSnapshot.txt %>%
            dplyr::filter(.data[["refsetId"]] == "999002271000000101") %>%
            dplyr::filter(stringr::str_detect(.data[["mapTarget"]],
                                              "#",
                                              negate = TRUE)) %>%

            # separate asterisk/dagger flag from ICD-10 codes
            dplyr::mutate(
              "asterisk_dagger" = dplyr::case_when(
                stringr::str_detect(.data[["mapTarget"]],
                                    stringr::regex("A$")) ~ "A",
                stringr::str_detect(.data[["mapTarget"]],
                                    stringr::regex("D$")) ~ "D",
                TRUE ~ NA_character_
              )
            ) %>%
            dplyr::mutate("mapTarget" = stringr::str_trim(
              stringr::str_remove(.data[["mapTarget"]], stringr::regex("[A|D]$"))
            )),
          sct_opcs4 = snomed_monolith_refset_Map$der2_iisssciRefset_ExtendedMapMONOSnapshot.txt %>%
            dplyr::filter(.data[["refsetId"]] == "1126441000000105") %>%
            dplyr::filter(stringr::str_detect(.data[["mapTarget"]],
                                              "#",
                                              negate = TRUE)),
          metadata_snomed_ct_uk_monolith = data.frame(metadata = fs::path_file(snomed_ct_uk_monolith))
        )
      )
    }

    if (!is.null(snomed_ct_nhs_data_migration)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(
          ctv3sctmap2 = nhs_data_migration_mapping_tables$clinically_assured$ctv3sctmap2_uk_20200401000001.txt,
          rcsctmap2 = nhs_data_migration_mapping_tables$clinically_assured$rcsctmap2_uk_20200401000001.txt,
          metadata_snomed_ct_nhs_data_migration = data.frame(metadata = fs::path_file(snomed_ct_nhs_data_migration))
        )
      )
    }

    if (!is.null(cprd_aurum_codebrowser_dir)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        cprd_aurum_medcodes_and_prodcodes
      )
    }

    if (!is.null(phecode_1_2_lkp)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(phecode_lkp = phecode_lkp)
      )
    }

    if (!is.null(icd10_phecode_1_2)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(icd10_phecode = icd10_phecode)
      )
    }

    if (!is.null(icd9_phecode_1_2)) {
      all_lkps_maps <- c(
        all_lkps_maps,
        list(icd9_phecode = icd9_phecode)
      )
    }

    # add metadata for codemapper version
    all_lkps_maps <- c(
      all_lkps_maps,
      list(
        metadata_codemapper = tibble::as_tibble(utils::installed.packages()) %>%
          dplyr::filter(.data[["Package"]] == "codemapper") %>%
          tidyr::unite(col = "metadata",
                       tidyselect::all_of(c(
                         "Package", "Version", "Built"
                       )),
                       sep = "|") %>%
          dplyr::select(tidyselect::all_of("metadata"))
      )
    )

    # convert all to tibbles (avoids potential problems when writing to Duckdb
    # database)
    all_lkps_maps <- all_lkps_maps %>%
      purrr::map(tibble::as_tibble)

    message("Success!")
    return(all_lkps_maps)
  }

#' Download and read the NHSBSA BNF_SNOMED mapping file
#'
#' Mapping table available from
#' \href{https://www.nhsbsa.nhs.uk/prescription-data/understanding-our-data/bnf-snomed-mapping}{here}.
#'
#' @param path Path where file will be downloaded to.
#'
#' @return File path to downloaded file.
#' @export
#' @examples
#' \dontrun{
#' get_nhsbsa_snomed_bnf()
#' }
get_nhsbsa_snomed_bnf <- function(path = file.path(
                                    tempdir(),
                                    "bnf_dmd.zip"
                                  )) {
  download_file(
    download_url = "https://www.nhsbsa.nhs.uk/sites/default/files/2025-01/BNF%20Snomed%20Mapping%20data%2020250120.zip",
    path = path
  )
}

#' Get UK Biobank resource 592 directly from UKB
#' website
#'
#' Downloads the UK Biobank code mappings file (\code{all_lkps_maps_v4.xlsx},
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592})
#' directly from the UKB website.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @param dir_path Directory path to download to.
#'
#' @return File path to downloaded `all_lkps_maps_v4.xlsx`.
#' @export
#' @examples
#' \dontrun{
#' # download UKB resource 592, returning file path invisibly
#' file_path <- get_ukb_all_lkps_maps()
#'
#' # view path to downloaded file
#' file_path
#' }
get_ukb_all_lkps_maps <- function(dir_path = tempdir()) {
  message("Getting UKB resource 592")
  # name of resource 592 excel file
  primarycare_codings <- "all_lkps_maps_v4.xlsx"

  # filepaths in tempdir
  primarycare_codings_zip_filepath <-
    file.path(tempdir(), "primarycare_codings.zip")
  primarycare_codings_excel_filepath <-
    file.path(tempdir(), primarycare_codings)

  # download primary care codings file to tempdir, if not already there
  if (!file.exists(primarycare_codings_zip_filepath)) {
    message("Downloading primarycare_codings.zip (UKB resource 592) to tempdir")
    utils::download.file(
      "https://biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip",
      primarycare_codings_zip_filepath,
      mode = "wb"
    )
  }

  # extract excel file only from zip
  message("Extracting all_lkps_maps_v4.xlsx from zip file to tempdir")
  utils::unzip(primarycare_codings_zip_filepath,
    files = primarycare_codings,
    exdir = dir_path
  )

  # return file path
  return(file.path(dir_path, primarycare_codings))
}

#' Download and read a UKB welf-reported medication code to ATC mapping file
#'
#' Mapping table obtained from [Wray et al
#' 2019](https://www.nature.com/articles/s41467-019-09572-5#Sec23),
#' Supplementary Data 1.
#'
#' @param path Path where file will be downloaded to.
#'
#' @return File path to downloaded file.
#' @export
#' @examples
#' \dontrun{
#' get_nhsbsa_snomed_bnf()
#' }
get_ukb_self_report_med_to_atc_map <- function(path = file.path(
                                                 tempdir(),
                                                 "self_report_med_to_atc_map.xlsx"
                                               )) {
  download_file(
    download_url = "https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-019-09572-5/MediaObjects/41467_2019_9572_MOESM3_ESM.xlsx",
    path = path
  )
}

#' Read UK Biobank resource 592 into a named list
#'
#' Reads the UK Biobank code mappings file (`all_lkps_maps_v4.xlsx`,
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}) into
#' a named list of data frames.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @param path Path to `all_lkps_maps_v4.xlsx`. By default, this is downloaded
#'   from the UK Biobank website using [get_ukb_all_lkps_maps()].
#'
#' @return A named list of data frames.
#' @export
#' @examples
#' read_all_lkps_maps(dummy_all_lkps_maps_path())
read_all_lkps_maps <- function(path = get_ukb_all_lkps_maps()) {
  tables <- read_excel_to_named_list(
    path = path,
    to_include = NULL,
    to_exclude = c("Description", "Contents"),
    col_types = "text"
  ) %>%
    purrr::map(rm_footer_rows_all_lkps_maps_df) %>%
    purrr::map(~ tibble::rowid_to_column(
      .data = .x,
      var = ".rowid"
    ))

  description_contents <- suppressMessages(read_excel_to_named_list(
    path = path,
    to_include = c("Description", "Contents"),
    to_exclude = NULL,
    col_types = "text"
  ))

  metadata <- description_contents$Contents[, 1] %>%
    stats::na.omit()

  names(metadata) <- "metadata"

  metadata <- dplyr::bind_rows(metadata,
                               data.frame(metadata = subset(
                                 names(description_contents$Description),
                                 stringr::str_detect(names(description_contents$Description), "Version")
                               ) %>%
                                 stringr::str_extract("Version.*$")))

  result <- c(list(metadata_all_lkps_maps = metadata),
              tables)

  return(result)
}

#' Download the Phecode 1.2 definitions file
#'
#' Download link obtained from https://phewascatalog.org/phecodes.
#'
#' @param path Path where file will be downloaded to.
#'
#' @return File path to downloaded file.
#' @export
#' @examples
#' \dontrun{
#' get_phecode_definitions()
#' }
get_phecode_definitions <- function(path = file.path(
                                      tempdir(),
                                      "phecode_definitions1.2.csv.zip"
                                    )) {
  download_file(
    download_url = "https://phewascatalog.org/files/phecode_definitions1.2.csv.zip",
    path = path
  )
}

#' Download the Phecode 1.2 to ICD9 mapping file
#'
#' Download link obtained from https://phewascatalog.org/phecodes.
#'
#' @param path Path where file will be downloaded to.
#'
#' @return File path to downloaded file.
#' @export
#' @examples
#' \dontrun{
#' get_phecode_icd9_map()
#' }
get_phecode_icd9_map <- function(path = file.path(
                                   tempdir(),
                                   "phecode_icd9_map_unrolled.csv.zip"
                                 )) {
  download_file(
    download_url = "https://phewascatalog.org/files/phecode_icd9_map_unrolled.csv.zip",
    path = path
  )
}

#' Download the Phecode 1.2 to ICD10 (beta) mapping file
#'
#' Download link obtained from https://phewascatalog.org/phecodes.
#'
#' @param path Path where file will be downloaded to.
#'
#' @return File path to downloaded file.
#' @export
#' @examples
#' \dontrun{
#' get_phecode_icd10_map()
#' }
get_phecode_icd10_map <- function(path = file.path(
                                    tempdir(),
                                    "Phecode_map_v1_2_icd10_beta.csv.zip"
                                  )) {
  download_file(
    download_url = "https://phewascatalog.org/files/Phecode_map_v1_2_icd10_beta.csv.zip",
    path = path
  )
}

# PRIVATE -----------------------------------------------------------------

#' Reformat the Read 2 to ICD10 mapping table
#'
#' Converts values in the `icd10_code` column to 'ALT_CODE' format ICD10 codes
#' that are recognised in the `icd10_lkp` lookup table. This involves dividing
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
#' converts appends 'X' to these codes to match how they appear in the
#' `icd10_lkp` table.
#'
#' ICD10 code 'C836' ('Diffuse non-Hodkin's lymphoma - Undifferentiated
#' (diffuse)') is also removed, as this code has been removed from ICD10 and does
#' not exist in the `icd10_lkp` table.
#'
#' @param read_v2_icd10 The Read 2 to ICD10 mapping table.
#' @param icd10_lkp Data frame. ICD10 lookup table (note, must have a '.rowid'
#'   column).
#'
#' @return A data frame.
#' @noRd
reformat_read_v2_icd10 <- function(read_v2_icd10,
                                   icd10_lkp) {

  # Undivided 3 character ICD10 ALT_CODE X/no X map (as named list)
  icd10_lkp_alt_x_map <- get_icd10_code_alt_code_x_map(
    icd10_lkp = icd10_lkp,
    undivided_3char_only = TRUE,
    as_named_list = "names_no_x"
  )

  # remove icd10_code 'C836' (see notes above)
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::filter(.data[["icd10_code"]] != "C836")

  # replace spaces and '+' with commas
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = stringr::str_replace_all(.data[["icd10_code"]],
      pattern = "[\\s|\\+]",
      replacement = ","
    ))

  # split by comma, then unnest
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = stringr::str_split(.data[["icd10_code"]],
      pattern = ","
    )) %>%
    tidyr::unnest(cols = "icd10_code")

  # remove 'D' and 'A' final characters from ICD10 codes, and place in separate
  # column `icd10_dagger_asterisk`
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate(
      "icd10_dagger_asterisk" = rm_or_extract_appended_icd10_dxa(
        icd10_codes = .data[["icd10_code"]],
        keep_x = TRUE,
        rm_extract = "extract"
      )
    ) %>%
    dplyr::mutate("icd10_code" = rm_or_extract_appended_icd10_dxa(
      icd10_codes = .data[["icd10_code"]],
      keep_x = TRUE,
      rm_extract = "rm"
    ))

  # expand icd10 code ranges, which are flagged as '2' under `icd10_code_def` (e.g. 'E100-E109')
  read_v2_icd10 <- read_v2_icd10 %>%
    expand_icd10_ranges(
      icd10_lkp = icd10_lkp,
      icd10_lkp_alt_x_map = icd10_lkp_alt_x_map
    )

  # Make sure undivided 3 character ICD10 codes have an 'X' appended
  read_v2_icd10 <- read_v2_icd10 %>%
    dplyr::mutate("icd10_code" = dplyr::recode(.data[["icd10_code"]], !!!icd10_lkp_alt_x_map))

  # check all ICD10 codes now exist in `icd10_lkp`
  check_codes_exist(
    codes = unique(read_v2_icd10$icd10_code),
    lkp_codes = icd10_lkp$ALT_CODE,
    code_type = "icd10"
  )

  # return result
  return(read_v2_icd10)
}

#' Reformat mapping table `read_ctv3_icd10`
#'
#' Removes 'D' and 'A' from the ends of ICD10 codes, and separates these into a
#' separate column called `icd10_dagger_asterisk`. The 'D' and 'A' indicate
#' whether the code is a 'dagger' or 'asterisk' respectively. However, these
#' codes are listed without the appended 'D'/'A' in the `icd10_lkp` table.
#'
#' @param read_ctv3_icd10 A data frame.
#'
#' @return A data frame
#' @noRd
reformat_read_ctv3_icd10 <- function(read_ctv3_icd10) {
  read_ctv3_icd10 %>%
    dplyr::mutate(
      "icd10_dagger_asterisk" = rm_or_extract_appended_icd10_dxa(
        icd10_codes = .data[["icd10_code"]],
        keep_x = TRUE,
        rm_extract = "extract"
      )
    ) %>%
    dplyr::mutate("icd10_code" = rm_or_extract_appended_icd10_dxa(
      icd10_codes = .data[["icd10_code"]],
      keep_x = TRUE,
      rm_extract = "rm"
    ))
}

#' Reformat mapping table `icd9_icd10`
#'
#' For ICD9 codes without an equivalent ICD10 code, the ICD10 code is recorded
#' as 'UNDEF', with `NA` for the description (and vice versa). This function
#' converts values of 'UNDEF' in the `ICD9` and `ICD10` columns to `NA`, and
#' checks that `NA` only appears for such cases (i.e. a ICD code cannot be `NA`
#' if it has a description and vice versa).
#'
#' @param icd9_icd10 The `icd9_icd10` mapping table
#'
#' @return A data frame
#' @noRd
reformat_icd9_icd10 <- function(icd9_icd10) {
  # convert 'UNDEF' ICD9/10 codes to `NA`
  icd9_icd10 <- icd9_icd10 %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(c(
        "ICD9",
        "ICD10"
      )),
      ~ ifelse(.x == "UNDEF",
        yes = NA_character_,
        no = .x
      )
    ))

  # check that description and ICD code are either both `NA` or both not `NA`
  icd9_icd10_nabular <- icd9_icd10 %>%
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ is.na(.x)
    ))

  assertthat::assert_that(all(icd9_icd10_nabular$ICD9 == icd9_icd10_nabular$DESCRIPTION_ICD9))
  assertthat::assert_that(all(icd9_icd10_nabular$ICD10 == icd9_icd10_nabular$DESCRIPTION_ICD10))

  # return result
  return(icd9_icd10)
}

reformat_icd10_phecode_map_1_2 <- function(icd10_phecode,
                                           all_lkps_maps) {

  # get vector of all present icd10 codes in ALT_CODE format. Also returns a
  # message listing ICD10 codes with modifiers that will map to >1 ICD10 code in
  # ALT_CODE format. Also raises warning if any unrecognised ICD10 codes are
  # present.
  icd10_codes_in_icd10_phecode <- reformat_icd10_codes(
    icd10_codes = icd10_phecode$ICD10,
    all_lkps_maps = all_lkps_maps,
    input_icd10_format = "ICD10_CODE",
    output_icd10_format = "ALT_CODE",
    unrecognised_codes = "warning",
    strip_x = FALSE
  )

  # append `ALT_CODE`
  icd10_phecode <- all_lkps_maps$icd10_lkp %>%
    dplyr::select(tidyselect::all_of(c(
      "ICD10_CODE",
      "ALT_CODE"
    ))) %>%
    dplyr::filter(.data[["ALT_CODE"]] %in% !!icd10_codes_in_icd10_phecode) %>%
    dplyr::collect() %>%
    dplyr::right_join(icd10_phecode,
      by = c("ICD10_CODE" = "ICD10"),
      relationship = "many-to-many"
    )

  # remove empty PHECODE rows
  icd10_phecode <- icd10_phecode %>%
    dplyr::filter(!is.na(.data[["PHECODE"]]))

  # result
  return(icd10_phecode)
}

extend_bnf_lkp <- function(all_lkps_maps) {
  # each drug entry in `bnf_lkp` gets repeated 8 times: chapter, section,
  # paragraph, subparagraph, chemical_substance, product_name, further_info,
  # full

  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  bnf_lkp %>%
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
    ) %>%
    dplyr::rename("code_full" = tidyselect::all_of("BNF_Presentation_Code")) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("code"),
      names_to = "BNF_Code_Level",
      values_to = "BNF_Code"
    ) %>%
    dplyr::select(
      tidyselect::all_of("BNF_Code_Level"),
      tidyselect::all_of("BNF_Code"),
      tidyselect::everything()
    ) %>%
    dplyr::mutate("BNF_Code_Level" = stringr::str_remove(
      .data[["BNF_Code_Level"]],
      "code_"
    )) %>%
    dplyr::distinct(.data[["BNF_Code"]],
      .keep_all = TRUE
    ) %>%
    dplyr::mutate(
      "Description" = dplyr::case_when(
        .data[["BNF_Code_Level"]] == "chapter" ~ .data[["BNF_Chapter"]],
        .data[["BNF_Code_Level"]] == "section" ~ .data[["BNF_Section"]],
        .data[["BNF_Code_Level"]] == "paragraph" ~ .data[["BNF_Paragraph"]],
        .data[["BNF_Code_Level"]] == "subparagraph" ~ .data[["BNF_Subparagraph"]],
        .data[["BNF_Code_Level"]] == "chemical_substance" ~ .data[["BNF_Chemical_Substance"]],
        .data[["BNF_Code_Level"]] == "product_name" ~ .data[["BNF_Product"]],
        .data[["BNF_Code_Level"]] == "further_info" ~ .data[["BNF_Presentation"]],
        .data[["BNF_Code_Level"]] == "full" ~ .data[["BNF_Presentation"]]
      )
    ) %>%
    dplyr::mutate(
      "BNF_Presentation" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Presentation"]]
      ),
      "BNF_Product" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info",
          "product_name"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Product"]]
      ),
      "BNF_Chemical_Substance" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info",
          "product_name",
          "chemical_substance"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Chemical_Substance"]]
      ),
      "BNF_Subparagraph" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info",
          "product_name",
          "chemical_substance",
          "subparagraph"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Subparagraph"]]
      ),
      "BNF_Paragraph" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info",
          "product_name",
          "chemical_substance",
          "subparagraph",
          "paragraph"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Paragraph"]]
      ),
      "BNF_Section" = dplyr::case_when(
        !.data[["BNF_Code_Level"]] %in% c(
          "full",
          "further_info",
          "product_name",
          "chemical_substance",
          "subparagraph",
          "paragraph",
          "section"
        ) ~ as.character(NA),
        TRUE ~ .data[["BNF_Section"]]
      ),
    ) %>%
    dplyr::select(tidyselect::all_of(c(
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
}

extend_read_v2_drugs_bnf <- function(all_lkps_maps) {
  # get required tables
  read_v2_drugs_bnf <- all_lkps_maps[["read_v2_drugs_bnf"]] %>%
    dplyr::collect()
  read_v2_drugs_lkp <- all_lkps_maps[["read_v2_drugs_lkp"]] %>%
    dplyr::collect()
  bnf_lkp <- all_lkps_maps[["bnf_lkp"]] %>%
    dplyr::collect()

  # extend 'bnf_lkp'
  bnf_lkp_extended <- extend_bnf_lkp(all_lkps_maps)

  # extend `read_v2_drugs_bnf`
  expected_nrow <- nrow(read_v2_drugs_bnf)

  result <- read_v2_drugs_bnf %>%
    # add read code descriptions
    dplyr::left_join(read_v2_drugs_lkp,
      by = "read_code"
    ) %>%
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
    ) %>%
    # add BNF details
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Chapter")],
      by = c("bnf_chapter_code" = "BNF_Code")
    ) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Section")],
      by = c("bnf_section_code" = "BNF_Code")
    ) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Paragraph")],
      by = c("bnf_paragraph_code" = "BNF_Code")
    ) %>%
    dplyr::left_join(bnf_lkp_extended[, c("BNF_Code", "BNF_Subparagraph")],
      by = c("bnf_subparagraph_code" = "BNF_Code")
    )

  # check nrows remains the same
  assertthat::assert_that(expected_nrow == nrow(result),
    msg = "Error! Unexpected number of rows when extending `read_v2_drugs_bnf`"
  )

  return(result)
}

#' Read the SNOMED CT UK Monolith Edition into R
#'
#' @param snomed_ct_uk_monolith_dir Optional: path to the unzipped [SNOMED CT UK
#'   Monolith
#'   Edition](https://isd.digital.nhs.uk/trud/users/guest/filters/2/categories/26/items/1799/releases).
#'
#' @return A list.
#' @noRd
read_snomed_ct_uk_monolith <- function(snomed_ct_uk_monolith_dir) {
  # terminology and refset tables
  snomed_monolith_terminology <-
    file.path(snomed_ct_uk_monolith_dir,
              "Snapshot",
              "Terminology") %>%
    list.files(full.names = TRUE) %>%
    purrr::set_names(nm = fs::path_file) %>%
    purrr::map( ~ data.table::fread(.x,
                                    sep = "\t",
                                    colClasses = "character"))

  snomed_monolith_refset <- file.path(snomed_ct_uk_monolith_dir,
                                      "Snapshot",
                                      "Refset") %>%
    list.files(full.names = TRUE) %>%
    purrr::set_names(nm = fs::path_file) %>%
    purrr::map(
      ~ .x %>%
        list.files(full.names = TRUE) %>%
        purrr::set_names(nm = fs::path_file) %>%
        purrr::map(~ data.table::fread(
          .x,
          sep = "\t",
          colClasses = "character"
        ))
    )

  return(
    list(
      snomed_monolith_terminology = snomed_monolith_terminology,
      snomed_monolith_refset = snomed_monolith_refset
    )
  )
}

#' Read the NHS Data Migration release into R
#'
#' @param nhs_data_migration_dir path to the unzipped [NHS Data
#'   Migration](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9/items/9/releases).
#'
#' @return A list.
#' @noRd
read_snomed_ct_nhs_data_migration_mapping_tables <- function(nhs_data_migration_dir) {
  # mapping tables for Read 2 and Read 3 to SNOMED 'clinically assured'
  file.path(nhs_data_migration_dir,
            "Mapping Tables",
            "Updated") %>%
    list.files(full.names = TRUE) %>%
    purrr::set_names(nm = \(x) fs::path_file(x) %>%
                       tolower() %>%
                       stringr::str_replace_all(" ", "_")) %>%
    purrr::discard_at(c("documentation", "not_clinically_assured")) %>%
    purrr::map(
      ~ .x %>%
        list.files(full.names = TRUE) %>%
        subset(.,
               fs::path_ext(.) == "txt") %>%
        # only select these 2 tables
        subset(
          .,
          fs::path_file(.) %in% c(
            "ctv3sctmap2_uk_20200401000001.txt",
            "rcsctmap2_uk_20200401000001.txt"
          )
        ) %>%
        purrr::set_names(nm = fs::path_file) %>%
        purrr::map(~ data.table::fread(
          .x,
          sep = "\t",
          colClasses = "character"
        ))
    )
}

read_cprd_aurum_codebrowser_dir <- function(cprd_aurum_codebrowser_dir) {
  # validate file paths
  stopifnot(dir.exists(cprd_aurum_codebrowser_dir))

  result <- list(
    cprd_prodcodes = file.path(cprd_aurum_codebrowser_dir, "CPRDAurumProduct.txt"),
    cprd_medcodes = file.path(cprd_aurum_codebrowser_dir, "CPRDAurumMedical.txt")
  )

  stopifnot(file.exists(result$cprd_prodcodes))
  stopifnot(file.exists(result$cprd_medcodes))

  # extract date from directory for metadata - TODO

  # read into named list
  result <- result |>
    purrr::map(\(.x) data.table::fread(
      .x,
      colClasses = c("character"),
      sep = "\t",
      quote = " ",
      na.strings = c("", "NA")
    ))

  # minimally reformat
  result$cprd_prodcodes <- result$cprd_prodcodes |>
    dplyr::rename_with(\(x) stringr::str_replace_all(x, " ", "_")) |>
    dplyr::mutate(DrugIssues = as.integer(DrugIssues))

  result$cprd_medcodes <- result$cprd_medcodes |>
    dplyr::mutate(Observations = as.integer(Observations))

  return(result)
}
