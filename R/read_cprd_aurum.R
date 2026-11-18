#' Read CPRD Aurum code browser files into R
#'
#' Reads the medical and product dictionaries from a CPRD Aurum code browser
#' release (for example `CPRD_CodeBrowser_YYYYMM_Aurum/`, containing
#' `CPRDAurumMedical.txt` and `CPRDAurumProduct.txt`) and returns lookup tables
#' for medcodes and prodcodes, plus mapping tables to SNOMED CT and Read v2.
#'
#' CPRD supplies the code browser to researchers with access to its data, and it
#' can't be downloaded publicly, so there is no `get_*()` function: supply the
#' path to your local copy.
#'
#' @section Columns:
#' The lookups keep every column of the dictionary files. Column names are
#' converted to lower case, with spaces replaced by underscores (`MedCodeId`
#' becomes `medcodeid`, `Term from EMIS` becomes `term_from_emis`).
#'
#' Column types are declared rather than guessed: `observations` and
#' `drugissues` are integer counts, and every other column, including the
#' numeric identifiers, is character. An error is raised if an expected column
#' is missing. A column the reader doesn't know about is read as character,
#' with a warning.
#'
#' @section Column filters:
#' The medcode lookup's `emiscodecategoryid`, and the prodcode lookup's
#' `routeofadministration` and `formulation`, are column filters (see
#' [lookup_metadata()]). Their values are taken from the release, and every
#' value is included by default, so no codes are hidden by default. The EMIS
#' code category IDs have no labels.
#'
#' @section Blank values:
#' Empty fields are read as `NA`, except in the column filter columns, which
#' keep them as empty strings (`""`). Filters match exact values and drop `NA`,
#' so the empty string is kept as a filter value in its own right.
#'
#' @section Mappings:
#' * **medcode to SNOMED CT** (`cprd_aurum_medcode_snomed`): from `medcodeid`
#'   to `snomedctconceptid`, keeping `snomedctdescriptionid`.
#' * **medcode to Read v2** (`cprd_aurum_medcode_read2`): from `medcodeid` to
#'   `read2_code`, the first 5 characters of `CleansedReadCode`, keeping the
#'   2-character `read2_term_code`. Rows without a cleansed Read code are
#'   dropped. `OriginalReadCode` is kept in the medcode lookup but not mapped.
#' * **prodcode to SNOMED CT** (`cprd_aurum_prodcode_snomed`): from `prodcodeid`
#'   to `dmdid`. dm+d identifiers are SNOMED CT concepts, so this links
#'   prodcodes to the dm+d hierarchy in SNOMED CT.
#'   Rows without a `dmdid` are dropped.
#'
#' Codes are not checked against the target code systems. A code that isn't in
#' the target lookup raises a warning when mapped.
#'
#' @param path Path to the CPRD Aurum code browser release directory,
#'   containing `CPRDAurumMedical.txt` and `CPRDAurumProduct.txt`. Only the
#'   files needed for `tables` must be present.
#' @param tables Character vector of tables to read. Any of
#'   `"cprd_aurum_medcode_lkp"`, `"cprd_aurum_prodcode_lkp"`,
#'   `"cprd_aurum_medcode_snomed"`, `"cprd_aurum_medcode_read2"` and
#'   `"cprd_aurum_prodcode_snomed"`. Defaults to all of them. The `medcode`
#'   tables come from `CPRDAurumMedical.txt` and the `prodcode` tables from
#'   `CPRDAurumProduct.txt`.
#' @param version Character string for the version label. If `NULL` (default),
#'   the release date (`yyyymm`) is taken from a directory name like
#'   `CPRD_CodeBrowser_YYYYMM_Aurum`, with a message saying which version is
#'   used. If the name doesn't include a release date, an error asks for
#'   `version` to be supplied.
#' @param source Character string for the data source.
#'
#' @return A named list with one element per requested table. Each holds
#'   `lookup` or `mapping`, a list with `table` (data.table) and `metadata`.
#'
#' @seealso [add_cprd_aurum()]
#' @export
#' @examples
#' \dontrun{
#' result <- read_cprd_aurum("path/to/CPRD_CodeBrowser_YYYYMM_Aurum")
#' result$cprd_aurum_medcode_lkp$lookup$table
#' }
read_cprd_aurum <- function(
  path,
  tables = cprd_aurum_tables(),
  version = NULL,
  source = "CPRD Aurum code browser"
) {
  tables <- rlang::arg_match(tables, cprd_aurum_tables(), multiple = TRUE)

  if (!dir.exists(path)) {
    codeminer_abort("Path does not exist or is not a directory: {.path {path}}")
  }

  if (is.null(version)) {
    version <- cprd_aurum_version(path)
  }

  needs_medical <- any(stringr::str_starts(tables, "cprd_aurum_medcode"))
  needs_product <- any(stringr::str_starts(tables, "cprd_aurum_prodcode"))

  result <- list()

  if (needs_medical) {
    codeminer_inform("Loading CPRD Aurum medical dictionary...")
    medical <- read_cprd_aurum_file(
      path,
      "CPRDAurumMedical.txt",
      cprd_aurum_medical_columns()
    ) |>
      blanks_to_empty_string("emiscodecategoryid") |>
      data.table::as.data.table()

    if ("cprd_aurum_medcode_lkp" %in% tables) {
      result$cprd_aurum_medcode_lkp <- list(
        lookup = list(
          table = medical,
          metadata = lookup_metadata(
            code_type = "CPRD Aurum medcode",
            lookup_version = version,
            lookup_code_col = "medcodeid",
            lookup_description_col = "term",
            lookup_source = source,
            col_filters = list(
              emiscodecategoryid = cprd_aurum_col_filter(
                medical$emiscodecategoryid,
                "EMIS code category ID. Includes every value by default."
              )
            )
          )
        )
      )
    }

    if ("cprd_aurum_medcode_snomed" %in% tables) {
      result$cprd_aurum_medcode_snomed <- list(
        mapping = list(
          table = medical |>
            dplyr::filter(!is.na(.data$snomedctconceptid)) |>
            dplyr::select(
              "medcodeid",
              "snomedctconceptid",
              "snomedctdescriptionid"
            ) |>
            data.table::as.data.table(),
          metadata = mapping_metadata(
            from_code_type = "CPRD Aurum medcode",
            to_code_type = "SNOMED CT",
            map_version = version,
            from_col = "medcodeid",
            to_col = "snomedctconceptid",
            map_source = source
          )
        )
      )
    }

    if ("cprd_aurum_medcode_read2" %in% tables) {
      result$cprd_aurum_medcode_read2 <- list(
        mapping = list(
          table = medical |>
            dplyr::filter(!is.na(.data$cleansedreadcode)) |>
            dplyr::mutate(
              read2_code = stringr::str_sub(.data$cleansedreadcode, 1, 5),
              read2_term_code = dplyr::na_if(
                stringr::str_sub(.data$cleansedreadcode, 6, 7),
                ""
              )
            ) |>
            dplyr::select("medcodeid", "read2_code", "read2_term_code") |>
            data.table::as.data.table(),
          metadata = mapping_metadata(
            from_code_type = "CPRD Aurum medcode",
            to_code_type = "Read v2",
            map_version = version,
            from_col = "medcodeid",
            to_col = "read2_code",
            map_source = source
          )
        )
      )
    }
  }

  if (needs_product) {
    codeminer_inform("Loading CPRD Aurum product dictionary...")
    product <- read_cprd_aurum_file(
      path,
      "CPRDAurumProduct.txt",
      cprd_aurum_product_columns()
    ) |>
      blanks_to_empty_string(c("routeofadministration", "formulation")) |>
      data.table::as.data.table()

    if ("cprd_aurum_prodcode_lkp" %in% tables) {
      result$cprd_aurum_prodcode_lkp <- list(
        lookup = list(
          table = product,
          metadata = lookup_metadata(
            code_type = "CPRD Aurum prodcode",
            lookup_version = version,
            lookup_code_col = "prodcodeid",
            lookup_description_col = "term_from_emis",
            lookup_source = source,
            col_filters = list(
              routeofadministration = cprd_aurum_col_filter(
                product$routeofadministration,
                "Route of administration. Includes every value by default; an empty string means no route is recorded."
              ),
              formulation = cprd_aurum_col_filter(
                product$formulation,
                "Formulation. Includes every value by default; an empty string means no formulation is recorded."
              )
            )
          )
        )
      )
    }

    if ("cprd_aurum_prodcode_snomed" %in% tables) {
      result$cprd_aurum_prodcode_snomed <- list(
        mapping = list(
          table = product |>
            dplyr::filter(!is.na(.data$dmdid)) |>
            dplyr::select("prodcodeid", "dmdid") |>
            data.table::as.data.table(),
          metadata = mapping_metadata(
            from_code_type = "CPRD Aurum prodcode",
            to_code_type = "SNOMED CT",
            map_version = version,
            from_col = "prodcodeid",
            to_col = "dmdid",
            map_source = source
          )
        )
      )
    }
  }

  result[intersect(cprd_aurum_tables(), tables)]
}

#' Table names returned by read_cprd_aurum()
#' @noRd
cprd_aurum_tables <- function() {
  c(
    "cprd_aurum_medcode_lkp",
    "cprd_aurum_prodcode_lkp",
    "cprd_aurum_medcode_snomed",
    "cprd_aurum_medcode_read2",
    "cprd_aurum_prodcode_snomed"
  )
}

#' Version label for a CPRD Aurum code browser release
#'
#' The `yyyymm` release date from a name like `CPRD_CodeBrowser_YYYYMM_Aurum`.
#' Errors if the name has no release date.
#' @noRd
cprd_aurum_version <- function(path, call = rlang::caller_env()) {
  name <- basename(path)
  release <- stringr::str_match(name, "CodeBrowser_(\\d{6})_Aurum")[, 2]

  if (is.na(release)) {
    codeminer_abort(
      c(
        "x" = "Could not find a release date in {.file {name}}",
        "i" = "Expected a name like {.file CPRD_CodeBrowser_YYYYMM_Aurum}.",
        "i" = "Supply the release date with {.arg version}, for example {.code version = \"YYYYMM\"}."
      ),
      call = call
    )
  }

  codeminer_inform(
    "Using version {.val {release}}, the release date in {.file {name}}."
  )
  release
}

#' Expected columns of the CPRD Aurum dictionary files
#'
#' Named by the column name in the file, with the type to read it as.
#' @noRd
cprd_aurum_medical_columns <- function() {
  c(
    MedCodeId = "character",
    Observations = "integer",
    OriginalReadCode = "character",
    CleansedReadCode = "character",
    Term = "character",
    SnomedCTConceptId = "character",
    SnomedCTDescriptionId = "character",
    Release = "character",
    EmisCodeCategoryId = "character"
  )
}

#' @noRd
cprd_aurum_product_columns <- function() {
  c(
    ProdCodeId = "character",
    dmdid = "character",
    `Term from EMIS` = "character",
    ProductName = "character",
    Formulation = "character",
    RouteOfAdministration = "character",
    DrugSubstanceName = "character",
    SubstanceStrength = "character",
    BNFChapter = "character",
    DrugIssues = "integer"
  )
}

#' Lower-case, underscore-separated column name
#' @noRd
cprd_aurum_col_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", "_")
}

#' Find and read one CPRD Aurum dictionary file
#'
#' Reads with the declared column types in `columns`, without quote handling
#' (some terms contain `"`), with empty fields as `NA` and column names in
#' lower case. Columns are matched to `columns` ignoring case and spacing.
#' Errors if an expected column is missing; reads unexpected columns as
#' character, with a warning.
#' @noRd
read_cprd_aurum_file <- function(
  dir,
  file_name,
  columns,
  call = rlang::caller_env()
) {
  files <- list.files(
    dir,
    pattern = paste0("^", file_name, "$"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    codeminer_abort(
      c(
        "x" = "Could not find {.file {file_name}}",
        "i" = "Expected under {.path {dir}}",
        "i" = "To read only the tables from the other dictionary, set {.arg tables}."
      ),
      call = call
    )
  }

  read_file <- \(...) {
    data.table::fread(
      files[[1]],
      sep = "\t",
      quote = "",
      na.strings = "",
      encoding = "UTF-8",
      ...
    )
  }

  header <- names(read_file(nrows = 0, colClasses = "character"))
  header_key <- cprd_aurum_col_name(header)
  expected_key <- cprd_aurum_col_name(names(columns))

  missing <- names(columns)[!expected_key %in% header_key]
  if (length(missing) > 0) {
    codeminer_abort(
      c(
        "x" = "{.file {basename(files[[1]])}} is missing expected column{?s} {.val {missing}}",
        "i" = "Found: {.val {header}}"
      ),
      call = call
    )
  }

  unexpected <- header[!header_key %in% expected_key]
  if (length(unexpected) > 0) {
    codeminer_warn(c(
      "!" = "{.file {basename(files[[1]])}} has unexpected column{?s} {.val {unexpected}}",
      "i" = "Unexpected columns are read as character."
    ))
  }

  col_classes <- rlang::set_names(
    unname(columns[match(header_key, expected_key)]),
    header
  )
  col_classes[is.na(col_classes)] <- "character"

  table <- read_file(colClasses = col_classes)
  names(table) <- cprd_aurum_col_name(names(table))

  tibble::as_tibble(table)
}

#' Replace NA with "" in the given columns
#' @noRd
blanks_to_empty_string <- function(df, cols) {
  dplyr::mutate(
    df,
    dplyr::across(dplyr::all_of(cols), \(x) dplyr::coalesce(x, ""))
  )
}

#' Column filter specification including every value by default
#' @noRd
cprd_aurum_col_filter <- function(x, description) {
  values <- stringr::str_sort(unique(x), numeric = TRUE)
  list(
    values = values,
    defaults = values,
    description = description
  )
}
