#' Download Phecode 1.2 files
#'
#' Downloads the Phecode 1.2 definitions file and/or mapping files from
#' [phewascatalog.org](https://phewascatalog.org/phecodes).
#'
#' @param dir_path Directory to download files to. Defaults to `tempdir()`.
#' @param files Character vector of files to download. Available options:
#'   * `"lkp"` — Phecode 1.2 definitions (lookup)
#'   * `"icd10_map"` — Phecode 1.2 to ICD-10 mapping (beta)
#'   * `"icd9_map"` — Phecode 1.2 to ICD-9 mapping
#'
#'   By default, downloads all three.
#' @param overwrite Logical. If `TRUE`, re-downloads existing files. Default is
#'   `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return A named list of file paths (invisibly), with names `"lkp"`,
#'   `"icd10_map"`, and/or `"icd9_map"` corresponding to the requested files.
#'
#' @seealso [read_phecode()], [add_phecode()]
#' @export
#' @examples
#' \dontrun{
#' paths <- get_phecode()
#' paths <- get_phecode(files = c("lkp", "icd10_map"), dir_path = "~/phecode")
#' }
get_phecode <- function(
  dir_path = tempdir(),
  files = c("lkp", "icd10_map", "icd9_map"),
  overwrite = FALSE,
  quiet = FALSE
) {
  rlang::arg_match(files, c("lkp", "icd10_map", "icd9_map"), multiple = TRUE)

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  file_specs <- list(
    lkp = list(
      url = "https://phewascatalog.org/phewas/_w_383d92ec/data/phecode_definitions1.2.csv.zip",
      zip_name = "phecode_definitions1.2.csv.zip",
      extracted_name = "phecode_definitions1.2.csv"
    ),
    icd10_map = list(
      url = "https://phewascatalog.org/phewas/_w_383d92ec/data/Phecode_map_v1_2_icd10_WHO_beta.csv.zip",
      zip_name = "Phecode_map_v1_2_icd10_beta.csv.zip",
      extracted_name = "Phecode_map_v1_2_icd10_WHO_beta.csv"
    ),
    icd9_map = list(
      url = "https://github.com/PheWAS/PheWAS/raw/refs/heads/master/data/phemap.rda",
      zip_name = NULL,
      extracted_name = "phemap.rda"
    )
  )

  result <- list()

  for (f in files) {
    spec <- file_specs[[f]]
    target_file <- file.path(dir_path, spec$extracted_name)

    if (file.exists(target_file) && !overwrite) {
      msg(c(
        "v" = "{.file {spec$extracted_name}} already exists.",
        "i" = "Set {.code overwrite = TRUE} to redownload."
      ))
      result[[f]] <- target_file
      next
    }

    msg("Downloading {.file {spec$extracted_name}}")

    if (!is.null(spec$zip_name)) {
      # Download and unzip
      zip_path <- file.path(tempdir(), spec$zip_name)
      tryCatch(
        utils::download.file(spec$url, zip_path, mode = "wb", quiet = quiet),
        error = function(e) {
          cli::cli_abort(c(
            "x" = "Failed to download {.file {spec$zip_name}}",
            "i" = "Error: {conditionMessage(e)}"
          ))
        }
      )
      utils::unzip(zip_path, exdir = dir_path, overwrite = overwrite)
    } else {
      # Direct download (no zip)
      tryCatch(
        utils::download.file(spec$url, target_file, mode = "wb", quiet = quiet),
        error = function(e) {
          cli::cli_abort(c(
            "x" = "Failed to download {.file {spec$extracted_name}}",
            "i" = "Error: {conditionMessage(e)}"
          ))
        }
      )
    }

    result[[f]] <- target_file
  }

  invisible(result)
}
