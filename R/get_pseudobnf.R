#' Get NHS BSA BNF code information (pseudo-BNF)
#'
#' Downloads the NHS Business Services Authority (NHSBSA) "BNF Code Information"
#' dataset from the NHSBSA Open Data Portal. This is the full BNF code hierarchy
#' (chapter -> section -> paragraph -> ... -> presentation), commonly referred to
#' as the pseudo-BNF classification, published monthly.
#'
#' @param dir_path Directory path to download to. Defaults to `tempdir()`.
#' @param release Character string specifying which release to download. Either:
#'   * `"latest"` (default) — the most recent monthly release.
#'   * A substring matching a single resource name (e.g. a `"YYYYMM"` year-month
#'     or a `"version_NN"` label).
#' @param overwrite Logical. If `TRUE`, re-downloads and overwrites an existing
#'   file. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages. Default
#'   is `FALSE`.
#'
#' @return File path to the downloaded CSV (invisibly).
#'
#' @details
#' Requires the \pkg{nhsbsa} package, which provides a low-level client for the
#' NHSBSA Open Data Portal API.
#'
#' @seealso [read_pseudobnf()], [add_pseudobnf()]
#' @export
#' @examples
#' \dontrun{
#' path <- get_pseudobnf()
#' read_pseudobnf(path)
#' }
get_pseudobnf <- function(
  dir_path = tempdir(),
  release = "latest",
  overwrite = FALSE,
  quiet = FALSE
) {
  if (!is.character(release) || length(release) != 1 || release == "") {
    cli::cli_abort(c(
      "x" = "{.arg release} must be a non-empty character string",
      "i" = "Use {.val latest} for the most recent release, or a substring
             matching a single resource name."
    ))
  }

  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "x" = "Directory does not exist: {.file {dir_path}}",
      "i" = "Create it with: {.code dir.create('{dir_path}', recursive = TRUE)}"
    ))
  }

  rlang::check_installed(
    "nhsbsa",
    reason = "to download datasets from the NHSBSA Open Data Portal."
  )

  dataset_id <- "bnf-code-information-current-year"

  msg <- function(...) if (!quiet) cli::cli_inform(...)

  msg("Getting NHS BSA BNF code information ({.val {dataset_id}})")

  resources <- nhsbsa::nhsbsa_list_resources(dataset_id)

  # Keep only CSV resources (the dataset also lists non-data files), newest first
  resources <- resources[
    !is.na(resources$format) & toupper(resources$format) == "CSV",
    ,
    drop = FALSE
  ]
  if (nrow(resources) == 0) {
    cli::cli_abort("No CSV resources found for dataset {.val {dataset_id}}.")
  }
  resources <- resources[
    order(resources$last_modified, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  if (release == "latest") {
    selected <- resources[1, ]
  } else {
    matches <- resources[
      stringr::str_detect(
        resources$name,
        stringr::regex(release, ignore_case = TRUE)
      ),
      ,
      drop = FALSE
    ]
    if (nrow(matches) == 0) {
      cli::cli_abort(c(
        "x" = "No release matching {.val {release}} found for dataset
               {.val {dataset_id}}.",
        "i" = "Available resources: {.val {resources$name}}"
      ))
    }
    if (nrow(matches) > 1) {
      cli::cli_abort(c(
        "x" = "{nrow(matches)} releases match {.val {release}}; exactly one is
               required.",
        "i" = "Matches: {.val {matches$name}}"
      ))
    }
    selected <- matches[1, ]
  }

  msg("Downloading {.val {selected$name}} to {.file {dir_path}}...")

  path <- nhsbsa::nhsbsa_download_resource(
    dataset_id,
    resource_id = selected$id,
    directory = dir_path,
    overwrite = overwrite,
    quiet = quiet
  )

  msg(c("v" = "NHS BSA BNF code information downloaded to {.file {path}}"))

  invisible(path)
}
