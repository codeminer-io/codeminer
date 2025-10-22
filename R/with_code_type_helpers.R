#' With Code Type Functions
#'
#' @description
#' These functions allow users to specify a code type and execute CodeMiner code
#' within that context.
#'
#' @param code The code to be processed.
#'
#' @details
#' Available functions:
#'
#' - `r paste(paste0("\\code{", codeminer::codeminer_metadata()$lookup_tables$code, "()}"), sep = "", collapse = "\n- ")`
#'
#' @return The result of executing the code within the specified `code_type`
#'   context.
#'
#' @examples
#' \dontrun{
#' icd10(DESCRIPTION("diabetes"))
#' bnf(CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
#' sct(
#'   CHILDREN(
#'     "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
#'   )
#' )
#' }
#'
#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
icd10 <- function(code) {
  with_code_type(code_type = "icd10", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
icd9 <- function(code) {
  with_code_type(code_type = "icd9", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
read3 <- function(code) {
  with_code_type(code_type = "read3", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
read2 <- function(code) {
  with_code_type(code_type = "read2", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
sct <- function(code) {
  with_code_type(code_type = "sct", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
opcs4 <- function(code) {
  with_code_type(code_type = "opcs4", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
phecode <- function(code) {
  with_code_type(code_type = "phecode", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
read2_drugs <- function(code) {
  with_code_type(code_type = "read2_drugs", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
bnf <- function(code) {
  with_code_type(code_type = "bnf", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
dmd <- function(code) {
  with_code_type(code_type = "dmd", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
data_coding_3 <- function(code) {
  with_code_type(code_type = "data_coding_3", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
data_coding_4 <- function(code) {
  with_code_type(code_type = "data_coding_4", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
data_coding_5 <- function(code) {
  with_code_type(code_type = "data_coding_5", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
data_coding_6 <- function(code) {
  with_code_type(code_type = "data_coding_6", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
medcode_aurum <- function(code) {
  with_code_type(code_type = "medcode_aurum", code = code)
}

#' @name with_code_type_helpers
#' @rdname with_code_type_helpers
#' @export
prodcode_aurum <- function(code) {
  with_code_type(code_type = "prodcode_aurum", code = code)
}

# PRIVATE -----------------------------------------------------------------

with_code_type <- function(code_type, code) {
  withr::with_options(
    list(
      codeminer.code_type = code_type,
      codeminer.map_to = code_type
    ),
    code = code
  )
}
