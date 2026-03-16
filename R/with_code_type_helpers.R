#' Set code type context
#'
#' @description
#' `with_type()` temporarily sets the active code type so that query functions
#' inside `code` use it without needing an explicit `type =` argument.
#'
#' Convenience wrappers (`icd10()`, `sct()`, etc.) call `with_type()` for each
#' supported code type.
#'
#' @param type A string naming the code type (e.g. `"icd10"`, `"sct"`). This
#'   corresponds to the `type` argument accepted by query functions such as
#'   [CODES()], [CHILDREN()], and [MAP()].
#' @param code The expression to evaluate within the code type context.
#'
#' @return The result of evaluating `code`.
#'
#' @examples
#' \dontrun{
#' with_type("icd10", DESCRIPTION("diabetes"))
#' icd10(DESCRIPTION("diabetes"))
#' bnf(CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
#' sct(
#'   CHILDREN(
#'     "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
#'   )
#' )
#' }
#'
#' @name with_type
#' @export
with_type <- function(type, code) {
  withr::with_options(
    list(
      codeminer.code_type = type,
      codeminer.map_to = type
    ),
    code = code
  )
}

#' @rdname with_type
#' @export
icd10 <- function(code) with_type("icd10", code)

#' @rdname with_type
#' @export
icd9 <- function(code) with_type("icd9", code)

#' @rdname with_type
#' @export
read3 <- function(code) with_type("read3", code)

#' @rdname with_type
#' @export
read2 <- function(code) with_type("read2", code)

#' @rdname with_type
#' @export
sct <- function(code) with_type("sct", code)

#' @rdname with_type
#' @export
opcs4 <- function(code) with_type("opcs4", code)

#' @rdname with_type
#' @export
phecode <- function(code) with_type("phecode", code)

#' @rdname with_type
#' @export
read2_drugs <- function(code) with_type("read2_drugs", code)

#' @rdname with_type
#' @export
bnf <- function(code) with_type("bnf", code)

#' @rdname with_type
#' @export
dmd <- function(code) with_type("dmd", code)

#' @rdname with_type
#' @export
data_coding_3 <- function(code) with_type("data_coding_3", code)

#' @rdname with_type
#' @export
data_coding_4 <- function(code) with_type("data_coding_4", code)

#' @rdname with_type
#' @export
data_coding_5 <- function(code) with_type("data_coding_5", code)

#' @rdname with_type
#' @export
data_coding_6 <- function(code) with_type("data_coding_6", code)
