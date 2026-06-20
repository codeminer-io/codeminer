#' Set code type context
#'
#' @description
#' `with_type()` temporarily sets the active code type so that query functions
#' inside `code` use it without needing an explicit `type =` argument. It works
#' with any code type present in the database.
#'
#' @param type A string naming the code type (e.g. `"ICD-10"`, `"SNOMED CT"`).
#'   This corresponds to the `type` argument accepted by query functions such
#'   as [CODES()], [CHILDREN()], and [MAP()].
#' @param code The expression to evaluate within the code type context.
#'
#' @return The result of evaluating `code`.
#'
#' @examples
#' \dontrun{
#' with_type("ICD-10", DESCRIPTION("diabetes"))
#' with_type("BNF", CODES("0204 << Beta-Adrenoceptor Blocking Drugs >>"))
#' with_type(
#'   "SNOMED CT",
#'   CHILDREN(
#'     "770765001 << Proliferative retinopathy of right eye due to diabetes mellitus (disorder) >>"
#'   )
#' )
#' }
#'
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
