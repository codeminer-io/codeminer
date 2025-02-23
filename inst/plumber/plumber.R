# plumber.R
library(plumber)
library(jsonlite)

#* @apiTitle My API for Clinical Events and Code Lookup
#* @apiDescription A simple API that serves dummy clinical events and allows searching for clinical codes.

#* Search for codes that match a description
#* @get /DESCRIPTION
#* @param reg_expr:string The regex pattern to search for.
#* @param code_type:string Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
#* @serializer json
function(reg_expr, code_type) {
  DESCRIPTION(
    reg_expr = reg_expr,
    code_type = code_type
  )
}

#* Look up descriptions for clinical codes
#* @get /CODES
#* @param codes:[string] Vector of codes to lookup.
#* @param code_type:string Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
#* @serializer json
function(codes, code_type) {
  CODES(
    codes = codes,
    code_type = code_type,
    unrecognised_codes = "warning"
  )
}

#* Get descendents for a set of codes
#* @get /CHILDREN
#* @param codes:[string] A vector of code strings to retrieve child codes for.
#* @param code_type:string Type of clinical code system to be searched. One of "sct" (SNOMED CT) or "icd10" (ICD-10).
#* @serializer json
function(codes, code_type) {
  CHILDREN(
    codes = codes,
    code_type = code_type,
    unrecognised_codes = "warning"
  )
}
