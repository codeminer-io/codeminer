#' Example ontology data
#'
#' An example ontology dataset consisting of a structured list of lookup and mapping tables,
#' and their associated metadata across multiple versions.
#'
#' @format ## `example_ontology`
#' A list with the following structure:
#'   - `lookup_tables` - List of lookup data frames (capital_letters, lowercase_letters)
#'   - `lookup_metadata` - List of lookup metadata data frames
#'   - `mapping_tables` - List of mapping data frames (capital_to_lowercase)
#'   - `mapping_metadata` - List of mapping metadata data frames
#'
#'   Each category contains versions v1, v2, and v3.
"example_ontology"
