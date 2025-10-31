#' Example ontology data
#'
#' An example ontology dataset consisting of a structured list of lookup and mapping tables,
#' and their associated metadata across multiple versions.
#'
#' @format ## `example_ontology`
#' A list with the following structure:
#'   - `lookup_tables` - List of lookup data frames (capital_letters, lowercase_letters)
#'   - `lookup_metadata` - Lookup metadata data frame
#'   - `mapping_tables` - List of mapping data frames (capital_to_lowercase)
#'   - `mapping_metadata` - Mapping metadata data frame
#'
#'   Each category contains versions v1, v2, and v3.
"example_ontology"
