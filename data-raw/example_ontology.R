#' Create example ontology tables for testing
#'
#' Returns an example ontology consisting of a structured list of lookup and mapping tables,
#' and their associated metadata across multiple versions.
#'
#' @return A list with the following structure:
#'   - `lookup_tables`: List of lookup data frames (capital_letters, lowercase_letters)
#'   - `lookup_metadata`: Data frame containing the metadata for the lookup tables
#'   - `mapping_tables`: List of mapping data frames (capital_to_lowercase)
#'   - `mapping_metadata`: Data frame containing the metadata for the mapping tables
#'
#'   Each category contains versions v1, v2, and v3.
#'
#' @keywords internal
create_example_data <- function() {
  # Lookup tables and metadata
  lookup_tables <- list(
    capital_letters_v1 = tibble::tibble(
      code = c("A", "B", "C"),
      description = c("ALPHA", "BRAVO", "CHARLIE")
    ),
    capital_letters_v2 = tibble::tibble(
      code = c("A", "B", "C", "D"),
      description = c("ALPHA", "BRAVO", "CHARLIE", "DELTA")
    ),
    capital_letters_v3 = tibble::tibble(
      code = c("A", "B", "C", "D", "E"),
      description = c("ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO")
    ),
    lowercase_letters_v1 = tibble::tibble(
      code = c("a", "b", "c"),
      description = c("alpha", "bravo", "charlie")
    ),
    lowercase_letters_v2 = tibble::tibble(
      code = c("a", "b", "c", "d"),
      description = c("alpha", "bravo", "charlie", "delta")
    ),
    lowercase_letters_v3 = tibble::tibble(
      code = c("a", "b", "c", "d", "e"),
      description = c("alpha", "bravo", "charlie", "delta", "echo")
    )
  )

  versions <- c("v1", "v2", "v3")
  capital_metadata <- tibble::tibble(
    lookup_table_name = paste0("capital_letters_", versions),
    coding_type = "capital_letters",
    lookup_version = versions,
    lookup_code_col = "code",
    lookup_description_col = "description",
    lookup_source = "example_data",
    preferred_synonym_col = NA_character_,
    preferred_code = NA_character_
  )
  lower_metadata <- tibble::tibble(
    lookup_table_name = paste0("lowercase_letters_", versions),
    coding_type = "lowercase_letters",
    lookup_version = versions,
    lookup_code_col = "code",
    lookup_description_col = "description",
    lookup_source = "example_data",
    preferred_synonym_col = NA_character_,
    preferred_code = NA_character_
  )
  lookup_metadata <- dplyr::bind_rows(capital_metadata, lower_metadata)

  # Mapping tables and metadata
  mapping_tables <- list(
    capital_to_lowercase_v1 = tibble::tibble(
      capital = c("A", "B", "C"),
      lowercase = c("a", "b", "c")
    ),
    capital_to_lowercase_v2 = tibble::tibble(
      capital = c("A", "B", "C", "D"),
      lowercase = c("a", "b", "c", "d")
    ),
    capital_to_lowercase_v3 = tibble::tibble(
      capital = c("A", "B", "C", "D", "E"),
      lowercase = c("a", "b", "c", "d", "e")
    )
  )

  mapping_metadata <- tibble::tribble(
    ~mapping_table_name       , ~from_coding_type , ~to_coding_type     , ~from_col , ~to_col     ,
    "capital_to_lowercase_v1" , "capital_letters" , "lowercase_letters" , "capital" , "lowercase" ,
    "capital_to_lowercase_v2" , "capital_letters" , "lowercase_letters" , "capital" , "lowercase" ,
    "capital_to_lowercase_v3" , "capital_letters" , "lowercase_letters" , "capital" , "lowercase"
  )

  return(list(
    lookup_tables = lookup_tables,
    lookup_metadata = lookup_metadata,
    mapping_tables = mapping_tables,
    mapping_metadata = mapping_metadata
  ))
}

example_ontology <- create_example_data()
usethis::use_data(example_ontology, overwrite = TRUE)
