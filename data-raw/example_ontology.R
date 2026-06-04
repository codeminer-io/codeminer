#' Create example ontology tables for testing
#'
#' Returns an example ontology consisting of a structured list of lookup, mapping,
#' and relationship tables, and their associated metadata across multiple versions.
#'
#' @return A list with the following structure:
#'   - `lookup_tables`: List of lookup data frames (capital_letters, lowercase_letters)
#'   - `lookup_metadata`: Data frame containing the metadata for the lookup tables
#'   - `mapping_tables`: List of mapping data frames (capital_to_lowercase)
#'   - `mapping_metadata`: Data frame containing the metadata for the mapping tables
#'   - `relationship_tables`: List of relationship data frames
#'   - `relationship_metadata`: Data frame containing the metadata for the relationship tables
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
      code = c("A", "B", "C", "D", "alpha_code", "beta_code"),
      description = c(
        "ALPHA",
        "BRAVO",
        "CHARLIE",
        "DELTA",
        "Alpha Attribute",
        "Beta Attribute"
      )
    ),
    capital_letters_v3 = tibble::tibble(
      code = c(
        "A",
        "B",
        "C",
        "D",
        "E",
        "alpha_code",
        "beta_code",
        "gamma_code",
        "delta_code"
      ),
      description = c(
        "ALPHA",
        "BRAVO",
        "CHARLIE",
        "DELTA",
        "ECHO",
        "Alpha Attribute",
        "Beta Attribute",
        "Gamma Attribute",
        "Delta Attribute"
      )
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
    code_type = "capital_letters",
    lookup_version = versions,
    lookup_code_col = "code",
    lookup_description_col = "description",
    lookup_category_col = NA_character_,
    lookup_source = "example_data",
    preferred_description_col = NA_character_,
    preferred_description_indicator = NA_character_
  )
  lower_metadata <- tibble::tibble(
    lookup_table_name = paste0("lowercase_letters_", versions),
    code_type = "lowercase_letters",
    lookup_version = versions,
    lookup_code_col = "code",
    lookup_description_col = "description",
    lookup_category_col = NA_character_,
    lookup_source = "example_data",
    preferred_description_col = NA_character_,
    preferred_description_indicator = NA_character_
  )
  lookup_metadata <- dplyr::bind_rows(capital_metadata, lower_metadata) |>
    dplyr::mutate(col_filters = NA_character_)

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

  # Stops air from malforming the tribble
  # fmt: skip
  # nolint start
  mapping_metadata <- tibble::tribble(
    ~mapping_table_name       , ~from_code_type   , ~to_code_type       , ~map_version , ~from_col , ~to_col     ,     ~map_source,
    "capital_to_lowercase_v1" , "capital_letters" , "lowercase_letters" , "v1"             , "capital" , "lowercase" , "example_data",
    "capital_to_lowercase_v2" , "capital_letters" , "lowercase_letters" , "v2"             , "capital" , "lowercase" , "example_data",
    "capital_to_lowercase_v3" , "capital_letters" , "lowercase_letters" , "v3"             , "capital" , "lowercase",  "example_data"
  ) |>
    dplyr::mutate(col_filters = NA_character_)
  # nolint end

  # Relationship tables and metadata
  # v1: Basic hierarchy (A -> B -> C)
  # v2: Add D to hierarchy (A -> B -> C -> D) and attributes
  # v3: Add E and more complex relationships
  relationship_tables <- list(
    capital_letters_relationship_v1 = tibble::tibble(
      from = c("B", "C"),
      to = c("A", "B"),
      type = c("is a", "is a")
    ),
    capital_letters_relationship_v2 = tibble::tibble(
      from = c("B", "C", "D", "A", "B"),
      to = c("A", "B", "C", "alpha_code", "beta_code"),
      type = c("is a", "is a", "is a", "has attribute", "has attribute")
    ),
    capital_letters_relationship_v3 = tibble::tibble(
      from = c("B", "C", "D", "E", "A", "B", "C", "E"),
      to = c(
        "A",
        "B",
        "C",
        "D",
        "alpha_code",
        "beta_code",
        "gamma_code",
        "delta_code"
      ),
      type = c(
        "is a",
        "is a",
        "is a",
        "is a",
        "has attribute",
        "has attribute",
        "has attribute",
        "has attribute"
      )
    )
  )

  # Stops air from malforming the tribble
  # fmt: skip
  # nolint start
  relationship_metadata <- tibble::tribble(
    ~relationship_table_name            , ~code_type        , ~relationship_version , ~from_col , ~to_col , ~type_col , ~child_parent_relationship_code ,
    "capital_letters_relationship_v1"  , "capital_letters" , "v1"                  , "from"    , "to"    , "type"    , "is a"                          ,
    "capital_letters_relationship_v2"  , "capital_letters" , "v2"                  , "from"    , "to"    , "type"    , "is a"                          ,
    "capital_letters_relationship_v3"  , "capital_letters" , "v3"                  , "from"    , "to"    , "type"    , "is a"
  ) |>
    dplyr::mutate(
      relationship_source = NA_character_,
      col_filters = NA_character_
    )
  # nolint end

  return(list(
    lookup_tables = lookup_tables,
    lookup_metadata = lookup_metadata,
    mapping_tables = mapping_tables,
    mapping_metadata = mapping_metadata,
    relationship_tables = relationship_tables,
    relationship_metadata = relationship_metadata
  ))
}

example_ontology <- create_example_data()
usethis::use_data(example_ontology, overwrite = TRUE)
