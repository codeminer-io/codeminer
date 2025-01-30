# adapted from shiny::runApp(system.file("examples/07-autocomplete-combine", package="shinyAce"))

library(shiny)
library(shinyAce)
library(dplyr)
library(rlang)


# Functions ---------------------------------------------------------------


expr_to_list <- function(expr) {
  if (rlang::is_call(expr)) {
    list(
      fun = rlang::as_string(rlang::call_name(expr)), # Function name as a string
      args = purrr::map(rlang::call_args(expr), expr_to_list) # Recursively process arguments
    )
  } else if (rlang::is_symbol(expr)) {
    # For symbols, return the name
    rlang::as_string(expr)
  } else if (rlang::is_atomic(expr)) {
    # For atomic values (e.g., numbers, characters), return as-is
    expr
  } else {
    stop("Unsupported type")
  }
}

# flatten unnecessary brackets
flatten_brackets <- function(node) {
  if (is.list(node) && "fun" %in% names(node)) {
    # If the node's function is "(", replace it with its arguments
    if (node$fun == "(") {
      # Recursively flatten the arguments
      flattened_args <- purrr::map(node$args, flatten_brackets)
      # Return the flattened arguments directly (remove the node with fun = "(")
      return(flattened_args[[1]])
    }
    # Otherwise, keep the node structure and recursively process arguments
    node$args <- purrr::map(node$args, flatten_brackets)
  }
  node
}

transform_query_base <- function(query) {
  if (!is.null(query$fun) && !is.null(query$args)) {
    if (query$fun %in% c("%OR%", "%AND%", "%NOT%")) {
      # Logical operator node
      rules_list <- purrr::map(query$args, transform_query_base)
      names(rules_list) <- NULL

      return(list(
        condition = gsub("%", "", toupper(query$fun)), # Remove % and convert to uppercase
        rules = rules_list
      ))
    } else if (query$fun %in% c("DESCRIPTION", "MAP", "CHILDREN", "CODES")) {
      # Leaf node
      return(list(
        id = tolower(query$fun),
        field = tolower(query$fun),
        type = "string",
        input = "text",
        operator = if (is.list(query$args) && !is.null(query$args$code_type)) query$args$code_type else "sct",
        value = if (is.list(query$args) && !is.null(query$args[[1]])) query$args[[1]] else query$args
      ))
    }
  }
}

transform_query <- function(query) {
  result <- transform_query_base(query)

  result$valid <- TRUE

  return(result)
}


# Temp --------------------------------------------------------------------

get_qbr_input_from_saved_queries <- function(x) {
  recurse <- function(x) {
    if (is.list(x) &
        identical(names(x),
                  c("id", "field", "type", "input", "operator", "value"))) {

      switch(
        x$id,
        "description" = NULL,
        "children" = NULL,
        "codes" = NULL,
        "saved_query" = x$value,
        "map_children" = NULL,
        "map_codes" = NULL,
        "sct_has_attributes" = x$value,
        "sct_get_attributes" = x$value,
        "sct_attribute_types_to" = x$value,
        "sct_attribute_types_from" = x$value,
        stop("Unrecognised filter!")
      )

    } else if (is.list(x)) {
      purrr::map(x, get_qbr_input_from_saved_queries)
    } else {
      NULL
    }
  }

  unlist(recurse(x))
}

input2 <- '
RESULT = DESCRIPTION("diab") %AND% DESCRIPTION("retin|mac") %NOT%
    DESCRIPTION("absent|without") %OR% ((DESCRIPTION("diab") %AND%
    DESCRIPTION("nephro|neuro") %NOT% DESCRIPTION("absent|without")))
'


# jqbr requirements -------------------------------------------------------


## codemapper constants ----------------------------------------------------

CODE_TYPE_TO_LKP_TABLE_MAP <- tibble::tribble(
  ~code, ~code_label, ~lkp_table, ~code_col, ~description_col, ~preferred_synonym_col, ~preferred_code, ~grouping_col, ~filter_cols,
  "icd10", "ICD-10", "icd10_lkp", "ALT_CODE", "DESCRIPTION", NA, NA, "category", NA,
  "icd9", "ICD-9", "icd9_lkp", "ICD9", "DESCRIPTION_ICD9", NA, NA, "category", NA,
  "read3", "Read 3", "read_ctv3_lkp", "read_code", "term_description", "description_type", "P", NA, NA,
  "read2", "Read 2", "read_v2_lkp", "read_code", "term_description", "term_code", "00", NA, NA,
  "sct", "SNOMED CT", "sct_description", "conceptId", "term_description", "typeId_description", "900000000000003001", NA, list(list(active_concept = c("*0*", "*1*"), active_description = c("0", "*1*"))),
  "opcs4", "OPCS4", "opcs4_lkp", "opcs4_code", "description", NA, NA, "category", NA,
  "phecode", "Phecode", "phecode_lkp", "phecode", "phenotype", NA, NA, "category", NA,
  "read2_drugs", "Read 2, drugs", "read_v2_drugs_lkp", "read_code", "term_description", NA, NA, NA, NA,
  "bnf", "BNF", "bnf_lkp", "BNF_Code", "Description", NA, NA, "BNF_Chemical_Substance", NA,
  # "dmd", "dmd_lkp", "concept_id", "term", NA, NA,
  "dmd", "DMD", "bnf_dmd", "snomed_code", "dm_d_product_description", NA, NA, "dm_d_product_description", NA,
  "data_coding_3", "Self-reported cancer (dc-3)", "self_report_cancer", "data_coding_3", "description", NA, NA, "category", NA,
  "data_coding_4", "Self-reported medications (dc-4)", "self_report_medication", "data_coding_4", "description", NA, NA, "category", NA,
  "data_coding_5", "Self-reported operations (dc-5)", "self_report_operation", "data_coding_5", "description", NA, NA, "category", NA,
  "data_coding_6", "Self-reported non-cancer (dc-6)", "self_report_non_cancer", "data_coding_6", "description", NA, NA, "category", NA,
)

# CLINICAL_CODE_MAPPINGS_MAP ----------------------------------------------

# used by `map_codes()`
# 'from' and 'to' cols: possible mapping combinations
# 'mapping_table': the appropriate mapping table to use for a 'from'/'to' combination
# 'from_col' and 'to_col': the columns to use when mapping
# Note, `preferred_synonym_col` and `preferred_code` refer to `to_col`
CLINICAL_CODE_MAPPINGS_MAP <- tibble::tribble(
  ~from, ~to, ~mapping_table, ~from_col, ~to_col, ~preferred_synonym_col, ~preferred_code, ~filter_cols,
  "icd9", "icd10", "icd9_icd10", "ICD9", "ICD10", NA, NA, NA,
  "read2_drugs", "bnf", "read_v2_drugs_bnf", "read_code", "bnf_code", NA, NA, NA,
  "read2", "icd9", "read_v2_icd9", "read_code", "icd9_code", NA, NA, NA,
  "read2", "icd10", "read_v2_icd10", "read_code", "icd10_code", NA, NA, list(list(icd10_code_def = c("*1*", "*15*", "*3*", "*5*", "*7*", "*8*", "2"))),
  "read2", "opcs4", "read_v2_opcs4", "read_code", "opcs_4.2_code", NA, NA, NA,
  "read2", "read3", "read_v2_read_ctv3", "READV2_CODE", "READV3_CODE", "TERMV3_TYPE", "P", list(list(IS_ASSURED = "*1*")),
  "read3", "icd9", "read_ctv3_icd9", "read_code", "icd9_code", NA, NA, NA,
  "read3", "icd10", "read_ctv3_icd10", "read_code", "icd10_code", NA, NA, list(list(mapping_status = c("*E*", "*G*", "*D*", "R", "A", "U"), refine_flag = c("*C*", "*P*", "M"), element_num = c("*0*", as.character(1:3)), block_num = c("*0*", as.character(1:14)))),
  "read3", "opcs4", "read_ctv3_opcs4", "read_code", "opcs4_code", NA, NA, NA,
  "read3", "read2", "read_ctv3_read_v2", "READV3_CODE", "READV2_CODE", "TERMV2_TYPE", "P", list(list(IS_ASSURED = "*1*")),
  "bnf", "sct", "bnf_dmd", "bnf_code", "snomed_code", NA, NA, NA,
  "icd10", "phecode", "icd10_phecode", "ALT_CODE", "PHECODE", NA, NA, NA,
  "icd9", "phecode", "icd9_phecode", "icd9", "phecode", NA, NA, NA,
  "sct", "icd10", "sct_icd10", "referencedComponentId", "mapTarget", NA, NA, NA,
  "sct", "opcs4", "sct_opcs4", "referencedComponentId", "mapTarget", NA, NA, NA,
  "read2", "sct", "rcsctmap2", "ReadCode", "ConceptId", NA, NA, list(list(IS_ASSURED = "*1*", MapStatus = "*1*")),
  "read3", "sct", "ctv3sctmap2", "CTV3_CONCEPTID", "SCT_CONCEPTID", NA, NA, list(list(IS_ASSURED = "*1*", MAPSTATUS = "*1*"))
)

## jqbr filters and operators --------------------------------------------------------------------

### Filters ---------------------------------------------------------
empty_saved_query_filter <- list(
  id = "saved_query",
  label = "Saved query",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("read2"),
  description = "Previously saved queries will be listed here."
)

child_codes_filter <- list(
  id = "children",
  label = "Children",
  type = "string",
  operators = list("read2"),
  description = "Search for children codes. Multiple codes may be supplied separated by '|' e.g. 'E10 | E11' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E11 << T2DM >>'."
)

description_contains_filter <- list(
  id = "description",
  label = "Description",
  type = "string",
  operators = list("read2"),
  description = "Search for codes that match a regular expression (case insensitive). For example, '^a' will search for all codes starting with either 'A' or 'a'."
)

codes_filter <- list(
  id = "codes",
  label = "Codes",
  type = "string",
  operators = list("read2"),
  description = "Search for one or more codes separated by '|' e.g. 'E10 | E101' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E101 << T1DM with coma >>'."
)

map_codes_filter <- list(
  id = "map_codes",
  label = "Map codes",
  type = "string",
  operators = list(),
  description = "Map codes from one coding system to another. Multiple codes may be supplied separated by '|' e.g. 'E101 | E102' for ICD10. Comments may also be included between '<< >>' e.g. 'E101 << T1DM with coma >> | E102 << T1DM with renal complications >>'."
)

map_children_filter <- list(
  id = "map_children",
  label = "Map children",
  type = "string",
  operators = list(),
  description = "Map child codes from one coding system to another. Multiple codes may be supplied separated by '|' e.g. 'E10 | E11' for ICD10. Comments may also be included between '<< >>' e.g. 'E10 << T1DM >> | E11 << T2DM >>'."
)

sct_has_attributes_filter <- list(
  id = "sct_has_attributes",
  label = "Has attributes",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_relationship"),
  description = "Retrieve SNOMED CT codes which posess certain attributes. Accepts saved queries as inputs only: the first saved query should be a set of attributes (e.g. '106544002 << Family Enterobacteriaceae (organism) >>'), while the second should be one or more relationship types (e.g. '246075003 << Causative agent (attribute) >>')."
)

sct_get_attributes_filter <- list(
  id = "sct_get_attributes",
  label = "Get attributes",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_relationship"),
  description = "Retrieve attributes for a set of SNOMED CT codes. Accepts saved queries as inputs only: the first saved query should be a set of attributes (e.g. '106544002 << Family Enterobacteriaceae (organism) >>'), while the second should be one or more relationship types (e.g. '246075003 << Causative agent (attribute) >>')."
)

sct_attribute_types_to_filter <- list(
  id = "sct_attribute_types_to",
  label = "Attribute types to",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_attribute_types"),
  description = "Retrieve attribute types that point to a set of SNOMED CT codes. Accepts saved queries as inputs only."
)

sct_attribute_types_from_filter <- list(
  id = "sct_attribute_types_from",
  label = "Attribute types from",
  type = "string",
  input = "select",
  values = list(""),
  operators = list("sct_attribute_types"),
  description = "Retrieve attribute types that point from a set of SNOMED CT codes. Accepts saved queries as inputs only."
)

# sct_attributes_filter_template <- list(
#   id = "sct_attributes",
#   label = "Attributes 246075003 Causative agent (attribute)",
#   type = "string",
#   operators = list("has"),
#   description = "Retrieve SNOMED CT codes based on attributes."
# )

# TO DELETE

# get_sct_attributes_filter_options <- function(sct_saved_queries = NULL) {
#   sct_attributes_filter_saved_query_options <-
#     stringr::str_glue('<option value=\"{sct_saved_queries}\">{sct_saved_queries}</option>')
#
#   result <- c('<option value=\"-1\">-</option>',
#               sct_attributes_filter_saved_query_options) %>%
#     paste(sep = "", collapse = " ")
#
#   result
# }
#
# sct_attributes_filter <- list(
#     id = "sct_attributes",
#     label = "Attributes",
#     type = "string",
#     operators = list("has"),
#     input = "
#       function(rule, name) {
#       var $container = rule.$el.find('.rule-value-container');
#
#       $container.on('change', '[name='+ name +'_1]', function(){
#         var h = '';
#
#         switch ($(this).val()) {
#           case 'A':
#             h = '<option value=\"-1\">-</option> <option value=\"1\">1</option> <option value=\"2\">2</option>';
#             break;
#           case 'B':
#             h = '<option value=\"-1\">-</option> <option value=\"3\">3</option> <option value=\"4\">4</option>';
#             break;
#           case 'C':
#             h = '<option value=\"-1\">-</option> <option value=\"5\">5</option> <option value=\"6\">6</option>';
#             break;
#         }
#
#         $container.find('[name$=_2]')
#           .html(h).toggle(!!h)
#           .val('-1').trigger('change');
#       });
#
#       return '\\
#       <select name=\"'+ name +'_1\"> \\
#         <option value=\"-1\">-</option> \\
#         <option value=\"A\">A</option> \\
#         <option value=\"B\">B</option> \\
#         <option value=\"C\">C</option> \\
#       </select> \\
#       <select name=\"'+ name +'_2\" style=\"display:none;\"></select>';
#     }
#                               ",
#     valueGetter = "function(rule) {
#       return rule.$el.find('.rule-value-container [name$=_1]').val()
#         +'.'+ rule.$el.find('.rule-value-container [name$=_2]').val();
#     }",
#     valueSetter = "function(rule, value) {
#       if (rule.operator.nb_inputs > 0) {
#         var val = value.split('.');
#
#         rule.$el.find('.rule-value-container [name$=_1]').val(val[0]).trigger('change');
#         rule.$el.find('.rule-value-container [name$=_2]').val(val[1]).trigger('change');
#       }
#     }"
#   )

filters <- list(
  description_contains_filter,
  codes_filter,
  map_codes_filter,
  map_children_filter,
  child_codes_filter,
  empty_saved_query_filter,
  sct_has_attributes_filter,
  sct_get_attributes_filter,
  sct_attribute_types_to_filter,
  sct_attribute_types_from_filter
)

### Operators -----------------------------------------------------------------

code_type_operators <- CODE_TYPE_TO_LKP_TABLE_MAP %>%
  dplyr::pull(.data[["code"]]) %>%
  purrr::map(\(x) list(
    type = x,
    nb_inputs = 1,
    multiple = FALSE,
    apply_to = "string"
  ))

operators <- c(code_type_operators,
               list(
                 list(
                   type = "equals",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "from ICD-10",
                   optgroup = "Map",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "ALL",
                   nb_inputs = 1,
                   multiple = FALSE,
                   apply_to = "string"
                 ),
                 list(
                   type = "sct_relationship",
                   nb_inputs = 2,
                   multiple = FALSE,
                   input = "select",
                   values = list(""),
                   apply_to = "string"
                 ),
                 list(
                   type = "sct_attribute_types",
                   nb_inputs = 1,
                   multiple = FALSE,
                   input = "select",
                   values = list(""),
                   apply_to = "string"
                 )
               ))

# UI ----------------------------------------------------------------------

ui <- shinyUI(fluidPage(jqbr::useQueryBuilder(),
                        titlePanel("shinyAce auto completion - combine completion lists"),
                        # radioButtons("dataset", "Dataset: ", c("mtcars", "airquality"), inline = TRUE),
                        uiOutput("ace_editor"),
                        actionButton("update_qbr", "Update qbr"),
                        jqbr::queryBuilderInput(
                          "qb",

                          plugins = list(
                            "sortable" = NULL,
                            "filter-description" = list("mode" = "bootbox"),
                            "bt-tooltip-errors" = NULL
                          ),

                          filters = filters,

                          operators = operators,

                          rules = list(condition = "AND",
                                       rules = list(
                                         list(
                                           id = "description",
                                           operator = "read2",
                                           value = "diab",
                                           description = "I'm a description"
                                         )
                                       )),
                          conditions = c("AND", "OR", "NOT"),
                          return_value = "rules",
                          display_errors = TRUE
                        ),

                        # TODO
                        fluidRow(column(6, wellPanel(
                          h4("Translated shinyACE input"),
                          verbatimTextOutput("current_query_tree")
                        )), column(6, wellPanel(
                          h4("Current QBR"),
                          verbatimTextOutput("current_qbr")
                        )))
))


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  ## Dataset Selection
  # dataset <- reactive({
  #   get(input$dataset)
  # })

  observeEvent(
    list(
      # input$code_type,
      # saved_queries(),
      # input$btn_save_updated_query,
      # input$cancel_query_update
      input$update_qbr
    ),
    {
      # update qbr saved query filter
      # available_saved_queries <- saved_queries()$objects[[input$code_type]]
      #
      # new_filters <- update_qbr_filters(input_code_type = input$code_type,
      #                                   available_maps = available_maps,
      #                                   available_saved_queries = as.list(available_saved_queries))
      req(input$update_qbr)

      # browser()

      new_rules <- current_query()

      if (purrr::pluck_depth(new_rules) == 2L) {
        # needed if single query statement e.g. `DESCRIPTION("diab")` (but not
        # `DESCRIPTION("diab") %OR% DESCRIPTION("retin")`)
        new_rules <- list(new_rules)
      }
# diab_ret_neph_neuro <- readRDS("~/Documents/phd/r_packages/codemapper/diab_ret_neph_neuro.rds")

      jqbr::updateQueryBuilder(
        inputId = "qb",
        # setFilters = new_filters,
        setRules = new_rules
        # setRules = get("DIAB_RETINOP_OR_NEPHROP_OR_NEUROP", diab_ret_neph_neuro$saved_queries()$results_meta)$qb
        # setRules = list(
        #   list(
        #     id = "description",
        #     field = "description",
        #     type = "string",
        #     input = "text",
        #     operator = "read2",
        #     value = "diab2",
        #     # description = "I'm a description",
        #     valid = TRUE
        #   )
        # )
        # setRules = update_qb_operator_code_type(input$qb, input$code_type),
        # destroy = TRUE
      )
    }
  )

  comps <- reactive({
    comps <- list()
    # comps[[input$dataset]] <- colnames(dataset())
    comps <- c(comps, list(dplyr = getNamespaceExports("dplyr")))
  })

  output$ace_editor <- renderUI({
    ## initially, only show completions in 'comps' (i.e., dplyr and selected dataset)
    shinyAce::aceEditor(
      "editor",
      mode = "r",
      value = 'DESCRIPTION("diab") %AND% DESCRIPTION("retin|mac") %NOT%
    DESCRIPTION("absent|without") %OR% ((DESCRIPTION("diab") %AND%
    DESCRIPTION("nephro|neuro") %NOT% DESCRIPTION("absent|without")))',
      height = "200px",
      autoComplete = "live",
      autoCompleters = "static",
      autoCompleteList = isolate(comps())
    )
  })

  ## Update static auto complete list according to dataset and add local completions
  observe({
    shinyAce::updateAceEditor(
      session,
      "editor",
      # autoCompleters = c("static", "text", "rlang"),
      autoCompleters = c("static", "text"),
      autoCompleteList = comps()
    )
  })

  ## adding an observer for R-language code completion
  ## will become active after the first switch to another
  ## dataset
  ace_completer <- aceAutocomplete("editor")
  ace_annotator <- aceAnnotate("editor")
  ace_tooltip   <- aceTooltip("editor")

  # show query
  current_query <- reactive({
    result <- input$editor |>
      rlang::parse_expr() |>
      expr_to_list() |>
      flatten_brackets() |>
      transform_query()

    result$valid <- NULL
    result
  })

  output$current_query_tree <- renderPrint({
    list(update_qb_operator_code_type(current_query(), code_type = "read2")) |>
      lobstr::tree()
  })

  output$current_qbr <- renderPrint(
    input$qb |>
      lobstr::tree()
  )
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
