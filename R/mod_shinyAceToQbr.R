
shinyAceToQbrInput <- function(id) {
  ns <- NS(id)

  shinyUI(fluidPage(
    jqbr::useQueryBuilder(),
    titlePanel("shinyAce auto completion - combine completion lists"),
    uiOutput(ns("ace_editor")),
  ))
}

shinyAceToQbrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    comps <- reactive({
      comps <- list()
      comps <- c(comps, list(codemapper = getNamespaceExports("codemapper")))
    })

    output$ace_editor <- renderUI({
      shinyAce::aceEditor(
        ns("editor"),
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

    # ## Update static auto complete list
    # observe({
    #   shinyAce::updateAceEditor(
    #     session,
    #     "editor",
    #     autoCompleters = c("static", "text"),
    #     autoCompleteList = comps()
    #   )
    # })

    ## adding an observer for R-language code completion
    ## will become active after the first switch to another
    ## dataset
    ace_completer <- shinyAce::aceAutocomplete("editor")
    ace_annotator <- shinyAce::aceAnnotate("editor")
    ace_tooltip   <- shinyAce::aceTooltip("editor")

    # show query
    current_query <- reactive({
      req(input$editor)
      result <- input$editor |>
        translate_codeminer_query_to_qbr_list()

      result
    })

    current_query
  })
}

# for testing
shinyAceToQbrApp <- function() {
  ui <- fluidPage(shinyAceToQbrInput("shiny_ace_input"),
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

                    rules = list(condition = "AND", rules = list(
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

                  h4("Translation identical to QBR?"),
                  verbatimTextOutput("identical_translation_qbr"),

                  fluidRow(column(6, wellPanel(
                    h4("Translated shinyACE input"),
                    verbatimTextOutput("current_query_tree")
                  )), column(6, wellPanel(
                    h4("Current QBR"), verbatimTextOutput("current_qbr")
                  )))
  )

  server <- function(input, output, session) {

    current_query <- shinyAceToQbrServer("shiny_ace_input")

    observeEvent(list(input$update_qbr), {
      # update qbr saved query filter
      req(input$update_qbr)

      new_rules <- current_query()

      valid_new_rules <- !identical(current_query(), list())

      if (!valid_new_rules) {
        showNotification(
          "Invalid query",
          type = "error",
          duration = 5,
          closeButton = TRUE
        )
      }

      req(valid_new_rules)

      # finalise qbr input before `jqbr::updateQueryBuilder()`
      new_rules$valid <- NULL

      jqbr::updateQueryBuilder(inputId = "qb", setRules = new_rules)
    })

    output$current_query_tree <- renderPrint({
      # list(update_qb_operator_code_type(current_query(), code_type = "read2")) |>
      current_query() |>
        lobstr::tree()
    })

    output$current_qbr <- renderPrint(input$qb |>
                                        lobstr::tree())

    output$identical_translation_qbr <- renderPrint(
      identical(current_query(), input$qb)
    )
  }

  shinyApp(ui, server)
}

# Helper functions --------------------------------------------------------

translate_codeminer_query_to_qbr_list <- function(query_string) {
  result <- tryCatch(query_string |>
    rlang::parse_expr() |>
    expr_to_list() |>
    flatten_brackets() |>
    transform_query(),
    error = function(cnd) list())

  if (identical(purrr::pluck_depth(result), 2L)) {
    # needed if single query statement e.g. `DESCRIPTION("diab")` (but not
    # `DESCRIPTION("diab") %OR% DESCRIPTION("retin")`)
    result$valid <- NULL
    result <- list(result)
  }

  return(result)
}

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
