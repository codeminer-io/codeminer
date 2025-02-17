codelistReactableInput <- function(id) {
  ns <- NS(id)

  # UI
  tagList(
    csvDownloadButton(ns("codelist"), filename = paste0(Sys.Date(), "_", "codelist.csv")),
    reactable::reactableOutput(ns("codelist")),
    verbatimTextOutput(ns("selected_codes")),
    verbatimTextOutput(ns("selected_codes_no_description"))
  )
}

codelistReactableServer <-
  function(id, df_reactable, extract_fn) {

    moduleServer(id, function(input, output, session) {

      ns <- session$ns

      df <- reactive({
        stopifnot(is.reactive(df_reactable))
        extract_fn(df_reactable())
        })

      output$codelist <- reactable::renderReactable({
        req(is.data.frame(df()))
        app_reactable(df())
        })

      selected <-
        reactive({
          rowids <- reactable::getReactableState("codelist", "selected")

          req(isTruthy(rowids))

          df_selected <- df()[rowids, 1:2]

          list(
            with_description = stringr::str_glue("{df_selected[[1]]} << {df_selected[[2]]} >>") %>%
              paste(sep = "", collapse = " | "),
            no_description = stringr::str_glue("{df_selected[[1]]}") %>%
              paste(sep = "", collapse = "|")
          )
          })

      output$selected_codes <- renderPrint(cat(selected()$with_description))
      output$selected_codes_no_description <- renderPrint(cat(selected()$no_description))

      # returns reactive
      selected
    })
  }

# for testing
codelistReactablesApp <- function(df_reactable = reactive(CHILDREN("16747741000119100", "sct")),
                                  extract_fn = function(x) x) {
  ui <- fluidPage(codelistReactableInput("codelist_reactable"))

  server <- function(input, output, session) {
    codelistReactableServer("codelist_reactable", df_reactable, extract_fn)
  }

  shinyApp(ui, server)
}
