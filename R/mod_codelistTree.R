codelistTreeInput <- function(id) {
  ns <- NS(id)

  # UI
  tagList(
    actionButton(ns("show_tree"), "Show codelist tree", class = "btn-lg btn-success"),
    shinyTree(
      ns("tree"),
      search = TRUE,
      searchtime = 1000,
      checkbox = TRUE,
      three_state = FALSE,
      unique = TRUE,
      sort = FALSE,
      theme = "proton"
    )
  )
}

codelistTreeServer <- function(id, codelist_tree, reset_signal) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tree_data <- reactiveVal(NULL)

    observeEvent(input$show_tree, {
      tree_data(codelist_tree)
    })

    observeEvent(reset_signal(), {
      print("Reset triggered")
      tree_data(NULL)
    }, ignoreInit = TRUE)

    output$tree <- renderTree({
      req(tree_data())
    })

    selected_tree_items <- reactive({
      tree <- input$tree
      req(tree)
      get_selected(tree, format = "slices")
    })

    selected_tree_items
  })
}

# for testing
codelistTreeApp <- function() {
  ui <- fluidPage(actionButton("reset", "Reset", class = "btn btn-danger"),
                  codelistTreeInput("codelist_tree"),
                  verbatimTextOutput("selected_tree_items"))

  server <- function(input, output, session) {
    selected_tree_items <- codelistTreeServer("codelist_tree", codelist_tree = c(list(
      `Physical object` = list(
        `261382003: Mask (physical object)` = list(
          `720029003: Diabetic retinopathy phototherapy mask (physical object)` = ""
        ),
        `303634002: Ophthalmological device (physical object)` = list(
          `720029003: Diabetic retinopathy phototherapy mask (physical object)` = ""
        ),
        `400912000: Visual acuity test equipment (physical object)` = list(
          `400914004: Early Treatment of Diabetic Retinopathy Study visual acuity chart (physical object)` = ""
        )
      ),
      Environment = list(
        `257585005: Clinic (environment)` = list(`702850009: Diabetic retinopathy clinic (environment)` = "")
      )
    )),
    reset_signal = reactive(input$reset)
    )

    output$selected_tree_items <- renderPrint(selected_tree_items())
  }

  shinyApp(ui, server)
}
