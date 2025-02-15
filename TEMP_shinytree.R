library(shiny)
library(shinyTree)

# hm_result <- readRDS("hm_result.rds")
# example_list <- as.list(hm_result)
# names(example_list)[1] <- "sct"
# example_list <- example_list[-1][1:2]

example_list <- readRDS("dr_tree_list.rds")

# names(example_list)[1] <- "sct"

# example_list <- example_list[-1][1]

ui <- fluidPage(
  shinyTree(
    "tree",
    search = TRUE,
    searchtime = 1000,
    checkbox = TRUE,
    three_state = FALSE
  ),
  "Currently Selected:",
  # verbatimTextOutput("sel_names"),
  # verbatimTextOutput("sel_names_parent_or_child")
  # verbatimTextOutput("sel_slices"),
  # verbatimTextOutput("sel_classid")
  verbatimTextOutput("sel_slices_parent_or_child")
)

server <- function(input, output, session) {
  output$tree <- renderTree(example_list)

  # output$sel_names <- renderPrint({
  #   tree <- input$tree
  #   req(tree)
  #   get_selected(tree)
  # })

  # output$sel_names_parent_or_child <- renderPrint({
  #   thing()
  #   })
  #
  # thing <- reactive({
  #   tree <- input$tree
  #   req(tree)
  #   parent_or_child <- get_selected(tree)
  #   parent_or_child
  # })

  # output$sel_slices <- renderPrint({
  #   tree <- input$tree
  #   req(tree)
  #   get_selected(tree, format = "slices")
  # })

  output$sel_slices_parent_or_child <- renderPrint({
    tree <- input$tree
    req(tree)
    parent_or_child <- get_selected(tree, format = "slices")
    # browser()
    parent_or_child
  })

  # output$sel_classid <- renderPrint({
  #   tree <- input$tree
  #   req(tree)
  #   get_selected(tree, format = "classid")
  # })
}

shinyApp(ui, server)
