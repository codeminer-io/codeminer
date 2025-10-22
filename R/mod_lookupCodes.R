lookupCodesInput <- function(id, available_code_types) {
  ns <- NS(id)

  tagList(shinyjs::useShinyjs(),
          tabsetPanel(id = ns("mainpanel"),
                      tabPanel(title = "Search", icon = icon("magnifying-glass"),
                               fluidRow(
                                 column(
                                   4,
                                   selectInput(
                                     ns("code_type"),
                                     "Code type",
                                     choices = get_code_type_labels(available_code_types = available_code_types)
                                   ),
                                   textAreaInput(ns("codes_input"), "Input", resize = "vertical"),
                                   radioButtons(ns("map_to"),
                                                "Map to",
                                                choices = "(Don't map)"),
                                   actionButton(ns("look_up"), "Look up")
                                 ),
                                 column(
                                   8,
                                   textInput(ns("save_as"), "Save as"),
                                   shinyjs::disabled(actionButton(ns(
                                     "save_recognised"
                                   ), "Save")),
                                   tabsetPanel(
                                     id = ns("codelist_tabs"),
                                     tabPanel("Recognised",
                                              codelistReactableInput(ns("recognised_codes"))),
                                     tabPanel("Unrecognised",
                                              codelistReactableInput(ns("unrecognised_codes")))
                                   )
                                 )
                               )),
                      tabPanel(
                        "Saved lookups",
                        icon = icon("table-list"),
                        selectInput(ns("select_saved_lookup_code_type"), "Code type", choices = NULL),
                        selectInput(ns("select_saved_lookup"), "Saved lookup", choices = NULL),
                        actionButton(ns("select_saved_lookup_button"), "Select"),
                        actionButton(ns("remove_lookup"), "Remove"),
                        codelistReactableInput(ns("saved_lookup_reactable"))
                      ),
                      tabPanel(
                        "Advanced settings",
                        icon = icon("gears"),
                        selectColFiltersInput(ns("lookup_advanced_settings"),
                                              display_filters = FALSE)
                      )))
}

lookupCodesServer <-
  function(id,
           available_maps,
           saved_lookups = reactiveVal(list())) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      col_filters <-
        selectColFiltersServer("lookup_advanced_settings", confirmation_modal = FALSE)

      codes_input_cleaned <- reactive(
        input$codes_input %>%
          stringr::str_split_1("\n") %>%
          stringr::str_trim(side = "both") %>%
          subset(.,
                 . != "")
      )

      observe({
        # require input to look up
        shinyjs::toggleState(ns("look_up"),
                             condition = isTruthy(codes_input_cleaned()),
                             asis = TRUE)
      })

      observe({
        updateRadioButtons(
          inputId = "map_to",
          choices = c(
            list("(Don't map)"),
            get_available_map_from_code_types(available_maps = available_maps,
                                              to = input$code_type) %>%
              get_code_type_labels()
          )
        )
      })

      codelist <-
        eventReactive(input$look_up,
                      ignoreInit = TRUE,
                      valueExpr = {
                        code_type <- input$code_type

                        recognised <- lookup_codes(
                          codes = codes_input_cleaned(),
                          code_type = input$code_type,
                          preferred_description_only = TRUE,
                          standardise_output = TRUE,
                          unrecognised_codes = "warning",
                          col_filters = col_filters()
                        )

                        if (input$map_to != "(Don't map)") {
                          recognised <- MAP(
                            codes = recognised,
                            to = input$map_to,
                            unrecognised_codes = "warning",
                            reverse_mapping = "warning",
                            col_filters = col_filters()
                          )

                          code_type <- input$map_to
                        }

                        unrecognised <- lookup_codes(
                          codes = codes_input_cleaned(),
                          code_type = input$code_type,
                          preferred_description_only = TRUE,
                          standardise_output = TRUE,
                          .return_unrecognised_codes = TRUE,
                          col_filters = col_filters()
                        )

                        list(
                          recognised = recognised,
                          unrecognised = unrecognised,
                          code_type = code_type
                        )
                      })

      observe({
        # require input to look up
        shinyjs::toggleState(
          ns("save_recognised"),
          condition = (nrow(codelist()$recognised) > 0) &
            isTruthy(input$save_as),
          asis = TRUE
        )
      })

      codelistReactableServer("recognised_codes", codelist, function(x) x$recognised)

      codelistReactableServer("unrecognised_codes", codelist, function(x) x$unrecognised)

      observeEvent(input$save_recognised, {
        new_saved_lookups <- saved_lookups()

        new_saved_lookups[[codelist()$code_type]][[input$save_as]] <-
          codelist()$recognised

        saved_lookups(new_saved_lookups)
      })

      # view saved lookups tab
      observe({
        choices <- names(saved_lookups()) %>%
          get_code_type_labels()

        if (is.null(choices)) {
          choices <- list("")
        }

        updateSelectInput(inputId = "select_saved_lookup_code_type",
                          choices = choices)
      })

      observe({
        choices <- names(saved_lookups()[[input$select_saved_lookup_code_type]])

        if (is.null(choices)) {
          choices <- ""
        }

        updateSelectInput(inputId = "select_saved_lookup",
                          choices = choices)
      })

      selected_lookup <- eventReactive(input$select_saved_lookup_button, {

        req(isTruthy(input$select_saved_lookup_code_type))
        req(isTruthy(input$select_saved_lookup))

        list(codelist = saved_lookups()[[input$select_saved_lookup_code_type]][[input$select_saved_lookup]],
             code_type = input$select_saved_lookup_code_type,
             name = input$select_saved_lookup)
      })

      observe({
        shinyjs::toggleState(
          id = ns("select_saved_lookup_button"),
          condition = isTruthy(input$select_saved_lookup_code_type) &
            isTruthy(input$select_saved_lookup),
          asis = TRUE
        )
      })

      observe({
        shinyjs::toggleState(
          id = ns("remove_lookup"),
          condition = isTruthy(input$select_saved_lookup_code_type) &
            isTruthy(input$select_saved_lookup),
          asis = TRUE
        )
      })

      observe({
        shinyjs::toggle(
          id = ns("saved_lookup_reactable"),
          condition = (selected_lookup()$code_type %in% names(saved_lookups())) &
            (selected_lookup()$name %in% names(saved_lookups()[[selected_lookup()$code_type]])),
          asis = TRUE
        )
      })

      codelistReactableServer("saved_lookup_reactable", selected_lookup, function(x) x$codelist)
      # output$saved_lookup_reactable <- reactable::renderReactable({
      #   req(isTruthy(selected_lookup()))
      #
      #   app_reactable(selected_lookup()$codelist)
      # })

      # remove saved lookup
      observeEvent(input$remove_lookup, {
        new_saved_lookup <- saved_lookups()

        new_saved_lookup[[input$select_saved_lookup_code_type]][[input$select_saved_lookup]] <-
          NULL

        new_saved_lookup <- purrr::compact(new_saved_lookup)

        if (!isTruthy(names(new_saved_lookup))) {
          new_saved_lookup <- list()
        }

        saved_lookups(new_saved_lookup)
      })

    })}
