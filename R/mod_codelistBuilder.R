#' UI for codelistBuilder shiny module
#'
#' @param id character
#' @param available_code_types Character vector of code types (must match code
#'   types in `CODE_TYPE_TO_LKP_TABLE_MAP$code`)
#' @param available_maps Data frame with columns 'from' and 'to' (must match
#'   'from-to' and 'to-from' combinations in `CLINICAL_CODE_MAPPINGS_MAP`)
#'
#' @return UI (html)
#' @noRd
#' @import shiny
codelistBuilderInput <-
  function(id, available_code_types, available_maps) {
    stopifnot(all(available_code_types %in% CODE_TYPE_TO_LKP_TABLE_MAP$code))

    ns <- NS(id)

    tagList(
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs(),
      jqbr::useQueryBuilder(),

      tabsetPanel(
        id = ns("mainpanel"),
        tabPanel(
          "Build query",
          icon = icon("pen"),

          ## Side bar -------------------------------
          sidebarLayout(
            sidebarPanel = sidebarPanel(
              width = 2,
              actionButton(ns("reset_qbr"), "Reset"),
              tabsetPanel(
                id = ns("tabs_select_code_type"),
                type = "hidden",
                tabPanelBody(
                  value = "tab_select_code_type_show",
                  radioButtons(
                    ns("code_type"),
                    "Code type",
                    choices = get_code_type_labels(available_code_types = available_code_types)
                  )
                ),
                tabPanelBody(
                  value = "tab_select_code_type_hide",
                  textOutput(ns("currently_updating_query")),
                  actionButton(ns("cancel_query_update"), "Cancel query update")
                )
              ),
              tabsetPanel(
                id = ns("tabs_load_saved_query"),
                type = "hidden",
                tabPanelBody(value = "tab_load_saved_query_hide"),
                tabPanelBody(
                  value = "tab_load_saved_query_show",
                  actionButton(ns("btn_qb_load_saved_query"), "Load/update query"),
                  ### Saved queries -----------------------------------------------------------

                  selectizeInput(
                    ns("saved_queries_selectize"),
                    "Remove saved queries",
                    choices = list("None"),
                    multiple = TRUE
                  ),
                  tabsetPanel(
                    id = ns("tabs_remove_saved_queries"),
                    type = "hidden",
                    tabPanelBody(value = "tab_remove_saved_queries_hide"),
                    tabPanelBody(value = "tab_remove_saved_queries_show",
                                 actionButton(ns(
                                   "remove_saved_queries"
                                 ), "Remove"))
                  )
                )
              )
            ),

            ## Main panel ------------------------------------
            mainPanel = mainPanel(

              ### shinyAce input --------------------

              a(id = ns("toggleshiny_ace_input"), "Show/hide CodeMiner query text editor", href = "#"),
              shinyjs::hidden(div(id = ns("hide_show_shiny_ace_input"),
              shinyAceToQbrInput(ns("shiny_ace_input")),
              actionButton(ns("shinyace_update_qbr"), "Update builder", class = "btn btn-success")
              )),

              ### Query builder input -----------------------------------------------------
              jqbr::queryBuilderInput(
                ns("qb"),

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
                                 operator = "sct",
                                 value = "diabetic retinopathy",
                                 description = "I'm a description"
                               )
                             )),
                conditions = c("AND", "OR", "NOT"),
                return_value = "rules",
                display_errors = TRUE
              ),

              ### Summarise and run query -------------------------------------------------

              verbatimTextOutput(ns("current_query")),
              actionButton(ns("run"), "Run query", class = "btn-lg btn-success"),


              ### Query results -----------------------------------------------------------

              h1("Result"),
              tabsetPanel(
                id = ns("query_result_tabs"),
                type = "hidden",
                tabPanelBody(value = "initial"),
                tabPanelBody(value = "query_result_empty_or_invalid",
                             verbatimTextOutput(
                               ns("result_empty_or_invalid_message")
                             )),
                tabPanelBody(value = "empty_query",
                             p("Please enter a query")),
                tabPanelBody(
                  value = "query_result",
                  uiOutput(ns("result_query")),
                  # verbatimTextOutput(ns("result_query")),
                  actionButton(ns("download_report"), "Download report", class = "btn-success"),
                  tabsetPanel(
                    id = ns("tabs_save_or_update_query"),
                    type = "hidden",
                    tabPanelBody(
                      value = "tab_save_query_input_show",
                      textInput(
                        ns("save_query_name"),
                        "Save as",
                        value = "",
                        placeholder = "Use capitals"
                      ),
                      tabsetPanel(
                        id = ns("tabs_show_save_query_button"),
                        type = "hidden",
                        tabPanelBody(value = "tab_save_query_button_hide"),
                        tabPanelBody(value = "tab_save_query_button_show",
                                     actionButton(ns("save_query"), "Save"))
                      )
                    ),
                    tabPanelBody(
                      value = "tab_update_query_button_show",
                      actionButton(ns("btn_save_updated_query"), "Update", class = "btn-lg btn-danger")
                    )
                  ),
                  textOutput(ns("result_summary")),
                  tabsetPanel(id = "tabs_codelist_reactable_tree",
                              type = "pills",
                              tabPanel(title = "Table",
                                       icon = icon("table"),
                                       codelistReactableInput(ns("result"))),
                              tabPanel(title = "Tree",
                                       icon = icon("folder-tree"),
                                       codelistTreeInput(ns("result_tree"))))
                )
              )
            )
          )
        ),

        ### Saved queries -----------------------------------
        tabPanel(
          "Saved queries",
          icon = icon("diagram-project"),
          tabsetPanel(
            id = ns("tabs_dag"),
            tabPanel(title = "DAG",
                     visNetwork::visNetworkOutput(ns("dag_visnetwork"))),
            tabPanel(title = "Nodes",
                     tableOutput(ns("dag_nodes"))),
            tabPanel(title = "Edges",
                     tableOutput(ns("dag_edges")))
          )
        ),

        ### Import from text query -------------------------------

        tabPanel(
          "Import query",
          icon = icon("upload"),
            selectInput(
              ns("import_code_type"),
              "Code type",
              choices = get_code_type_labels(available_code_types = available_code_types)
            ),
            shinyAceToQbrInput(ns("shinyace_import_input")),
            actionButton(ns("shinyace_import_button"), "Import", class = "btn btn-success")
        ),

        ### Advanced settings ------------------------------------
        tabPanel(
          "Advanced settings",
          icon = icon("gears"),
          selectColFiltersInput(ns("builder_advanced_settings"),
                                display_filters = FALSE)
        )

      )
    )
  }


#' UI for codelistBuilder shiny module
#'
#' @param id character
#' @param available_code_types Character vector of code types (must match code types in `CODE_TYPE_TO_LKP_TABLE_MAP$code`)
#'
#' @return UI (html)
#' @noRd
#' @import shiny
codelistBuilderServer <-
  function(id,
           available_maps,
           saved_queries = reactiveVal(list(
             objects = list(),
             results = new.env(),
             results_meta = new.env(),
             dag = list(nodes = data.frame(),
                        edges = data.frame())
           ))) {

    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      ## Advanced settings -------------------------------------------------------

      col_filters <-
        selectColFiltersServer("builder_advanced_settings", confirmation_modal = TRUE)

      query_options <- reactive(rlang::expr(
        list(
          codemapper.code_type = !!input$code_type,
          codemapper.map_to = !!input$code_type,
          codemapper.reverse_mapping = "warning",
          codemapper.unrecognised_codes_mapped = "warning",
          codemapper.unrecognised_codes_lookup = "error",
          codemapper.col_filters = !!col_filters()
        )
      ))

      # Update all saved queries if col_filters() updates

      # TODO - optimise
      observeEvent(col_filters(),
                   recompute_all_queries(saved_queries = saved_queries,
                                         query_options = query_options))


      ## Import from query text --------------------------------------------------

      shinyace_queries <- shinyAceToQbrServer("shinyace_import_input",
                                              height = "500px",
                                              initial_value = '# BNF child codes, mapped to SCT
bnf((BNF_STATIN_CHEMICAL_SUBSTANCES = CHILDREN("0212000AA << Rosuvastatin Calcium >> | 0212000AC << Simvastatin & Ezetimibe >> | 0212000AJ << Fenofibrate/Simvastatin >> | 0212000B0 << Atorvastatin >> | 0212000C0 << Cerivastatin >> | 0212000M0 << Fluvastatin Sodium >> | 0212000R0 << Lovastatin >> | 0212000X0 << Pravastatin Sodium >> | 0212000Y0 << Simvastatin >>")))\n
sct((CHILDREN_MAPPED_BNF_STATIN_CHEMICAL_SUBSTANCES = MAP(BNF_STATIN_CHEMICAL_SUBSTANCES, from = "bnf")))\n
# SCT child codes
sct((CHILDREN_PRODUCT_CONTAINING_STATIN = CHILDREN("96302009 << Product containing 3-hydroxy-3-methylglutaryl-coenzyme A reductase inhibitor (product) >>")))\n
# SCT codes with statin active ingredient
sct((CHILDREN_STATIN_SUBSTANCE = CHILDREN("372912004 << Substance with 3-hydroxy-3-methylglutaryl-coenzyme A reductase inhibitor mechanism of action (substance) >>")))\nsct((HAS_INGREDIENT = CODES("127489000 << Has active ingredient (attribute) >> | 762949000 << Has precise active ingredient (attribute) >> |  10362801000001104 << Has specific active ingredient (attribute) >>")))\nsct((HAS_STATIN_SUBSTANCE = HAS_ATTRIBUTES(CHILDREN_STATIN_SUBSTANCE, relationship = HAS_INGREDIENT)))\n
# Combine above to a single SCT codelist
sct((STATINS_CODELIST_SCT = CHILDREN_MAPPED_BNF_STATIN_CHEMICAL_SUBSTANCES %OR% CHILDREN_PRODUCT_CONTAINING_STATIN %OR% HAS_STATIN_SUBSTANCE))\n
# Map to CPRD prodcodes
STATINS_CODELIST_PRODCODE = MAP(STATINS_CODELIST_SCT, from = "sct")',
                                              single_query_only = FALSE)

      # confirm selected choices
      observeEvent(input$shinyace_import_button, {
        showModal(
          modalDialog(
            "This will remove all currently saved queries. Are you sure you want to continue?",
            title = "Confirm",
            footer = tagList(
              actionButton(ns("confirm_cancel_import"), "Cancel"),
              actionButton(ns("confirm_proceed_import"), "Proceed", class = "btn btn-danger")
            )
          )
        )
      })

      observeEvent(input$confirm_cancel_import, {
        removeModal()
      })

      observeEvent(
        input$confirm_proceed_import,
        handlerExpr = {
          # progress bar
          withProgress(message = "Importing...", {
            step <- 1

            # reset, saving current environment in case import fails
            empty_saved_queries <- list(
              objects = list(),
              results = new.env(),
              results_meta = new.env(),
              dag = list(nodes = data.frame(), edges = data.frame())
            )

            old_saved_queries <- saved_queries()

            saved_queries(empty_saved_queries)

            # import queries
            for (i in seq_along(shinyace_queries())) {

              print(deparse1(shinyace_queries()[[i]]$query_call[[2]]))

              query_call <- shinyace_queries()[[i]]$query_call

              # determine whether query statement is from a different code_type
              # e.g. `icd10(CYST = DESCRIPTION("cyst"))` needs code_type "icd10"
              query_code_type <- names(shinyace_queries())[i]

              if (!is.null(query_code_type) & !identical(input$import_code_type, query_code_type) & query_code_type %in% CODE_TYPE_TO_LKP_TABLE_MAP$code) {
                query_code_type <- query_code_type
              } else {
                query_code_type <- input$import_code_type
              }

              if (!check_if_call_has_assignment(query_call)) {
                showNotification(
                  paste0("Error with: `", deparse1(query_call), "`. Aborting import"),
                  type = "error",
                  duration = 10
                )
                showNotification(
                  paste0("All statements should be assigned with `=`"),
                  type = "warning",
                  duration = 10
                )
                # reset to previous state
                saved_queries(old_saved_queries)
                showNotification(
                  paste0("Previous state has been restored"),
                  type = "default",
                  duration = 10
                )
                break()
              }

              # get
              query_call <- query_call[[3]]

              import_query_options <- query_options()
              import_query_options$codemapper.code_type <- query_code_type
              import_query_options$codemapper.map_to <- query_code_type
              import_query_options <- reactiveVal(import_query_options)

              .query_result_temp <- reactiveVal(run_query(
                  query = query_call,
                  code_type = query_code_type,
                  qb = shinyace_queries()[[i]]$qbr,
                  query_options = import_query_options,
                  saved_queries = saved_queries,
                  dag_igraph = dag_igraph
                ))

              if (rlang::is_error(.query_result_temp()$error)) {
                showNotification(paste0(
                  "Error for `",
                  deparse1(shinyace_queries()[[i]]$query_call[[2]]),
                  "`. Aborting import - ",
                  as.character(.query_result_temp()$error)
                ),
                type = "error")

                # reset to previous state
                saved_queries(old_saved_queries)
                showNotification(
                  paste0("Did you mean to select a different import code type?"),
                  type = "message",
                  duration = 10
                )
                showNotification(
                  paste0("Previous state has been restored"),
                  type = "default",
                  duration = 10
                )
                break()
              }

              if (identical(nrow(.query_result_temp()$result), 0L)) {
                showNotification(paste0(
                  "No matching codes found for `",
                  deparse1(shinyace_queries()[[i]]$query_call[[2]]),
                  "`. Aborting import"
                ),
                type = "error")

                # reset to previous state
                saved_queries(old_saved_queries)
                showNotification(
                  paste0("Did you mean to select a different import code type?"),
                  type = "message",
                  duration = 10
                )
                showNotification(
                  paste0("Previous state has been restored"),
                  type = "default",
                  duration = 10
                )
                break()
              }

              update_saved_queries(
                query = deparse1(shinyace_queries()[[i]]$query_call[[2]]),
                query_result = .query_result_temp,
                saved_queries = saved_queries,
                code_type = query_code_type,
                query_options = query_options,
                ignore_existing_query = FALSE
              )

              incProgress(1 / length(shinyace_queries()))
              step <- step + 1
            }
          })

          rm(".query_result_temp")

          message("Complete!")
          print(step)
          print(saved_queries())
          removeModal()
        }
      )

      ## Query -------------------------------------------------------------------

      output$current_query <- renderPrint({
        if (is.null(input$qb)) {
          cat("<Enter query>")
        } else {
          custom_qbr_translation(input$qb, saved_queries()$dag$nodes) %>%
            rlang::expr_text() %>%
            cat()
        }
      })

      ## Update query builder from shinyAce --------------------------

      shinyjs::onclick("toggleshiny_ace_input",
        shinyjs::toggle(id = "hide_show_shiny_ace_input", anim = TRUE)
      )

      shinyace_qbr_query <- shinyAceToQbrServer("shiny_ace_input",
                                                height = "150px")

      observeEvent(input$shinyace_update_qbr, {
        # update qbr saved query filter
        req(input$shinyace_update_qbr)

        new_rules <- shinyace_qbr_query()

        valid_new_rules <- !identical(shinyace_qbr_query(), list())

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

      ## Reset query builder -----------------------------------------------------

      observeEvent(input$reset_qbr,
                   jqbr::updateQueryBuilder(
                     inputId = "qb",
                     setRules = list(condition = "AND",
                                     rules = list(
                                       list(
                                         id = "description",
                                         operator = "read2",
                                         value = "diab",
                                         description = "I'm a description"
                                       )
                                     ))
                   ))


      ## Run query ---------------------------------------------------------------

      query_result <- eventReactive(input$run, {
        # `FALSE` if no query input; `NA` if no results returned by query
        if (is.null(input$qb)) {
          x <- FALSE
        } else {
          query <- custom_qbr_translation(input$qb, saved_queries()$dag$nodes)

          x <- run_query(
            query = query,
            code_type = input$code_type,
            qb = input$qb,
            query_options = query_options,
            saved_queries = saved_queries,
            dag_igraph = dag_igraph
          )

          print(lobstr::tree(input$qb))
        }

        x
      })

      query_result_type <- eventReactive(query_result(), {
        if (identical(query_result(), FALSE)) {
          x <- "empty_query"
        } else if (identical(names(query_result()), c("result", "error"))) {
          x <- "query_result_empty_or_invalid"
        } else {
          x <- "query_result"
        }

        x
      })

      observeEvent(query_result_type(), {
        updateTabsetPanel(inputId = "query_result_tabs", selected = query_result_type())
      })

      output$result_empty_or_invalid_message <- renderText({
        req(query_result_type() == "query_result_empty_or_invalid")

        if (!rlang::is_empty(query_result()$error)) {
          validate(paste0("Error! ", query_result()$error$message))
        }

        "No codes found matching search criteria"
      })

      output$result_summary <- renderText({
        req(query_result_type() == "query_result")

        result <- paste0("N results: ",
                         nrow(query_result()$result),
                         ".")

        message(result)

        print(result)
      })

      codelistReactableServer("result", query_result, function(x) x$result)

      codelistTreeServer(
        "result_tree",
        query_result = query_result,
        extract_fn = function(x) x$result,
        reset_signal = reactive(input$run)
      )

      output$result_query <- renderUI({
        req(query_result_type() == "query_result")

        # render ACE (read only)
        shinyAce::aceEditor(
          ns("editor"),
          mode = "r",
          value = query_result()$query_code |>
            purrr::map(
              \(code_expr) wrap_query_expr_with_code_type(
                code_expr,
                query_result()$code_type,
                saved_queries()$dag$nodes
              )
            ) |>
            purrr::map(deparse1) |>
            paste(sep = "", collapse = "\n"),
          height = "150px",
          autoScrollEditorIntoView = TRUE,
          readOnly = TRUE,
          wordWrap = TRUE
        )
      })

      # Download query Quarto report -------------------------------------------

      observeEvent(input$download_report,
        ignoreInit = TRUE,
        handlerExpr = {
          showModal(
            modalDialog(
              textInput(ns("qmd_title"), label = "Title", placeholder = "My codelist"),
              textInput(ns("qmd_author"), label = "Author", placeholder = "My name"),
              dateInput(ns("qmd_date"), label = "Date"),
              textAreaInput(ns("qmd_description"), label = "Description", placeholder = "A brief description"),
              textInput(ns("qmd_filename"), label = "File name", placeholder = "codelist"),
              title = "Download codelist report",
              footer = tagList(
                actionButton(ns("cancel_download_qmd"), "Cancel"),
                downloadButton(ns("download_qmd"), label = "Download")
              )
            )
          )
      })

      output$download_qmd <- downloadHandler(
        filename = function() {
          paste0(input$qmd_date, "_", input$qmd_filename, ".html")
        },
        content = function(file) {

          # See also https://stackoverflow.com/a/74948301

          # Notifcation
          id <-
            showNotification(
              "Rendering report...",
              type = "message",
              duration = NULL,
              closeButton = FALSE
            )
          on.exit(removeNotification(id), add = TRUE)

          # reset user input and remove modal dialog
          removeModal()

          # setup
          TEMPFILE_NAME <- tempfile()
          TEMPFILE_QMD <- paste0(TEMPFILE_NAME, ".qmd")
          TEMPFILE_HTML <- paste0(TEMPFILE_NAME, ".html")

          # code to include indicator columns in final codelist table
          interim_strategies <- query_result()$query_code |>
            purrr::map_chr(\(x) as.character(x)[2])

          interim_strategies <- interim_strategies[1:(length(interim_strategies) - 1)]

          append_indicators_case_when_statements <- interim_strategies |>
            purrr::map_chr(\(x) stringr::str_glue("  mutate({x} = case_when(code %in% {x}$code ~ '1', TRUE ~ '.'))"))

          APPEND_INDICATORS_CODE <- c("RESULT <- RESULT",
                                      append_indicators_case_when_statements) |>
            paste(sep = "", collapse = " |>\n")

          APPEND_INACTIVE_SCT_CODE <- ""

          if (input$code_type == "sct") {
            APPEND_INACTIVE_SCT_CODE <- "
inactive_codes <- CODES(
  RESULT$code,
  code_type = 'sct',
  preferred_description_only = TRUE,
  standardise_output = TRUE,
  unrecognised_codes = 'warning',
  col_filters = list(sct_description = list(active_concept = '0', active_description = '1'))
  ) %>%
  .$code %>%
  suppressWarnings()

RESULT <- RESULT %>%
  dplyr::mutate('...inactive_sct' = dplyr::case_when(.data[['code']] %in% inactive_codes ~ '1',
                TRUE ~ '.'))
"
          }

          # write qmd report
          report_template <-
            '---
title: "{TITLE}"
subtitle: "{SUBTITLE_CODE_TYPE}"
author: {AUTHOR}
date: {DATE}
date-format: medium
format:
  html:
    code-fold: true
    code-link: true
    code-overflow: wrap
    embed-resources: true
    code-tools:
      source: true
      toggle: true
      caption: Code
---

```{{r}}
#| message: false
library(codemapper)
library(dplyr)
library(htmltools)

options({QUERY_OPTIONS})
```
# Query

{DESCRIPTION}

::: {{.column-screen-inset}}
```{{r}}
#| code-fold: false
#| output: false
{QUERY_CODE}
```
:::

# Codelist

::: {{.column-screen-inset}}
```{{r}}
#| output: false
# append indicator columns
{APPEND_INDICATORS_CODE}
{APPEND_INACTIVE_SCT_CODE}
```

```{{r}}
# display interactive table with button to download as a csv file
htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "{ONCLICK}"
    ),

    reactable::reactable(
      RESULT,
      filterable = TRUE,
      searchable = TRUE,
      resizable = TRUE,
      paginationType = "jump",
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100, 200),
      elementId = "codelist-download"
    )
  )
)
```
:::

::: {{.callout-note appearance="simple" collapse=true}}

## Session info

Quarto version: `r quarto::quarto_version()`

```{{r}}
sessioninfo::session_info()
```

:::

'

      report_template |>
        stringr::str_glue(
          TITLE = stringr::str_remove_all(input$qmd_title, "`"),
          SUBTITLE_CODE_TYPE = get_code_type_labels(query_result()$code_type,
                                                    direction = "id_label")[[1]],
          AUTHOR = stringr::str_remove_all(input$qmd_author, "`"),
          DATE = input$qmd_date,
          QUERY_OPTIONS = rlang::expr_text(query_options()),
          DESCRIPTION = stringr::str_remove_all(input$qmd_description, "`"),
          QUERY_CODE = query_result()$query_code |>
            purrr::map(
              \(code_expr) wrap_query_expr_with_code_type(
                code_expr,
                query_result()$code_type,
                saved_queries()$dag$nodes
              )
            ) |>
            paste(sep = "", collapse = "\n"),
          APPEND_INDICATORS_CODE = APPEND_INDICATORS_CODE,
          ONCLICK = stringr::str_glue("Reactable.downloadDataCSV('codelist-download', '{input$qmd_filename}.csv')")
        ) |>
        print() |>
        writeLines(con = TEMPFILE_QMD)

      # render report
      quarto::quarto_render(TEMPFILE_QMD, execute_dir = ".")

      file.copy(TEMPFILE_HTML, file)
        }
      )

    observeEvent(input$cancel_download_qmd, {
      removeModal()
    })


    # Code type ---------------------------------------------------------------

    observe({
      shinyjs::toggleState(ns("code_type"),
                           condition = !is.null(input$qb),
                           asis = TRUE)
    })

    # Saved queries --------------------------------------------------------------

    dag_igraph <- eventReactive(saved_queries(), {
      if (nrow(saved_queries()$dag$nodes) > 0 &
          nrow(saved_queries()$dag$edges) > 0) {
        # ascertain dependencies using igraph package
        dag <-
          igraph::graph_from_data_frame(
            d = saved_queries()$dag$edges,
            directed = TRUE,
            vertices = saved_queries()$dag$nodes
          )
      } else {
        dag <- NULL
      }

      dag
    })

    # update query builder in response to any of (i) code type change (ii)
    # updated list of saved queries (iii) finish updating a saved query (either
    # cancel or save the update)
    observeEvent(
      list(
        input$code_type,
        saved_queries(),
        input$btn_save_updated_query,
        input$cancel_query_update
      ),
      {
        # update qbr saved query filter
        new_filters <- update_qbr_filters(input_code_type = input$code_type,
                                          available_maps = available_maps,
                                          saved_query_objects = as.list(saved_queries()$objects))

        jqbr::updateQueryBuilder(
          inputId = "qb",
          setFilters = new_filters,
          setRules = update_qb_operator_code_type(input$qb, input$code_type),
          destroy = TRUE
        )
      }
    )

    ### Save current query ------------------------------------------------------

    # show 'save query' button only if a query name has been entered
    observeEvent(input$save_query_name, {
      duplicate_query_name <-
        (input$save_query_name %in% saved_queries()$dag$nodes$id) &
        (input$save_query_name != "")

      shinyFeedback::feedbackWarning("save_query_name",
                                     duplicate_query_name,
                                     "Query with this name already exists")
      req(!duplicate_query_name)

      tab <-
        ifelse(
          shiny::isTruthy(input$save_query_name) & !duplicate_query_name,
          "tab_save_query_button_show",
          "tab_save_query_button_hide"
        )

      updateTabsetPanel(inputId = "tabs_show_save_query_button", selected = tab)
    })

    # click save query button
    observeEvent(input$save_query,
                 label = "add_to_saved_queries",
                 handlerExpr = {
                   update_saved_queries(
                     query = input$save_query_name,
                     query_result = query_result,
                     saved_queries = saved_queries,
                     code_type = query_result()$code_type,
                     query_options = query_options
                   )

                   # reset query name input
                   updateTextInput(inputId = "save_query_name",
                                   value = "")
                 })


    ### Show saved queries ------------------------------------------------------

    observe(label = "update_saved_queries_selectize",
            x = {
              # update saved queries check boxes
              updateSelectizeInput(inputId = "saved_queries_selectize",
                                   choices = saved_queries()$objects)
            })

    # update qbr load_query input tab and options
    observeEvent(
      eventExpr = saved_queries(),
      label = "update_qbr_load_query_input",
      ignoreInit = TRUE,
      handlerExpr = {
        tab <- ifelse(
          shiny::isTruthy(names(saved_queries()$objects)),
          "tab_load_saved_query_show",
          "tab_load_saved_query_hide"
        )

        updateTabsetPanel(inputId = "tabs_load_saved_query", selected = tab)
      }
    )

    ### Load/update saved queries -----------------------------------------------

    observeEvent(input$btn_qb_load_saved_query,
                 ignoreInit = TRUE,
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       selectInput(
                         ns("select_qb_load_saved_query"),
                         label = "Select saved query",
                         choices = saved_queries()$objects,
                         multiple = FALSE,
                       ),
                       title = "Load or modify existing query?",
                       footer = tagList(
                         actionButton(ns("btn_cancel_query_load_update"), "Cancel"),
                         actionButton(ns("btn_load_query"), "Load", class = "btn-lg btn-success"),
                         actionButton(ns("btn_update_query"), "Update", class = "btn btn-danger")
                       )
                     )
                   )
                 })

    observeEvent(input$btn_load_query, {
      selected_saved_query <- input$select_qb_load_saved_query

      updateRadioButtons(
        inputId = "code_type",
        selected = saved_queries()$dag$nodes %>%
          dplyr::filter(.data[["id"]] == !!selected_saved_query) %>%
          dplyr::pull(.data[["group"]])
      )

      jqbr::updateQueryBuilder(
        inputId = "qb",
        setRules = get(selected_saved_query,
                       envir = saved_queries()$results_meta)$qb
      )
      removeModal()
      showNotification(paste0("Loaded ", selected_saved_query))
    })

    observeEvent(input$btn_cancel_query_load_update, {
      removeModal()
    })

    output$currently_updating_query <- renderText({
      req(input$select_qb_load_saved_query)
      paste0(
        "Updating saved query: ",
        input$select_qb_load_saved_query,
        " (",
        input$code_type,
        ")"
      )
    })

    observeEvent(input$btn_update_query, {
      selected_saved_query <- input$select_qb_load_saved_query

      available_saved_queries <- character()

      if (!is.null(saved_queries()$objects)) {
        available_saved_queries <- saved_queries()$objects |>
          purrr::list_flatten() |>
          as.character()

        available_saved_queries <- available_saved_queries[!available_saved_queries == selected_saved_query]
      }

      # ensure saved query filter only shows upstream dependencies for selected
      # query
      if (!is.null(dag_igraph())) {
        downstream_dependencies <- find_node_dependencies(graph = dag_igraph(),
                                               node = selected_saved_query,
                                               mode = "out")

        available_saved_queries <- available_saved_queries[!available_saved_queries %in% downstream_dependencies]
      }

      # finally
      code_type <- saved_queries()$dag$nodes %>%
        dplyr::filter(.data[["id"]] == !!selected_saved_query) %>%
        dplyr::pull(.data[["group"]])

      updateRadioButtons(
        inputId = "code_type",
        selected = code_type
      )

      updateTabsetPanel(inputId = "tabs_select_code_type",
                        selected = "tab_select_code_type_hide")

      saved_query_objects <- as.list(saved_queries()$objects)

      if (!rlang::is_empty(saved_query_objects)) {
        saved_query_objects <- saved_queries()$objects |>
          purrr::map(\(code_type) purrr::keep(code_type, .p = \(x) x %in% downstream_dependencies))
      }

      new_filters <- update_qbr_filters(input_code_type = code_type,
                                        available_maps = available_maps,
                                        saved_query_objects = saved_query_objects)


      jqbr::updateQueryBuilder(
        inputId = "qb",
        setFilters = new_filters,
        setRules = get(
          input$select_qb_load_saved_query,
          envir = saved_queries()$results_meta
        )$qb
      )

      updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

      updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_update_query_button_show")

      removeModal()

      showNotification(paste0("Updating ", input$select_qb_load_saved_query))
    })

    observeEvent(input$btn_save_updated_query, {
      # update saved queries
      update_saved_queries(
        query = input$select_qb_load_saved_query,
        query_result = query_result,
        saved_queries = saved_queries,
        code_type = input$code_type,
        query_options = query_options
      )

      updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

      updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_save_query_input_show")

      updateTabsetPanel(inputId = "tabs_select_code_type",
                        selected = "tab_select_code_type_show")
    })

    observeEvent(input$cancel_query_update,
                 {
                   updateTabsetPanel(inputId = "query_result_tabs", selected = "empty_query")

                   updateTabsetPanel(inputId = "tabs_save_or_update_query", selected = "tab_save_query_input_show")

                   updateTabsetPanel(inputId = "tabs_select_code_type",
                                     selected = "tab_select_code_type_show")
                 })

    ### Remove saved queries ----------------------------------------------------

    observe(label = "show_hide_tabs_remove_saved_queries",
            x = {
              tab <- ifelse(
                shiny::isTruthy(input$saved_queries_selectize),
                "tab_remove_saved_queries_show",
                "tab_remove_saved_queries_hide"
              )
              updateTabsetPanel(inputId = "tabs_remove_saved_queries", selected = tab)
            })

    updated_dag <- eventReactive(input$remove_saved_queries,
                                 label = "determine_which_saved_queries_to_remove",
                                 valueExpr = {
                                   # determine dependencies
                                   if (!is.null(dag_igraph())) {
                                     # ascertain dependencies using igraph package
                                     dependencies <-
                                       input$saved_queries_selectize %>%
                                       purrr::set_names() %>%
                                       purrr::map(~ find_node_dependencies(graph = dag_igraph(),
                                                                           node = .x)) %>%
                                       purrr::compact()

                                     nodes_to_remove <-
                                       c(
                                         input$saved_queries_selectize,
                                         purrr::reduce(dependencies, c, .init = NULL)
                                       ) %>%
                                       unique()

                                     # finally
                                     new_dependencies <- list(
                                       nodes = saved_queries()$dag$nodes %>%
                                         dplyr::filter(!.data[["id"]] %in% nodes_to_remove),
                                       edges = saved_queries()$dag$edges %>%
                                         dplyr::filter(
                                           !.data[["to"]] %in% nodes_to_remove,
                                           !.data[["from"]] %in% nodes_to_remove
                                         )
                                     )
                                   } else {
                                     nodes_to_remove <- input$saved_queries_selectize
                                     new_dependencies <-
                                       list(
                                         nodes = saved_queries()$dag$nodes %>%
                                           dplyr::filter(!.data[["id"]] %in% nodes_to_remove),
                                         edges = data.frame()
                                       )
                                     dependencies <- list()
                                   }

                                   # return updated DAG
                                   list(
                                     new_dependencies = new_dependencies,
                                     nodes_to_remove = nodes_to_remove,
                                     dependencies = dependencies
                                   )
                                 })

    observeEvent(updated_dag(), label = "remove_from_saved_queries",
                 handlerExpr = {
                   # confirm dialog box if removing selected queries will also
                   # impact other queries
                   if (!rlang::is_empty(updated_dag()$dependencies)) {
                     modal_confirm <- modalDialog(
                       paste0(
                         "The following saved queries will also be deleted: ",
                         paste(
                           subset(
                             updated_dag()$nodes_to_remove,!updated_dag()$nodes_to_remove %in% input$saved_queries_selectize
                           ),
                           sep = "",
                           collapse = ", "
                         ),
                         "."
                       ),
                       title = "Dependencies detected for selected queries",
                       footer = tagList(
                         actionButton(ns("cancel"), "Cancel"),
                         actionButton(ns("ok"), "Delete", class = "btn btn-danger")
                       )
                     )

                     showModal(modal_confirm)
                   } else {
                     # otherwise, simply remove selected queries
                     remove_saved_queries(updated_dag = updated_dag,
                                          saved_queries = saved_queries)
                   }
                 })

    observeEvent(input$ok, {
      remove_saved_queries(updated_dag = updated_dag,
                           saved_queries = saved_queries)
      showNotification("Saved queries deleted")
      removeModal()
    })

    observeEvent(input$cancel, {
      removeModal()
    })


    ### Render DAG -----------------------------------------------------------

    output$dag_visnetwork <-
      visNetwork::renderVisNetwork({
        req(nrow(saved_queries()$dag$nodes) > 0)

        visNetwork::visNetwork(
          nodes = saved_queries()$dag$nodes %>%
            # relabel code types
            dplyr::left_join(CODE_TYPE_TO_LKP_TABLE_MAP %>%
                               dplyr::select(tidyselect::all_of(c("code", "code_label"))),
                             by = c("group" = "code")) %>%
            dplyr::select(-tidyselect::all_of("group")) %>%
            dplyr::rename("group" = "code_label") %>%
            # show n codes for each saved query
            dplyr::mutate("n" = paste0("\n(", prettyNum(.data[["n"]], big.mark = ","), ")")) %>%
            tidyr::unite(col = "label",
                         tidyselect::all_of(c("label", "n")),
                         sep = " ",
                         remove = TRUE),
          edges = saved_queries()$dag$edges,
          width = "100%"
        ) |>
          visNetwork::visEdges(arrows = "to") |>
          visNetwork::visNodes(shadow = list(enabled = TRUE, size = 10)) |>
          visNetwork::visHierarchicalLayout(direction = "LR",
                                            sortMethod = "directed") |>
          visNetwork::visInteraction(
            zoomView = TRUE,
            dragView = TRUE,
            dragNodes = TRUE
          ) |>
          visNetwork::visGroups() |>
          visNetwork::visLegend() |>
          visNetwork::visOptions(selectedBy = list(variable = "group"))
      })

    output$dag_nodes <- renderTable(saved_queries()$dag$nodes)
    output$dag_edges <- renderTable(saved_queries()$dag$edges)
    })
  }
