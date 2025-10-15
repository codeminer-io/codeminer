# PUBLIC ------------------------------------------------------------------

## App ---------------------------------------------------------------------

#' Shiny app for building clinical codelists.
#'
#' Launches a Shiny app. Note, requires `all_lkps_maps` database file.
#'
#' @param all_lkps_maps The path to a Duckdb database containing the lookup and mapping tables.
#'   By default, the path is taken from the environmental variable `ALL_LKPS_MAPS_DB`.
#' @param ... Additional args passed on to \code{\link[shiny]{shinyApp}}
#'
#' @export
#' @import shiny
#'
#' @examples
#' \dontrun{
#' # build dummy all_lkps_maps database
#' all_lkps_maps_dummy <- build_all_lkps_maps_dummy()
#' all_lkps_maps_db <- all_lkps_maps_to_db(
#'   all_lkps_maps = all_lkps_maps_dummy,
#'   db_path = tempfile(fileext = ".db")
#' )
#'
#' # launch app
#' RunCodelistBuilder(all_lkps_maps = all_lkps_maps_db)
#' }
RunCodelistBuilder <- function(all_lkps_maps = Sys.getenv("ALL_LKPS_MAPS_DB"), ..., .envir = parent.frame()) {
  # Requires a data base file, the path to which will be made available as an
  # environmental variable
  if (!file.exists(all_lkps_maps)) {
    stop(sprintf("No database file found at path `%s`", all_lkps_maps))
  }

  # Record db path in envvar, used in downstream queries
  withr::local_envvar(list("ALL_LKPS_MAPS_DB" = all_lkps_maps), .local_envir = .envir)

  con <- check_all_lkps_maps_path(all_lkps_maps)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  # determine which lookup tables are available
  available_code_types <- CODE_TYPE_TO_LKP_TABLE_MAP %>%
    dplyr::filter(.data[["lkp_table"]] %in% DBI::dbListTables(con)) %>%
    dplyr::pull(.data[["code"]])

  # determine which mapping tables are available
  available_maps <- CLINICAL_CODE_MAPPINGS_MAP %>%
    dplyr::filter(.data[["mapping_table"]] %in% DBI::dbListTables(con)) %>%
    dplyr::select(tidyselect::all_of(c("from", "to")))

  available_maps <- dplyr::bind_rows(
    available_maps,
    available_maps %>%
      dplyr::rename(
        "to" = tidyselect::all_of("from"),
        "from" = tidyselect::all_of("to")
      )
  )

  # reactive value to store saved queries - to be shared between modules
  saved_queries <- reactiveVal(list(
    objects = list(),
    results = new.env(),
    results_meta = new.env(),
    dag = list(nodes = data.frame(), edges = data.frame())
  ))

  # reactive value to store saved look ups - to be shared between modules
  saved_lookups <- reactiveVal(list())

  ui <- shinydashboard::dashboardPage(
    skin = "purple",
    shinydashboard::dashboardHeader(title = "CODEMINER"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Build",
          tabName = "builder_tab",
          icon = icon("pen")
        ),
        shinydashboard::menuItem(
          "Search",
          tabName = "lookup_tab",
          icon = icon("magnifying-glass")
        ),
        shinydashboard::menuItem(
          "Compare",
          tabName = "compare_tab",
          icon = icon("code-compare")
        ),
        shinydashboard::menuItem(
          "Bookmark",
          tabName = "bookmark_tab",
          icon = icon("bookmark")
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "builder_tab",
          fluidPage(
            codelistBuilderInput(
              "builder",
              available_code_types = available_code_types,
              available_maps = available_maps
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "compare_tab",
          h2("Compare codelists"),
          fluidPage(
            compareCodelistsInput("compare_codelists")
          )
        ),
        shinydashboard::tabItem(
          tabName = "lookup_tab",
          fluidPage(
            lookupCodesInput(
              "lookup_codes",
              available_code_types = available_code_types
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "bookmark_tab",
          h2("Save/restore codelists"),
          fluidPage(
            downloadButton("download", label = "Save", icon = icon("bookmark")),
            fileInput("upload", "Restore")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    codelistBuilderServer(
      "builder",
      available_maps = available_maps,
      saved_queries = saved_queries
    )

    compareCodelistsServer(
      "compare_codelists",
      saved_queries = saved_queries,
      saved_lookups = saved_lookups
    )

    lookupCodesServer(
      "lookup_codes",
      available_maps = available_maps,
      saved_lookups = saved_lookups
    )

    output$download <- downloadHandler(
      filename = function() {
        "codeminer.rds"
      },
      content = function(file) {
        saveRDS(
          list(saved_queries = saved_queries, saved_lookups = saved_lookups),
          file = file
        )
      }
    )

    observeEvent(input$upload, {
      new_bookmark <- readRDS(input$upload$datapath)
      saved_queries(new_bookmark$saved_queries())
      saved_lookups(new_bookmark$saved_lookups())
    })
  }

  shinyApp(ui, server, ...)
}
