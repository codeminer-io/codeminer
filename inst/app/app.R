library(codeminer)
library(gitcreds)
options(shiny.maxRequestSize = 20 * 1024^2)

build_all_lkps_maps() |>
  all_lkps_maps_to_db()

RunCodelistBuilder(all_lkps_maps = "all_lkps_maps.db")
