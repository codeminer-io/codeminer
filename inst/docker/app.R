# By default, use db containing only publicly available resources
ALL_LKPS_MAPS_DB <- "./all_lkps_maps.db"

# User can supply their own db by mounting a folder called `all_lkps_maps`
# containing `all_lkps_maps.db`, to the container root directory
USER_SUPPLIED_DB_PATH <- "/all_lkps_maps/all_lkps_maps.db"
if (file.exists(USER_SUPPLIED_DB_PATH)) {
  ALL_LKPS_MAPS_DB <- USER_SUPPLIED_DB_PATH

  # TODO - running app with environmental variables. See
  # https://www.r-bloggers.com/2019/06/shinyapp-runapp-shinyappdir-and-a-fourth-option/
  Sys.setenv(ALL_LKPS_MAPS_DB = ALL_LKPS_MAPS_DB)
}

# Run app
codeminer::RunCodelistBuilder(all_lkps_maps = ALL_LKPS_MAPS_DB)
