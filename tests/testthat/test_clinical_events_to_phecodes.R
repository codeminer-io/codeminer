# SETUP ---------------------------------------------------------------

all_lkps_maps_dummy <- build_all_lkps_maps_dummy()

ALL_LKPS_MAPS_DUMMY <- all_lkps_maps_to_db(
  all_lkps_maps_dummy,
  db_path = tempfile(fileext = ".db")
)

clinical_events_dummy <- dummy_clinical_events_tidy()
