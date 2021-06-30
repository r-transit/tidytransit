#' Returns TRUE if the given gtfs_obj contains the table. Used to check for
#' tidytransit's calculated tables in sublist
#' @param gtfs_obj gtfs object
#' @param table_name name as string of the table to look for
feed_contains <- function(gtfs_obj, table_name) {
  exists(table_name, where = gtfs_obj) ||
    (exists(".", where = gtfs_obj) && exists(table_name, where = gtfs_obj$.))
}

# stops_table = g$stops
# 
# create_stop_name_tbl = function(stop_table) {
#   
#   
#   parent_station_ids = stops_table$parent_station
# 
#   parent_stops = stops_table %>% filter(stop_id %in% parent_station_ids)
#   parent_stops
#   
#   gtfs_obj$stops
# }
