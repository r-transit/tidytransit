#' extract all possible stops across all trips for given route
#' @noRd
get_possible_stops <- function(gtfs_obj, trip_ids) {
  gtfs_obj$stop_times_df %>%
    dplyr::slice(which(trip_id %in% trip_ids)) %>%
    dplyr::select(stop_id) %>%
    unique %>%
    magrittr::extract2(1)

}
