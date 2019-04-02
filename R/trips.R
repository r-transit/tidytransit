#' Add trip pattern data frame to the gtfs object
#' 
#' @param gtfs_obj gtfs feed
#' @return gtfs_obj
#' @importFrom dplyr group_by summarise ungroup left_join
#' @importFrom digest digest
set_trippattern <- function(gtfs_obj) {
  get_trippattern_id <- function(sequence, stop_ids, arrival_times, departure_times) {
    x <- order(sequence)
    h <- c(stop_ids[x], arrival_times[x], departure_times[x])
    id <- digest::digest(h, "md5")
    id <- paste0("t_", substr(id, 0, 8))
    return(id)
  }
  
  # TODO use frequencies instead of stop_times
  
  # find servicepattern_ids for all services
  gtfs_obj$trip_pattern <- gtfs_obj$stop_times %>% 
    left_join(select(gtfs_obj$trips, trip_id, route_id), by = "trip_id") %>% 
    group_by(trip_id, route_id) %>%
    summarise(
      trippattern_id = get_trippattern_id(stop_sequence, stop_id, arrival_time, departure_time)
    ) %>% ungroup() %>% select(-route_id)

  return(gtfs_obj)
}