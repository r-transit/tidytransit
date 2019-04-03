#' Add trip pattern data frame to the gtfs object
#' 
#' @param gtfs_obj gtfs feed
#' @param id_prefix all ids start with this string
#' @param hash_length length the hash should be cut to with substr(). Use -1 if the full hash should be used
#' @param hash_algo hashing algorithm used by digest
#' @return gtfs_obj
#' @importFrom dplyr group_by summarise ungroup left_join
#' @importFrom digest digest
set_trippattern <- function(gtfs_obj, id_prefix = "t_", hash_length = 7, hash_algo = "md5") {
  get_trippattern_id <- function(stop_ids, arrival_times, departure_times) {
    h <- c(stop_ids, arrival_times, departure_times)
    hash <- digest::digest(h, hash_algo)
    id <- paste0(id_prefix, substr(hash, 0, hash_length))
    return(id)
  }
  
  if(hash_length < 1) {
    get_trippattern_id <- function(stop_ids, arrival_times, departure_times) {
      h <- c(stop_ids, arrival_times, departure_times)
      hash <- digest::digest(h, hash_algo)
      id <- paste0(id_prefix, hash)
      return(id)
    }
  }
  
  # TODO use frequencies instead of stop_times
  # TODO add test coverage
  
  # find servicepattern_ids for all services
  trip_pattern <- gtfs_obj$stop_times %>%
    left_join(select(gtfs_obj$trips, trip_id, route_id), by = "trip_id") %>%
    arrange(trip_id, stop_sequence) %>%
    group_by(trip_id, route_id) %>%
    summarise(
      trippattern_id = get_trippattern_id(stop_id, arrival_time, departure_time)
    ) %>% ungroup() %>% select(-route_id)
  
  gtfs_obj$.$trip_pattern <- trip_pattern
  
  return(gtfs_obj)
}