#' Get a set of stops for a given set of service ids and route ids
#' 
#' @param gtfs_obj as read by read_gtfs()
#' @param service_ids the service for which to get stops 
#' @param route_ids the route_ids for which to get stops 
#' @return stops for a given service
#' @export
#' @examples \donttest{
#' library(dplyr)
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path)
#' select_service_id <- filter(nyc$calendar, monday==1) %>% pull(service_id)
#' select_route_id <- sample_n(nyc$routes, 1) %>% pull(route_id)
#' filtered_stops_df <- filter_stops(nyc, select_service_id, select_route_id)
#' }
filter_stops <- function(gtfs_obj, service_ids, route_ids) {
  some_trips <- dplyr::filter(gtfs_obj$trips, 
                              .data$service_id %in% service_ids &
                                .data$route_id %in% route_ids)
  
  some_stop_times <- dplyr::filter(gtfs_obj$stop_times,
                                   .data$trip_id %in% some_trips$trip_id) 
  
  some_stops <- dplyr::filter(gtfs_obj$stops,
                              .data$stop_id %in% some_stop_times$stop_id)
  
  return(some_stops)
}
