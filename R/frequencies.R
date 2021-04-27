#' Get Stop Frequency
#' 
#' Note that some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates. 
#' 
#' @param gtfs_obj a list of gtfs dataframes as read by [read_gtfs()].
#' @param start_hour (optional) an integer indicating the start hour (default 6)
#' @param end_hour (optional) an integer indicating the end hour (default 22)
#' @param service_ids (optional) a set of service_ids from the calendar dataframe 
#'                    identifying a particular service id. If not provided the service_id 
#'                    with the most departures is used
#' @param by_route default TRUE, if FALSE then calculate headway for any line coming through the stop in the same direction on the same schedule. 
#' @export
#' @return dataframe of stops with the number of departures and the headway 
#'         (departures divided by timespan) as columns.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! quo enquo
#' @importFrom stats median sd
#' @importFrom tidyr spread
#' @examples 
#' data(gtfs_duke)
#' stop_frequency <- get_stop_frequency(gtfs_duke)
#' x <- order(stop_frequency$mean_headway)
#' head(stop_frequency[x,])
get_stop_frequency <- function(gtfs_obj,
                               start_hour = 6,
                               end_hour = 22,
                               service_ids = NULL,
                               by_route = FALSE) {
  n_deps <- direction_id <- NULL
  if(is.null(service_ids)) {
    dep_per_trip = gtfs_obj$stop_times %>% 
      dplyr::group_by(trip_id) %>% dplyr::count(name = "n_deps") %>% 
      dplyr::ungroup()
    dep_per_service_id = left_join(gtfs_obj$trips, dep_per_trip, "trip_id") %>% 
      dplyr::group_by(service_id) %>% 
      dplyr::summarise(n_deps = sum(n_deps)) %>% 
      dplyr::arrange(dplyr::desc(n_deps))
    service_ids = dep_per_service_id$service_id[1]  
  }
  trips = gtfs_obj$trips %>% filter(service_id %in% service_ids)
  
  # TODO change times to hms or strings
  stop_times = gtfs_obj$stop_times %>%
    filter(trip_id %in% trips$trip_id) %>% 
    filter(departure_time >= start_hour*3600 & arrival_time <= end_hour*3600) %>% 
    left_join(trips[c("trip_id", "route_id", "direction_id", "service_id")], "trip_id") 
  
  # find number of departure per stop_id, route_id, service_id
  if(by_route) {
  freq = stop_times %>% 
    dplyr::group_by(stop_id, route_id, direction_id, service_id) %>% 
    dplyr::count(name = "n_departures") %>% dplyr::ungroup()
  } else {
    freq = stop_times %>% 
      dplyr::group_by(stop_id, service_id) %>% 
      dplyr::count(name = "n_departures") %>% dplyr::ungroup()
  }
  # calculate average headway
  duration = (end_hour-start_hour)*3600
  freq$mean_headway <- round(duration / freq$n_departures)

  freq
}
