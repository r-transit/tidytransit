#' Get Stop Frequency
#' 
#' Calculate the number of departures and mean headways for all stops within a
#' given timespan and for given service_ids. 
#' 
#' @note Some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates.
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param start_time analysis start time, can be given as "HH:MM:SS", 
#'                   hms object or numeric value in seconds.
#' @param end_time analysis perdiod end time, can be given as "HH:MM:SS", 
#'                 hms object or numeric value in seconds.
#' @param service_ids A set of service_ids from the calendar dataframe 
#'                    identifying a particular service id. If not provided, the service_id 
#'                    with the most departures is used.
#' @param by_route Default TRUE, if FALSE then calculate headway for any line coming 
#'                 through the stop in the same direction on the same schedule. 
#' @return dataframe of stops with the number of departures and the headway 
#'         (departures divided by timespan) in seconds as columns
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data !! quo enquo
#' @importFrom stats median sd
#' @export
#' @examples 
#' data(gtfs_duke)
#' stop_frequency <- get_stop_frequency(gtfs_duke)
#' x <- order(stop_frequency$mean_headway)
#' head(stop_frequency[x,])
get_stop_frequency <- function(gtfs_obj,
                               start_time = "06:00:00",
                               end_time = "22:00:00",
                               service_ids = NULL,
                               by_route = TRUE) {
  n_deps <- direction_id <- NULL
  
  if(is.character(start_time)) start_time <- hhmmss_to_seconds(start_time)
  if(is.character(end_time)) end_time <- hhmmss_to_seconds(end_time)
  
  # get service id with most departures
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
  
  # filter stop_times to service_ids and start/end_time
  trips = gtfs_obj$trips %>% filter(service_id %in% service_ids)
  
  stop_times = gtfs_obj$stop_times %>%
    filter(trip_id %in% trips$trip_id) %>% 
    filter(departure_time >= start_time & arrival_time <= end_time) %>% 
    left_join(trips[c("trip_id", "route_id", "direction_id", "service_id")], "trip_id") 
  
  # find number of departure per stop_id (route_id, direction_id, service_id)
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
  duration = as.numeric(end_time-start_time)
  freq$mean_headway <- round(duration / freq$n_departures)
  
  freq
}

#' Get Route Frequency
#' 
#' Calculate the number of departures and mean headways for routes within a given timespan
#' and for given service_ids.
#' 
#' @note Some GTFS feeds contain a frequency data frame already. 
#' Consider using this instead, as it will be more accurate than what 
#' tidytransit calculates. 
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param start_time analysis start time, can be given as "HH:MM:SS", 
#'                   hms object or numeric value in seconds.
#' @param end_time analysis perdiod end time, can be given as "HH:MM:SS", 
#'                 hms object or numeric value in seconds.
#' @param service_ids A set of service_ids from the calendar dataframe 
#'                    identifying a particular service id. If not provided, the service_id 
#'                    with the most departures is used.
#' @return a dataframe of routes with variables or headway/frequency in seconds for a route 
#'         within a given time frame
#' @export
#' @examples 
#' data(gtfs_duke)
#' routes_frequency <- get_route_frequency(gtfs_duke)
#' x <- order(routes_frequency$median_headways)
#' head(routes_frequency[x,])
get_route_frequency <- function(gtfs_obj,
                                start_time = "06:00:00",
                                end_time = "22:00:00",
                                service_ids = NULL) {
  total_departures <- median_headways <- mean_headways <- NULL
  n_departures <- mean_headway <- st_dev_headways <- stop_count <- NULL
  if(feed_contains(gtfs_obj, "frequencies") && nrow(gtfs_obj$frequencies) > 0) {  
    message("A pre-calculated frequencies dataframe exists for this feed already, consider using that.") 
  } 
  departures_per_stop = get_stop_frequency(gtfs_obj, start_time, end_time, 
                                           service_ids, by_route = TRUE)

  if(nrow(departures_per_stop) > 0) {
    routes_frequency = departures_per_stop %>% 
      group_by(route_id) %>%
      summarise(total_departures = sum(n_departures),
                median_headways = round(median(mean_headway)),
                mean_headways = round(mean(mean_headway)),
                st_dev_headways = round(sd(mean_headway), 2),
                stop_count = dplyr::n())
  } else {
    stop("Failed to calculate frequency, no departures found")
  }
  
  return(routes_frequency)
}
