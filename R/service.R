#' Get the most frequent service for a set of trips. 
#' 
#' @param trips gtfs dataframe with a service_trips count column grouped by the service_id, route, etc of interest
#' @return trips gtfs data frame within groups that are in the most frequent service window
#' @keywords internal
#' @importFrom dplyr %>%
#' @keywords internal
most_frequent_service <- function(trips) {
  trips %>%
    dplyr::top_n(1, .data$service_trips)
}

#' Filter a gtfs calendar dataframe to service ids for specific days of the week.
#' 
#' @param gtfs_object object made by join_all_gtfs_tables
#' @param dow default to "weekday" (1,1,1,1,1,0,0)
#' @return service ids that match the schedule specified
#' @keywords internal
service_by_dow <- function(calendar_df,
                           dow=c(1,1,1,1,1,0,0)){
  calendar_df <- subset(calendar_df, 
                        calendar_df$monday == dow[1] & 
                        calendar_df$tuesday == dow[2] & 
                        calendar_df$wednesday == dow[3] & 
                        calendar_df$thursday == dow[4] & 
                        calendar_df$friday == dow[5] &
                        calendar_df$saturday == dow[6] &
                        calendar_df$sunday == dow[7])
  return(calendar_df$service_id)
}

#' Summarise the number of trips per service
#' 
#' @param gtfsr object
#' @return count of service by id
#' @export
#' @keywords internal
count_service_trips <- function(trips) {
  trips %>%
    dplyr::group_by(.data$service_id) %>% 
      dplyr::mutate(service_trips = n()) %>%
        tibble::as_tibble()
}

#' Get a set of stops for a given set of service ids and route ids
#' 
#' @param gtfs_obj as read by read_gtfs()
#' @param service_ids the service for which to get stops 
#' @param route_ids the route_ids for which to get stops 
#' @return stops for a given service
#' @export
#' @examples \donttest{
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path,local=TRUE)
#' select_service_id <- filter(nyc$calendar_df,monday==1) %>% pull(service_id)
#' select_route_id <- sample_n(nyc$routes_df,1) %>% pull(route_id)
#' filtered_stops_df <- filter_stops(nyc,select_service_id,select_route_id)
#' }
filter_stops <- function(gtfs_obj, select_service_ids, select_route_ids) {
  some_trips <- gtfs_obj$trips_df %>%
    dplyr::filter(.data$service_id %in% select_service_ids &
                    .data$route_id %in% select_route_ids)
  
  some_stop_times <- gtfs_obj$stop_times_df %>% 
    dplyr::filter(.data$trip_id %in% some_trips$trip_id) 
  
  some_stops <- gtfs_obj$stops_df %>%
    dplyr::filter(.data$stop_id %in% some_stop_times$stop_id)
  
  return(some_stops)
}