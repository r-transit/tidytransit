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

#' Filter all trips passing through an area
#' 
#' Only keeps trips that pass stops within the given through_area. 
#' Only stops, routes, services, shapes, frequencies and transfers belonging to 
#' one of those trips are kept.
#' 
#' @param gtfs_obj tidygtfs object
#' @param area all trips passing through this area are kept. Either a bounding box 
#'             (numeric vector with xmin, ymin, xmax, ymax) or a sf object.
#' @export
filter_trips_through_area <- function(gtfs_obj, area) {
  if(inherits(gtfs_obj$stops, "sf") & inherits(area, "sf")) {
    sf::st_agr(gtfs_obj$stops) <- "constant"
    stops_area = sf::st_intersection(gtfs_obj$stops, sf::st_geometry(area))
    stop_ids = stops_area$stop_id
  } else {
    if(inherits(area, "sf")) {
      area <- sf::st_bbox(area)
    } else {
      if(length(area) != 4 | !is.numeric(area)) {
        stop("bbox_area must be a numeric vector of length four, with xmin, ymin, xmax and ymax values")
      }
    }
    stop_ids = gtfs_obj$stops %>% filter_bbox(area)
    stop_ids <- unique(stop_ids$stop_id)
  }
  gtfs_obj$stops$stop_in_area <- gtfs_obj$stops$stop_id %in% stop_ids
  
  filter_trips(gtfs_obj, stop_ids)
}

#' Filter all trips that pass the given stops
#' 
#' Only keeps trips that pass stops with the given stop_ids or stop_names.
#' Only stops, routes, services, shapes, frequencies and transfers belonging to 
#' one of those trips are kept.
#' You can either provide stop_ids or stop_names (which are then converted to stop_ids)
#' 
#' @param gtfs_obj tidygtfs object
#' @param stop_ids vector with stop_ids
#' @param stop_names vector with stop_names
#' @export
filter_trips = function(gtfs_obj, stop_ids = NULL, stop_names = NULL) {
  if(!is.null(stop_names)) {
    if(!is.null(stop_ids)) stop("Please provide either stop_ids or stop_names")
    stop_ids = gtfs_obj$stops$stop_id[which(gtfs_obj$stops$stop_name %in% stop_names)]
  }
  if(is.null(stop_ids) && is.null(stop_names)) stop("Please provide either stop_ids or stop_names")
  
  if(!any(stop_ids %in% gtfs_obj$stops$stop_id)) {
    stop("stop_ids found in stops table: ", paste(stop_ids, collapse = ", "))
  }
    
  trip_ids = gtfs_obj$stop_times[which(gtfs_obj$stop_times$stop_id %in% stop_ids),]
  trip_ids <- unique(trip_ids$trip_id)
  route_ids = gtfs_obj$trips[which(gtfs_obj$trips$trip_id %in% trip_ids),]
  route_ids <- unique(route_ids$route_id)
  
  # first batch via trip_ids
  gtfs_obj$stop_times <- gtfs_obj$stop_times[which(gtfs_obj$stop_times$trip_id %in% trip_ids),]
  gtfs_obj$routes <- gtfs_obj$routes[which(gtfs_obj$routes$route_id %in% route_ids),]
  gtfs_obj$trips <- gtfs_obj$trips[which(gtfs_obj$trips$trip_id %in% trip_ids),]
  
  # other
  trip_stop_ids = gtfs_obj$stop_times$stop_id
  service_ids = unique(gtfs_obj$trips$service_id)
  gtfs_obj$stops <- gtfs_obj$stops[which(gtfs_obj$stops$stop_id %in% trip_stop_ids),]
  
  gtfs_obj$.$dates_services <- filter(gtfs_obj$.$dates_services, service_id %in% service_ids)
  if(feed_contains(gtfs_obj, "calendar")) {
    gtfs_obj$calendar <- gtfs_obj$calendar[which(gtfs_obj$calendar$service_id %in% service_ids),]
  }
  if(feed_contains(gtfs_obj, "calendar_dates")) {
    gtfs_obj$calendar_dates <- gtfs_obj$calendar_dates[which(gtfs_obj$calendar_dates$service_id %in% service_ids),]
  }
  if(feed_contains(gtfs_obj, "shapes")) {
    gtfs_obj$shapes <- gtfs_obj$shapes[which(gtfs_obj$shapes$shape_id %in% gtfs_obj$trips$shape_id),]
  }
  if(feed_contains(gtfs_obj, "frequencies")) {
    gtfs_obj$frequencies <- gtfs_obj$frequencies[which(gtfs_obj$frequencies$trip_id %in% trip_ids),]
  }
  if(feed_contains(gtfs_obj, "transfers")) {
    gtfs_obj$transfers <- gtfs_obj$transfers[which(
      gtfs_obj$transfers$from_stop_id %in% trip_stop_ids | 
        gtfs_obj$transfers$to_stop_id %in% trip_stop_ids),]
  }
  
  gtfs_obj
}

filter_bbox <- function(data, bbox, buffer = 0, coord_cols = c("stop_lon", "stop_lat")) {
  stopifnot(is.numeric(buffer))  
  E_col = coord_cols[1]
  N_col = coord_cols[2]
  
  if(is.null(names(bbox))) {
    names(bbox)  <- c("xmin", "ymin", "xmax", "ymax")
  }
  
  if(buffer > 0) {
    bbox <- bbox + c(-1,-1,1,1)*buffer
  }
  
  data[which(data[[E_col]] >= bbox["xmin"] &
               data[[N_col]] >= bbox["ymin"] &
               data[[E_col]] <= bbox["xmax"] &
               data[[N_col]] <= bbox["ymax"]),]
}
