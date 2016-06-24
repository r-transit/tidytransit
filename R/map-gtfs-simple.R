# MAPPING SIMPLE -----------------------------------------------------------------
# "SIMPLE" here refers to mapping the information of a single stop or route.

# Functions to quickly and easily map
#	- a single stop
# - stops for a route
# - shape of a route

#' map a single stop
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param stop_id Character. A single ID for a stop of interest.
#'
#' @return Leaflet map object with point plotted at stop lat/long value
#' @export

map_gtfs_stop <- function(gtfs_obj, stop_id) {

	stopifnot(
		class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		length(stop_id) == 1
		)

	id <- stop_id

	df <- gtfs_obj$stops_df %>%
		dplyr::slice(which(stop_id %in% id))

	if(dim(df)[1] == 0) {
		s <- "Stop '%s' was not found." %>% sprintf(id)
		stop(s)
	}

	stop <- df %>%
		dplyr::select(stop_name, stop_lat, stop_lon) %>%
		dplyr::rename(name = stop_name, lat = stop_lat, lng = stop_lon)

	m <- stop %>%
		leaflet::leaflet() %>%
		leaflet::addTiles() %>%
		leaflet::addMarkers(popup = stop$name, lat = stop$lat, lng = stop$lng)
	m

}

#' map all stops for a route
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_id Character. A single ID for a route of interest.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_route_stops <- function(gtfs_obj, route_id) {

	stopifnot(
		class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$routes_df),
		length(route_id) == 1
		)

	id <- route_id
	rm('route_id')

	# extract vector of all trips matching route_id
	trip_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id)) %>%
		dplyr::select(trip_id) %>%
		magrittr::extract2(1)

	if(length(trip_ids) == 0) {
		s <- "No trips for Route ID '%s' were found." %>% sprintf(id)
		stop(s)
	}

	# extract all possible stops across all trips for given route
	possible_stops <- gtfs_obj$stop_times_df %>%
		dplyr::slice(which(trip_id %in% trip_ids)) %>%
		dplyr::select(stop_id) %>%
		unique %>%
		magrittr::extract2(1)

	stops <- gtfs_obj$stops_df %>%
		dplyr::slice(which(stop_id %in% possible_stops)) %>%
		dplyr::select(stop_name, stop_lat, stop_lon) %>%
		dplyr::rename(name = stop_name, lat = stop_lat, lng = stop_lon)

	m <- stops %>%
		leaflet::leaflet() %>%
		leaflet::addTiles() %>%
		leaflet::addCircleMarkers(
			popup = stops$name,
			color = 'red',
			radius = 4,
	    stroke = TRUE,
	    fillOpacity = 0.7,
			lat = stops$lat,
			lng = stops$lng) %>%
		leaflet::addLegend(colors = 'red', labels = "Stops")
	m

}
