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
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.

#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_route_stops <- function(gtfs_obj, route_ids, stop_opacity = 0.5, route_colors = NULL) {

	stopifnot(
		class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$routes_df),
		!is.null(gtfs_obj$trips_df),
		length(route_ids) > 0
	)

  if(typeof(route_ids) != "character") route_ids %<>% unlist

	plotting_data <- get_routes_sldf(gtfs_obj, route_ids)

	route_ids <- plotting_data$shapes_routes_df$route_id # update route ids

	# update/check variables
	## stop_opacity
	if(any(stop_opacity < 0, stop_opacity > 1)) stop_opacity = 0.5 # error in opacity is fixed

	## route_colors
	if(length(route_colors) != length(route_ids)) {
	  warning("route_colors and route_ids are not the same length. route_colors is ignored and default colors will be used.")
	  route_colors <- NULL
	}
	if(!is.null(route_colors)) {

	  route_colors <- scales::col2hcl(route_colors) %>%
	    sapply(. %>% substr(.,1,7), USE.NAMES = FALSE)

	  plotting_data$routes_colors_df$color <- route_colors

	}

	# create map with shapes
	m <- plotting_data$gtfslines %>%
	  leaflet::leaflet() %>%
	  leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
	  leaflet::addLegend(
	    colors = plotting_data$routes_colors_df$color,
	    labels = paste("Route", plotting_data$routes_colors_df$route_short_name))

  # extract vector of all trips matching route_ids
  trip_ids <- gtfs_obj$trips_df %>%
    dplyr::inner_join(plotting_data$shapes_routes_df,
                      by = c('route_id', 'shape_id', 'service_id')) %>%
    dplyr::select(trip_id) %>%
    dplyr::distinct(trip_id) %>%
    magrittr::extract2(1)

  if(length(trip_ids) == 0) {
    s <- "No trips for Route ID '%s' were found." %>% sprintf(paste(route_ids, collapse = ", "))
    stop(s)
  }

  possible_stops <- get_possible_stops(gtfs_obj, trip_ids)

  stops <- gtfs_obj$stops_df %>%
    dplyr::slice(which(stop_id %in% possible_stops)) %>%
    dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>%
    dplyr::rename(lat = stop_lat, lng = stop_lon)

  # rs = routes, stops
  rs_df <- gtfs_obj$trips_df %>%
    dplyr::inner_join(gtfs_obj$routes_df, by = 'route_id') %>%
    dplyr::select(trip_id, route_id) %>%
    dplyr::distinct(trip_id, route_id) %>%
    dplyr::left_join(gtfs_obj$stop_times_df, by = 'trip_id') %>%
    dplyr::select(route_id, stop_id) %>%
    dplyr::distinct(route_id, stop_id)

  # update stops and reduce
  stops %<>%
    dplyr::inner_join(rs_df, by = 'stop_id') %>%
    dplyr::inner_join(plotting_data$routes_colors_df, by = 'route_id') %>%
    dplyr::slice(which(stop_id %in% possible_stops))

  m %>% leaflet::addCircleMarkers(
    popup = stops$stop_name,
    radius = 7,
    stroke = TRUE,
    weight = 4,
    color = 'black',
    fill = TRUE,
    fillColor = stops$color,
    fillOpacity = stop_opacity,
    lat = stops$lat,
    lng = stops$lng)

}
