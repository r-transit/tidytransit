# MAPPING NETWORKS ----------------------------------------------
# "NETWORKS" in this case is describing when the information of two or more routes is placed on
# the same map layer.

# Functions to quickly and easily map
# - all stops for a network
#	- all routes for a network

#' get stops for all routes of an agency
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped. Default is NULL, which assumes only one agency exists, taking first agency name.
#'
#' @return dataframe of route and stop ids for given agency id
#' @noRd

get_agency_stops <- function(gtfs_obj, agency_name) {

	# rename agency name
	agency <- agency_name
	rm('agency_name')

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$stop_times_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df))

	# find agency routes
	if((!"agency_id" %in% gtfs_obj$routes_df)) {
		# if no agency id, then assume all routes belong to agency_name
		route_ids <- gtfs_obj$routes_df$route_id %>% unique
	} else {

		# find routes for a given agency
		agency_ids <- gtfs_obj$agency_df %>%
			dplyr::slice(which(agency_name %in% agency)) %>%
			magrittr::extract2(1) %>%
			unique

		route_ids <- gtfs_obj$routes_df %>%
			dplyr::slice(which(agency_id %in% agency_ids)) %>%
			dplyr::select(route_id) %>%
			magrittr::extract2(1) %>%
			unique
	}

	# extract vector of all trips matching route_id
	trip_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% route_ids)) %>%
		dplyr::select(trip_id) %>%
		magrittr::extract2(1) %>%
		unique

	if(length(trip_ids) == 0) {
		s <- "No trips for Route ID '%s' were found." %>% sprintf(agency)
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

  # update stops
  # note: stops are shared between routes. {stops x routes} > {stops}
  stops %<>%
  	dplyr::inner_join(rs_df, by = 'stop_id')

	return(stops)

}


#' map all routes for an agency
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is FALSE
#'
#' @return Leaflet map object with all routes plotted for given agency ID.
#' @export

map_gtfs_agency_routes <- function(gtfs_obj, agency_name = NULL, include_stops = FALSE) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$agency_df),
		!is.null(gtfs_obj$shapes_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df),
		length(agency_name) < 2,
		is.logical(include_stops))

	# if agency_name is null, take the first agency_name in gtfs obj
	if(is.null(agency_name)) {
		agency <- gtfs_obj$agency_df$agency_name[1]
	} else {
		agency <- agency_name
	}

	rm('agency_name')

	# find agency routes
	if((!"agency_id" %in% gtfs_obj$routes_df)) {
		# if no agency id, then assume all routes belong to agency_name
		route_ids <- gtfs_obj$routes_df$route_id %>% unique
	} else {

		# find routes for a given agency
		agency_ids <- gtfs_obj$agency_df %>%
			dplyr::slice(which(agency_name %in% agency)) %>%
			magrittr::extract2(1) %>%
			unique

		route_ids <- gtfs_obj$routes_df %>%
			dplyr::slice(which(agency_id %in% agency_ids)) %>%
			dplyr::select(route_id) %>%
			magrittr::extract2(1) %>%
			unique
	}

	plotting_data <- get_routes_sldf(gtfs_obj = gtfs_obj, route_ids = route_ids)

  # create map with shapes
  m <- plotting_data$gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  	leaflet::addPolylines(
  		color = plotting_data$shape_colors$color,
  		opacity = plotting_data$shape_colors$opacity,
  		popup = plotting_data$shape_colors$popups) %>%
		leaflet::addLegend(
			colors = plotting_data$routes_colors_df$color,
			labels = paste("Route", plotting_data$routes_colors_df$route_short_name),
			title = stringr::str_to_title(agency))

	if(include_stops) {
		# get stops data
	  stops <- get_agency_stops(gtfs_obj, agency_name = agency)
	  stops %<>%
	  	dplyr::inner_join(plotting_data$routes_colors_df, by = 'route_id')

	  m %<>% leaflet::addCircleMarkers(
			popup = stops$stop_name,
			radius = 4,
	    stroke = TRUE,
			color = 'black',
	    opacity = 0.3,
	    fill = TRUE,
	    fillColor = stops$color,
			lat = stops$lat,
			lng = stops$lng)
	}

	return(m)

}
