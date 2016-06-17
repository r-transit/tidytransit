# MAPPING NETWORKS ----------------------------------------------
# "NETWORKS" in this case is describing when the information of two or more routes is placed on
# the same map layer.

# Functions to quickly and easily map
# - all stops for a network
#	- all routes for a network

#' get stops for all routes of an agency
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_id Character. Provide the ID of the agency whose routes are being mapped.
#'
#' @return dataframe of route and stop ids for given agency id
#' @export

get_agency_stops <- function(gtfs_obj, agency_id) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$stop_times_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df))

	id <- agency_id
	rm('agency_id')

	# find agency routes
	route_ids <- gtfs_obj$routes_df %>%
		dplyr::slice(which(agency_id %in% id)) %>%
		dplyr::select(route_id) %>%
		magrittr::extract2(1) %>%
		unique

	# extract vector of all trips matching route_id
	trip_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% route_ids)) %>%
		dplyr::select(trip_id) %>%
		magrittr::extract2(1) %>%
		unique

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
  stops %<>% dplyr::inner_join(rs_df, by = 'stop_id')

	return(stops)

}


#' map all routes for an agency
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_id Character. Provide the ID of the agency whose routes are being mapped.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is FALSE
#'
#' @return Leaflet map object with all routes plotted for given agency ID.
#' @export

map_gtfs_agency_routes <- function(gtfs_obj, agency_id, include_stops = FALSE) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$agency_df),
		!is.null(gtfs_obj$shapes_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df),
		is.logical(include_stops))

	id <- agency_id
	rm('agency_id')

	# find agency routes
	route_ids <- gtfs_obj$routes_df %>%
		dplyr::slice(which(agency_id %in% id)) %>%
		dplyr::select(route_id) %>%
		magrittr::extract2(1) %>%
		unique

	# extract vector of all shapes matching route_id
	shape_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% route_ids)) %>%
		dplyr::select(shape_id) %>%
		magrittr::extract2(1) %>%
		unique

	if(length(shape_ids) == 0) {
		s <- "No shapes for Route ID '%s' were found." %>% sprintf(id)
		stop(s)
	}

	shape_routes_df <- gtfs_obj$trips_df %>%
		dplyr::select(shape_id, route_id) %>%
		unique

	gtfsroutes <- gtfs_obj$routes_df %>%
		dplyr::slice(which(route_id %in% route_ids))

	gtfstrips <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% route_ids))

	gtfsagency <- gtfs_obj$agency_df

	# extract all shapes for given shape ids
	gtfsshape <- gtfs_obj$shapes_df %>%
		dplyr::slice(which(shape_id %in% shape_ids))

	# code was taken from `stplanr::gtfs2sldf` (package::function)
	sp_lines <- (gtfsshape %>% dplyr::rename(lat = shape_pt_lat, lon = shape_pt_lon) %>%
		dplyr::group_by(shape_id) %>%
    dplyr::arrange(shape_pt_sequence) %>% dplyr::do_(gtfsline = "sp::Lines(sp::Line(as.matrix(.[,c('lon','lat')])),unique(.$shape_id))") %>%
    dplyr::ungroup() %>% dplyr::do_(gtfsline = "sp::SpatialLines(.[[2]], proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))")) %>%
		magrittr::extract2('gtfsline') %>%
		magrittr::extract2(1)

	df <- gtfstrips %>%
    dplyr::distinct(shape_id) %>%
    dplyr::do_("`rownames<-`(.,.$shape_id)") %>%
    as.data.frame

  gtfslines <- sp::SpatialLinesDataFrame(sp_lines, data = df) %>% rgeos::gSimplify(.00001)

  # get shape ids
  ids <- gtfslines@lines %>%
  	lapply(. %>% '@'('ID')) %>% unlist

  # extract corresponding route ids and names for shape ids
  route_colors_df <- dplyr::data_frame(route_id = route_ids,
  	color = scales::hue_pal()(length(route_ids))) %>%
  	dplyr::left_join(gtfs_obj$routes_df %>% dplyr::select(route_id, route_short_name), by = 'route_id')

  # merge colors to shape_routes
  shape_routes_color_df <- shape_routes_df %>%
  	dplyr::left_join(route_colors_df, by = 'route_id')

  # make color vector for shapes
  shape_clrs <- shape_routes_color_df %>%
  	dplyr::slice(match(ids, shape_routes_color_df$shape_id)) %>% # order matters; use `match` not `which`
  	magrittr::extract2('color')

  # get route names corresponding to colors
  popups <- shape_routes_color_df %>%
  	dplyr::slice(match(ids, shape_routes_color_df$shape_id)) %>%
  	magrittr::extract2('route_id')

  # get agency name
  agency_name <- gtfs_obj$agency_df %>%
  	dplyr::slice(which(agency_id %in% id)) %>%
  	magrittr::extract2('agency_name')

  # create map with shapes
  m <- gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  	leaflet::addPolylines(
  		color = shape_clrs,
  		opacity = 0.125,
  		popup = popups) %>% # assign color to each separate shape file
		leaflet::addLegend(
			colors = route_colors_df$color,
			labels = paste("Route",route_colors_df$route_short_name),
			title = stringr::str_to_title(agency_name))

	if(include_stops) {
		# get stops data
	  stops <- get_agency_stops(gtfs_obj, agency_id = id)
	  stops %<>% dplyr::left_join(route_colors_df, by = 'route_id')

	  m %>% leaflet::addCircleMarkers(
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
