# Purpose -----------------------------------------------------------------

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

	stopifnot(class(gtfs_obj) == 'gtfs', !is.null(gtfs_obj$stops_df))

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

	stopifnot(class(gtfs_obj) == 'gtfs', !is.null(gtfs_obj$stops_df), !is.null(gtfs_obj$routes_df))

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


#' map the shape, with stops, for a route
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_id Character. A single ID for a route of interest.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_route_shape <- function(gtfs_obj, route_id, include_stops = TRUE) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$agency_df),
		!is.null(gtfs_obj$shapes_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df))

	id <- route_id
	rm('route_id')

	# extract vector of all trips matching route_id
	shape_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id)) %>%
		dplyr::select(shape_id) %>%
		magrittr::extract2(1) %>%
		unique

	if(length(shape_ids) == 0) {
		s <- "No shapes for Route ID '%s' were found." %>% sprintf(id)
		stop(s)
	}

	gtfsroutes <- gtfs_obj$routes_df %>%
		dplyr::slice(which(route_id %in% id))

	gtfstrips <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id))

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

  if(include_stops) {

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
  }

  m <- gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addTiles() %>%
  	leaflet::addPolylines(color = 'blue')

  # get agency name
  agency <- gtfs_obj$routes_df %>%
  	dplyr::slice(which(route_id %in% id)) %>%
  	magrittr::extract2('agency_id')

  agency_name <- gtfs_obj$agency_df %>%
		dplyr::slice(which(agency_id %in% agency)) %>%
  	magrittr::extract2('agency_name')


	if(include_stops) {
		m %>%
			leaflet::addCircleMarkers(
				popup = stops$name,
				color = 'red',
				radius = 4,
		    stroke = TRUE,
		    fillOpacity = 0.7,
				lat = stops$lat,
				lng = stops$lng) %>%
			leaflet::addLegend(colors = c('red', 'blue'),
				labels = c("Stops", "Route"),
				title = stringr::str_to_title(agency_name))
	} else {
		m %>%
			leaflet::addLegend(colors = c('blue'),
				labels = c("Route"),
				title = stringr::str_to_title(agency_name))
	}

}


#' map all routes for an agency
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_id Character. Provide the ID of the agency whose routes are being mapped.
#'
#' @return Leaflet map object with all routes plotted for given agency ID.
#' @export

map_gtfs_agency_routes <- function(gtfs_obj, agency_id) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$agency_df),
		!is.null(gtfs_obj$shapes_df),
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
	m

}
