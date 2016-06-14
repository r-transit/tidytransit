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
		warn <- "Stop '%s' was not found. NULL is returned" %>% sprintf(id)
		warning(warn)
		return(NULL)
	}

	stop <- df %>%
		dplyr::select(stop_name, stop_lat, stop_lon) %>%
		dplyr::rename(name = stop_name, lat = stop_lat, lng = stop_lon)

	m <- stop %>%
		leaflet::leaflet() %>%
		leaflet::addTiles() %>%
		leaflet::addMarkers(popup = stop$name, lat = stp$lat, lng = stop$lng)
	m

}

#' map all stops for a route
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_id Character. A single ID for a route of interest.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_all_stops_in_route <- function(gtfs_obj, route_id) {

	stopifnot(class(gtfs_obj) == 'gtfs', !is.null(gtfs_obj$stops_df), !is.null(gtfs_obj$routes_df))

	id <- route_id

	# extract vector of all trips matching route_id
	trip_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id)) %>%
		dplyr::select(trip_id) %>%
		magrittr::extract2(1)

	if(dim(trips)[1] == 0) {
		warn <- "No trips for Route ID '%s' were found. NULL is returned" %>% sprintf(id)
		warning(warn)
		return(NULL)
	}

	# extract all possible stops across all trips for given route
	possible_stops <- gtfs_obj$stop_times_df %>%
		dplyr::slice(which(trip_id %in% trip_ids)) %>%
		dplyr::select(stop_id) %>%
		unique %>%
		extract2(1)

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
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_all_stops_in_route <- function(gtfs_obj, route_id) {

	stopifnot(class(gtfs_obj) == 'gtfs', !is.null(gtfs_obj$stops_df), !is.null(gtfs_obj$routes_df))

	id <- route_id

	# extract vector of all trips matching route_id
	shape_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id)) %>%
		dplyr::select(shape_id) %>%
		magrittr::extract2(1) %>%
		unique

	if(dim(shape_ids)[1] == 0) {
		s <- "No shapes for Route ID '%s' were found. NULL is returned" %>% sprintf(id)
		stop(s)
	}


	# extract vector of all trips matching route_id
	trip_ids <- gtfs_obj$trips_df %>%
		dplyr::slice(which(route_id %in% id)) %>%
		dplyr::select(trip_id) %>%
		magrittr::extract2(1)

	if(dim(trips)[1] == 0) {
		s <- "No trips for Route ID '%s' were found. NULL is returned" %>% sprintf(id)
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

	# extract all possible stops across all trips for given route
	possible_stops <- gtfs_obj$stop_times_df %>%
		dplyr::slice(which(trip_id %in% trip_ids)) %>%
		dplyr::select(stop_id) %>%
		unique %>%
		extract2(1)

	stops <- gtfs_obj$stops_df %>%
		dplyr::slice(which(stop_id %in% possible_stops)) %>%
		dplyr::select(stop_name, stop_lat, stop_lon) %>%
		dplyr::rename(name = stop_name, lat = stop_lat, lng = stop_lon)


	# code was taken from `stplanr::gtfs2sldf` (package::function)
	sp_lines <- (gtfsshape %>% dplyr::rename(lat = shape_pt_lat, lon = shape_pt_lon) %>%
		dplyr::group_by_(~shape_id) %>%
    dplyr::arrange_(~shape_pt_sequence) %>% dplyr::do_(gtfsline = "sp::Lines(sp::Line(as.matrix(.[,c('lon','lat')])),unique(.$shape_id))") %>%
    dplyr::ungroup() %>% dplyr::do_(gtfsline = "sp::SpatialLines(.[[2]], proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))")) %>%
		magrittr::extract2('gtfsline') %>%
		magrittr::extract2(1)

	df <- gtfstrips %>% dplyr::inner_join(gtfsroutes) %>%
            dplyr::distinct_(~route_id, ~shape_id, ~route_short_name,
                ~route_long_name, ~route_desc, ~route_type, ~route_color,
                ~route_text_color, ~agency_id) %>% dplyr::select_(~route_id,
            ~shape_id, ~route_short_name, ~route_long_name, ~route_desc,
            ~route_type, ~route_color, ~route_text_color, ~agency_id) %>%
            dplyr::inner_join(gtfsagency) %>%
            dplyr::do_("`rownames<-`(.,.$shape_id)") %>%
            as.data.frame

  gtfslines <- sp::SpatialLinesDataFrame(sp_lines, data = df) %>% gSimplify(.00001)

  m <- gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addTiles() %>%
  	leaflet::addPolylines(color = 'blue') %>%
  	leaflet::addCircleMarkers(
			popup = stops$name,
			color = 'red',
			radius = 4,
	    stroke = TRUE,
	    fillOpacity = 0.7,
			lat = stops$lat,
			lng = stops$lng) %>%
		leaflet::addLegend(colors = c('red', 'blue'), labels = c("Stops", "Route"))
	m

}


