#' map the shape, with stops, for one or more routes.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.

#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs_route_shapes <- function(gtfs_obj, route_ids, service_ids = NULL, include_stops = TRUE, stop_opacity = 0.5, route_colors = NULL) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$agency_df),
		!is.null(gtfs_obj$shapes_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df),
		length(route_ids) > 0,
		is.logical(include_stops))

	if(typeof(service_ids) != "character") service_ids %<>% unlist
	if(typeof(route_ids) != "character") route_ids %<>% unlist

	plotting_data <- get_routes_sldf(gtfs_obj, route_ids, service_ids)

	route_ids <- plotting_data$shapes_routes_df$route_id # update route ids


	# update/check variables
	## stop_opacity
	if(any(stop_opacity < 0, stop_opacity > 1)) stop_opacity = 0.5 # error in opacity is fixed

	## route_colors
	if(!is.null(route_colors)) {
  	if(length(route_colors) != length(route_ids)) {
  	  warning("route_colors and route_ids are not the same length. route_colors is ignored and default colors will be used.")
  	  route_colors <- NULL
  	} else {
  	  route_colors <- scales::col2hcl(route_colors) %>%
  	    sapply(. %>% substr(.,1,7), USE.NAMES = FALSE)
  	  shape_colors <- route_colors

  	  plotting_data$routes_colors_df$color <- route_colors
  	  plotting_data$shape_colors$color <- shape_colors
  	}
	}

  # find agency names from routes
	if((!"agency_id" %in% gtfs_obj$routes_df)) {
		# if no agency id, then assume all routes belong to agency_name
		agency <- gtfs_obj$agency_df$agency_name[1]
	} else {

	  # get agency_name name
		agency_ids <- gtfs_obj$routes_df %>%
			dplyr::slice(which(route_id %in% route_ids)) %>%
			magrittr::extract2('agency_id') %>%
			unique

		agency <- gtfs_obj$agency_df %>%
			dplyr::slice(which(agency_id %in% agency_ids)) %>%
			magrittr::extract2('agency_name')

	}

	agency_lbl <- paste(agency, collapse = " & ")

  # create map with shapes
  m <- plotting_data$gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  	leaflet::addPolylines(
  		color = plotting_data$shape_colors$color,
  		opacity = plotting_data$shape_colors$opacity,
  		popup = plotting_data$shape_colors$popups) %>% # assign color to each separate shape file
		leaflet::addLegend(
			colors = plotting_data$routes_colors_df$color,
			labels = paste("Route", plotting_data$routes_colors_df$route_short_name),
			title = stringr::str_to_title(agency_lbl))

	if(include_stops) {

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

		stops <- agency %>%
			lapply(. %>% get_agency_stops(gtfs_obj, agency_name = .)) %>%
			dplyr::bind_rows()
	  stops %<>%
	  	dplyr::inner_join(plotting_data$routes_colors_df, by = 'route_id')
	  stops %<>%
	  	dplyr::slice(which(stop_id %in% possible_stops))

	  m %<>% leaflet::addCircleMarkers(
			popup = stops$stop_name,
			radius = 6,
	    stroke = TRUE,
			weight = 4,
			color = 'black',
	    fill = TRUE,
	    fillColor = stops$color,
	    fillOpacity = stop_opacity,
			lat = stops$lat,
			lng = stops$lng)

	}

	m

}

#' Get shapes spatial data for given route ids
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @return Environment containing spatial data, labels, colorings used for plotting
#' @noRd

get_routes_sldf <- function(gtfs_obj, route_ids, service_ids = NULL) {

		stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$shapes_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df),
		length(route_ids) > 0)

		# check for bad route ids
		bad_route_ids <- route_ids[which(!route_ids %in% gtfs_obj$routes_df$route_id)]
		route_ids <- route_ids[which(route_ids %in% gtfs_obj$routes_df$route_id)]

		# error if all route ids are bad
		if(length(route_ids) == 0) {
			s <- "No provided Route ID(s) --- '%s' --- were found. Please provide valid Route IDs." %>% sprintf(paste(bad_route_ids, collapse = ", "))
			stop(s)
		}

		# warn if some route ids are omitted
		if(length(bad_route_ids) > 0) {
			s <- "Route ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_route_ids, collapse = ", "))
			warning(s)
		}

		if(!is.null(service_ids)) {

			# check service ids
			bad_service_ids <- service_ids[which(!service_ids %in% gtfs_obj$trips_df$service_id)]
			service_ids <- service_ids[which(service_ids %in% gtfs_obj$trips_df$service_id)]

			if(length(service_ids) == 0) {
				s <- "No provided Service ID(s) --- '%s' --- were found. Please provide valid Service IDs." %>% sprintf(paste(bad_service_ids, collapse = ", "))
				stop(s)
			}

			if(length(bad_service_ids) > 0) {
				s <- "Service ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_service_ids, collapse = ", "))
				warning(s)
			}


			shapes_routes_df <- gtfs_obj$trips_df %>%
				dplyr::slice(which(service_id %in% service_ids)) %>%
				dplyr::slice(which(route_id %in% route_ids)) %>%
				dplyr::select(shape_id, route_id, service_id) %>%
				dplyr::distinct(., shape_id, route_id, .keep_all = TRUE) # want only distinct routes

		} else {

			shapes_routes_df <- gtfs_obj$trips_df %>%
				dplyr::slice(which(route_id %in% route_ids)) %>%
				dplyr::select(shape_id, route_id, service_id) %>%
				dplyr::distinct(., shape_id, route_id, .keep_all = TRUE) # want only distinct routes

		}

		# extract matching shape ids
		shape_ids <- shapes_routes_df$shape_id

		if(length(shape_ids) == 0) {
			s <- "No shapes for Route ID '%s' were found." %>% sprintf(paste(route_ids, collapse = ", "))
			stop(s)
		}

		gtfstrips <- gtfs_obj$trips_df %>%
			dplyr::slice(which(route_id %in% route_ids))

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

	  # get updated shape ids (order matters)
	  shape_ids <- sapply(sp_lines@lines, function(x) x@ID)

	  df <- shape_ids %>%
	  	as.data.frame %>%
	  	`rownames<-`(., shape_ids)

	  gtfslines <- sp::SpatialLinesDataFrame(sp_lines, data = df) %>%
	  	rgeos::gSimplify(.00001)

	  # extract corresponding route ids and names for shape ids
	  routes_colors_df <- dplyr::data_frame(route_id = route_ids,
	  	color = scales::hue_pal()(length(route_ids))) %>%
	  	dplyr::left_join(gtfs_obj$routes_df %>% dplyr::select(route_id, route_short_name), by = 'route_id')


	  # merge colors to shape_routes
	  shapes_routes_colors_df <- shapes_routes_df %>%
	  	dplyr::left_join(routes_colors_df, by = 'route_id')

	  # make color vector for shapes
	  shape_colors <- shapes_routes_colors_df %>%
	  	dplyr::group_by(route_id) %>%
	  	dplyr::mutate(n = n(), opacity = 0.75/(n)) %>%
	  	dplyr::mutate(popups = paste("Route", route_short_name)) %>%
	  	dplyr::select(-n) %>%
	  	dplyr::ungroup() %>% # important to keep order correct!
	  	dplyr::slice(match(shape_id, shape_ids)) %>% # match helps resort rows so colors/labels match up with gtfslines (only works cause we have ONE of each shape)
	  	dplyr::select(shape_id, color, opacity, popups)

	  drop <- ls()[which(!(ls() %in% c("gtfslines", "shape_colors",
	  	"shapes_routes_df","routes_colors_df")))]
	  rm(list = drop)
	  rm('drop')

  	environment()
  }
