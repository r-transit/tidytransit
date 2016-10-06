#' map the shape, with stops, for one or more routes.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param shape_ids Vector (Character). Shape IDs. NULL by Default.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#' @param only_stops Boolean. Whether to map only stops, no routes. Overrides `include_stops`. Default is FALSE.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5.
#' @param stop_details Boolean. Whether to generate detail stop information. Default is FALSE.
#' @param route_opacity Numeric. Value must be between 0 and 1. Default is NULL.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.

map_gtfs_routes <- function(gtfs_obj, route_ids, service_ids = NULL, shape_ids = NULL, include_stops = TRUE, only_stops = FALSE, stop_opacity = 0.5, stop_details = FALSE, route_opacity = NULL, route_colors = NULL) {

  # GET PLOTTING DATA --------------------------------
  plotting_data <- get_routes_sldf(gtfs_obj, route_ids, service_ids, shape_ids, route_opacity, route_colors)

	# AGENCY LABELS (maybe defunct?) ------------------------------------------------
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

	# PLOTTING ------------------------------------------------
  # create map with shapes
  m <- plotting_data$gtfslines %>%
  	leaflet::leaflet() %>%
  	leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
		leaflet::addLegend(
			colors = plotting_data$routes_colors_df$color,
			labels = paste("Route", plotting_data$routes_colors_df$route_short_name),
			title = stringr::str_to_title(agency_lbl))

  if(only_stops) {
    include_stops <- TRUE
  } else {
    m %<>% leaflet::addPolylines(
      color = plotting_data$shape_colors$color,
      opacity = plotting_data$shape_colors$opacity,
      label = plotting_data$shape_colors$labels,
      popup = plotting_data$shape_colors$popups) # assign color to each separate shape file
  }

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
			dplyr::bind_rows() %>%
	  	dplyr::inner_join(plotting_data$routes_colors_df, by = 'route_id') %>%
	  	dplyr::slice(which(stop_id %in% possible_stops))

	  # whether to add stop details
	  if(stop_details) {
	  	stops %<>%
	    	dplyr::mutate(popups = gen_stop_popups(stop_name, stop_id, lat, lng))
	  } else {
	  	stops %<>%
	    	dplyr::mutate(popups = stop_name)
	  }

	  m %<>% leaflet::addCircleMarkers(
			label = stops$stop_name,
			popup = stops$popups,
			radius = 6,
	    stroke = TRUE,
	    opacity = stop_opacity,
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

