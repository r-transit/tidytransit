#' Map all routes for an agency. If no agency is specified, the first observed agency is taken by default and all its routes are mapped.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#' @param only_stops Boolean. Whether to map only stops, no routes. Default is FALSE.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.

#'
#' @return Leaflet map object with all routes plotted for given agency ID.
#' @export

map_gtfs_agency_network <- function(gtfs_obj, agency_name = NULL, route_ids = NULL, include_stops = TRUE, only_stops = FALSE, stop_opacity = 0.5, route_colors = NULL) {

  stopifnot(class(gtfs_obj) == 'gtfs',
            !is.null(gtfs_obj$stops_df),
            !is.null(gtfs_obj$agency_df),
            !is.null(gtfs_obj$shapes_df),
            !is.null(gtfs_obj$trips_df),
            !is.null(gtfs_obj$routes_df),
            length(agency_name) < 2,
            any(is.character(agency_name), is.null(agency_name)),
            is.logical(include_stops))

  # if agency_name is null, take the first agency_name in gtfs obj
  if(is.null(agency_name)) {
    agency <- gtfs_obj$agency_df$agency_name[1]
    s <- sprintf("No agency_name was provided. The first observed agency, %s, name is used.", agency)
    message(s)
  } else {
    agency <- agency_name
    rm('agency_name')
  }

  if(is.null(route_ids)) {
    s <- sprintf("No route_id(s) was/were provided. All agency routes will be mapped.")
    message(s)
  }

  ## check route_ids
  if(typeof(route_ids) != "character") route_ids %<>% unlist
  if(!is.null(route_ids)) route_ids <- as.character(route_ids) # if not null, ensure characters

  # find agency routes
  if(!"agency_id" %in% names(gtfs_obj$routes_df)) {
    # if no agency id, then assume all routes belong to agency_name
    all_routes <- gtfs_obj$routes_df$route_id %>% unique

    if(is.null(route_ids)) {
      route_ids <- all_routes
    } else {
      indx <- match(all_routes, route_ids) %>% na.omit()
      route_ids <- all_routes[indx]
    }
  } else {
    ## get all routes
    # find routes for a given agency
    agency_ids <- gtfs_obj$agency_df %>%
      dplyr::slice(which(agency_name %in% agency)) %>%
      magrittr::extract2(1) %>%
      unique

    all_routes <- gtfs_obj$routes_df %>%
      dplyr::slice(which(agency_id %in% agency_ids)) %>%
      dplyr::select(route_id) %>%
      magrittr::extract2(1) %>%
      unique

    if(is.null(route_ids)) {
      route_ids <- all_routes
    } else {
      indx <- match(all_routes, route_ids) %>% na.omit()
      not_found <- route_ids[!route_ids %in% all_routes] #routes not found

      #check to see if routes were dropped
      if(length(route_ids) > length(indx)) {
        s <- sprintf("route_id(s) %s are/were not found.", paste(not_found))
        warning(s)
      }

      route_ids <- all_routes[indx]
    }
  }

  # ensure routes are found
  if(length(route_ids) < 1) {
    warning("Specified route_ids not found. Defaulting to using known agency routes.")
    route_ids <- all_routes
  }

  # update/check variables
  ## stop_opacity
  if(any(stop_opacity < 0, stop_opacity > 1)) stop_opacity = 0.5 # error in opacity is fixed

  ## route_colors
  if(!is.null(route_colors)) {
    if(length(route_colors) != length(route_ids)) {
      warning("route_colors and route_ids are not the same length. route_colors is ignored and default colors will be used.")
    } else {
      route_colors <- scales::col2hcl(route_colors) %>%
        sapply(. %>% substr(.,1,7), USE.NAMES = FALSE)
      shape_colors <- route_colors

      plotting_data$routes_colors_df$color <- route_colors
      plotting_data$shape_colors$color <- shape_colors
    }
  }


  plotting_data <- get_routes_sldf(gtfs_obj = gtfs_obj, route_ids = route_ids)

  # create map with shapes
  m <- plotting_data$gtfslines %>%
    leaflet::leaflet() %>%
    leaflet::addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
    leaflet::addLegend(
      colors = plotting_data$routes_colors_df$color,
      labels = paste("Route", plotting_data$routes_colors_df$route_short_name),
      title = stringr::str_to_title(agency))

  if(only_stops) {
    include_stops <- TRUE
  } else {
    m  %<>%
      leaflet::addPolylines(
        color = plotting_data$shape_colors$color,
        opacity = plotting_data$shape_colors$opacity,
        popup = plotting_data$shape_colors$popups)
  }

  if(include_stops) {
    # get stops data
    stops <- get_agency_stops(gtfs_obj, agency_name = agency)
    stops %<>%
      dplyr::inner_join(plotting_data$routes_colors_df, by = 'route_id')

    m %<>% leaflet::addCircleMarkers(
      popup = stops$stop_name,
      radius = 6,
      weight = 4,
      stroke = TRUE,
      color = 'black',
      opacity = stop_opacity,
      fill = TRUE,
      fillColor = stops$color,
      fillOpacity = 0.5,
      lat = stops$lat,
      lng = stops$lng)
  }

  m

}


#' Get stops for all routes of an agency.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped. Default is NULL, which assumes only one agency exists, taking first agency name.
#'
#' @return dataframe of route and stop ids for given agency id
#' @noRd

get_agency_stops <- function(gtfs_obj, agency_name) {

	stopifnot(class(gtfs_obj) == 'gtfs',
		!is.null(gtfs_obj$stops_df),
		!is.null(gtfs_obj$stop_times_df),
		!is.null(gtfs_obj$trips_df),
		!is.null(gtfs_obj$routes_df),
		any(is.character(agency_name), is.null(agency_name)))

	# rename agency name
	agency <- agency_name
	rm('agency_name')

	# find agency routes
	if(!"agency_id" %in% names(gtfs_obj$routes_df)) {
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
		s <- "No trips for Route ID '%s' were found." %>%
		  sprintf(agency)
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

  # update stops
  # note: stops are shared between routes. {stops x routes} > {stops}
  stops %<>%
  	dplyr::inner_join(rs_df, by = 'stop_id')

	return(stops)

}



