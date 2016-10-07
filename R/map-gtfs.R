#' General mapping function. Specify a map type and/or a route id.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param shape_ids Vector (Character). Shape IDs. NULL by Default.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#' @param only_stops Boolean. Whether to map only stops, no routes. Overrides `include_stops`. Default is FALSE.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5.
#' @param stop_details Boolean. Whether to generate detail stop information. Default is FALSE.
#' @param route_opacity Numeric. Value must be between 0 and 1. Default is .75.

#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs <- function(gtfs_obj, route_ids = NULL, service_ids = NULL, shape_ids = NULL, agency_name = NULL, include_stops = TRUE, only_stops = FALSE, stop_details = FALSE, stop_opacity = 0.5, route_opacity = 0.75, route_colors = NULL) {

  stopifnot(class(gtfs_obj) == 'gtfs',
    !is.null(gtfs_obj$stops_df),
    !is.null(gtfs_obj$agency_df),
    !is.null(gtfs_obj$shapes_df),
    !is.null(gtfs_obj$trips_df),
    !is.null(gtfs_obj$routes_df),
    any(is.character(agency_name), is.null(agency_name)),
    is.logical(include_stops),
    is.logical(only_stops),
    is.logical(stop_details))

  # check route ids ------------------------------------------------
  if(typeof(route_ids) != "character") route_ids %<>% unlist
  if(!is.null(route_ids)) {
    route_ids <- as.character(route_ids)
    route_ids <- unique(route_ids)
  } # if not null, ensure characters

  ## check service ids ------------------------
  if(typeof(service_ids) != "character") service_ids %<>% unlist
  if(!is.null(service_ids)) {
    service_ids <- as.character(service_ids)
    service_ids <- unique(service_ids)
  } # if not null, ensure characters

  # check shape ids ------------------------------------------------
  if(typeof(shape_ids) != "character") shape_ids %<>% unlist
  if(!is.null(shape_ids)) {
    shape_ids <- as.character(shape_ids)
    shape_ids <- unique(shape_ids)
  } # if not null, ensure characters

  # update/check variables ------------------------------------------------
  ## check opacities
  stop_opacity <- as.numeric(stop_opacity)
  route_opacity <- as.numeric(route_opacity)
  if(any(stop_opacity < 0, stop_opacity > 1)) stop_opacity = 0.5 # error in opacity is fixed
  if(any(route_opacity < 0, route_opacity > 1)) route_opacity = .75 # force ok ruote_opacity


  # NO AGENCY, NO ROUTES ------------------------------------------------
  if(is.null(agency_name) & is.null(route_ids)) {
    s <- sprintf("No agency name or route_id(s) were provided. The first observed agency is selected and all its routes will be mapped.")
    message(s)

    agency_name <- gtfs_obj$agency_df$agency_name[1]

    agency <- agency_name # need to rename so we can verify
    agency_ids <- gtfs_obj$agency_df %>%
      dplyr::slice(which(agency_name %in% agency)) %>%
      magrittr::extract2(1) %>%
      unique

    all_agency_routes <- gtfs_obj$routes_df %>%
      dplyr::slice(which(agency_id %in% agency_ids)) %>%
      dplyr::select(route_id) %>%
      magrittr::extract2(1) %>%
      unique()

    route_ids <- all_agency_routes

  }

  # YES AGENCY, NO ROUTES ------------------------------------------------
  if(!is.null(agency_name) & is.null(route_ids)) {

    # check agency names
    agency_names <- gtfs_obj$agency_df$agency_name %>% unique
    if(any(!agency_name %in% agency_names)) stop("Agency name not found.")

    agency <- agency_name # need to rename so we can verify
    agency_ids <- gtfs_obj$agency_df %>%
      dplyr::slice(which(agency_name %in% agency)) %>%
      magrittr::extract2(1) %>%
      unique

    all_agency_routes <- gtfs_obj$routes_df %>%
      dplyr::slice(which(agency_id %in% agency_ids)) %>%
      dplyr::select(route_id) %>%
      magrittr::extract2(1) %>%
      unique()

    route_ids <- all_agency_routes

  }

  # NO AGENCY, YES ROUTES ------------------------------------------------
  if(is.null(agency_name) & !is.null(route_ids)) {

    # check for valid routes
    all_routes <- gtfs_obj$routes_df$route_id %>% unique

    indx <- match(route_ids, all_routes) %>% stats::na.omit()
    not_found <- route_ids[!route_ids %in% all_routes] #routes not found

    #check to see if routes were dropped
    if(length(route_ids) > length(indx)) {
      s <- sprintf("route_id(s) %s are/were not found. Removed.", paste(not_found))
      warning(s)
    }

    route_ids <- all_routes[indx]

  }

  # YES ROUTES, YES AGENCY ------------------------------------------------
  if(!is.null(agency_name) & !is.null(route_ids)) {

    # check valid agency
    agency_names <- gtfs_obj$agency_df$agency_name %>% unique

    if(any(!agency_name %in% agency_names)) stop("Agency name not found.")

    # check for valid routes
    all_routes <- gtfs_obj$routes_df$route_id %>% unique

    indx <- match(route_ids, all_routes) %>% stats::na.omit()
    not_found <- route_ids[!route_ids %in% all_routes] #routes not found

    #check to see if routes were dropped
    if(length(route_ids) > length(indx)) {
      s <- sprintf("route_id(s) %s are/were not found. Removed.", paste(not_found))
      warning(s)
    }

    route_ids <- all_routes[indx]

  }


  if(is.null(agency_name) & !is.null(route_ids)) {

    if(length(route_ids) < 1) {
      stop("No specified route_ids were found.")
    }

    m <- map_gtfs_routes(gtfs_obj,
                      route_ids = route_ids,
                      service_ids = service_ids,
                      shape_ids = shape_ids,
                      include_stops = include_stops,
                      only_stops = only_stops,
                      stop_opacity = stop_opacity,
                      stop_details = stop_details,
                      route_opacity = route_opacity,
                      route_colors = route_colors)
  } else {

    stopifnot(length(route_ids) > 0, length(agency_name) > 0)

    m <- map_gtfs_agency_network(gtfs_obj,
                        agency_name = agency_name,
                        route_ids = route_ids,
                        service_ids = service_ids,
                        shape_ids = shape_ids,
                        include_stops = include_stops,
                        only_stops = only_stops,
                        stop_opacity = stop_opacity,
                        stop_details = stop_details,
                        route_opacity = route_opacity,
                        route_colors = route_colors)
  }

  m

}
