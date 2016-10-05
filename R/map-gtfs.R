#' General mapping function. Specify a map type and/or a route id.
#'
#' @param gtfs_obj A GTFS list object with components agency_df, etc.
#' @param agency_name Character. Provide the name of the agency whose routes are being mapped.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param include_stops Boolean. Whether to layer on stops to the route shape. Default is TRUE.
#' @param only_stops Boolean. Whether to map only stops, no routes. Overrides `include_stops`. Default is FALSE.
#' @param stop_opacity Numeric. Value must be between 0 and 1. Defaults is 0.5.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000'). Default is NULL.
#'
#' @return Leaflet map object with all stop lat/long values plotted for a route.
#' @export

map_gtfs <- function(gtfs_obj, agency_name = NULL, route_ids = NULL, service_ids = NULL, include_stops = TRUE, only_stops = FALSE, stop_opacity = 0.5, route_colors = NULL) {

  # if an agency is listed, map it.
  if(!is.null(agency_name)) {
    m <- map_gtfs_agency_network(gtfs_obj,
                                 agency_name = agency_name,
                                 route_ids = route_ids,
                                 include_stops = include_stops,
                                 only_stops = only_stops,
                                 stop_opacity = stop_opacity,
                                 route_colors = route_colors)
  } else {
    # check route_ids. If exists, map.
    if(!is.null(route_ids)) {
      m <- map_gtfs_routes(gtfs_obj,
                          route_ids = route_ids,
                          service_ids = service_ids,
                          include_stops = include_stops,
                          only_stops = only_stops,
                          stop_opacity = stop_opacity,
                          route_colors = route_colors)
    } else {
      m <- map_gtfs_agency_network(gtfs_obj,
                                   agency_name = agency_name,
                                   route_ids = route_ids,
                                   include_stops = include_stops,
                                   only_stops = only_stops,
                                   stop_opacity = stop_opacity,
                                   route_colors = route_colors)
    }
  }

  m

}
