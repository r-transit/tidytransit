#' Convert stops and shapes to Simple Features
#' 
#' Stops are converted to POINT sf data frames. Shapes are created as
#' LINESTRING data frame. Note that this function replaces stops and shapes
#' tables in gtfs_obj.
#'
#' @param gtfs_obj a standard tidytransit gtfs object
#' @param skip_shapes if TRUE, shapes are not converted. Default FALSE.
#' @param crs optional coordinate reference system (used by sf::st_transform) to transform 
#'            lon/lat coordinates of stops and shapes
#' @param quiet boolean whether to print status messages
#' @return gtfs_obj a tidytransit gtfs object with stops and shapes as sf data frames
#' @export
gtfs_as_sf <- function(gtfs_obj, skip_shapes = FALSE, crs = NULL, quiet = TRUE) {
  if(!quiet) message('Converting stops to simple features')
  gtfs_obj$stops <- try(stops_as_sf(gtfs_obj$stops, crs))
  
  if(feed_contains(gtfs_obj, "shapes") && !skip_shapes) {
    if(!quiet) message('Converting shapes to simple features')
    gtfs_obj$shapes <- try(shapes_as_sf(gtfs_obj$shapes, crs))
  } else if(feed_contains(gtfs_obj, "shapes") && !skip_shapes) { 
    warning('No shapes available in gtfs_obj') 
  }
  return(gtfs_obj)
}

#' Convert stops into Simple Features Points
#'
#' @param stops a gtfs$stops dataframe
#' @param crs optional coordinate reference system (used by sf::st_transform) to transform 
#'            lon/lat coordinates
#' @return an sf dataframe for gtfs routes with a point column
#' @export
#' @examples
#' data(gtfs_duke)
#' some_stops <- gtfs_duke$stops[sample(nrow(gtfs_duke$stops), 40),]
#' some_stops_sf <- stops_as_sf(some_stops)
#' plot(some_stops_sf)
stops_as_sf <- function(stops, crs = NULL) {
  stops_sf <- sf::st_as_sf(stops,
                           coords = c("stop_lon", "stop_lat"),
                           crs = 4326)
  if(!is.null(crs)) {
    stops_sf <- sf::st_transform(stops_sf, crs)
  }
  
  return(stops_sf)
}

#' Convert shapes into Simple Features Linestrings
#'
#' @param gtfs_shapes a gtfs$shapes dataframe
#' @param crs optional coordinate reference system (used by sf::st_transform) to transform 
#'            lon/lat coordinates 
#' @export
#' @return an sf dataframe for gtfs shapes
shapes_as_sf <- function(gtfs_shapes, crs = NULL) {
  list_of_line_tibbles <- split(gtfs_shapes, gtfs_shapes$shape_id)
  list_of_linestrings <- lapply(list_of_line_tibbles, shape_as_sf_linestring)
  
  shape_linestrings <- sf::st_sfc(list_of_linestrings, crs = 4326)
  
  shapes_sf <- sf::st_sf(shape_id = names(list_of_line_tibbles), geometry = shape_linestrings)
  shapes_sf$shape_id <- as.character(shapes_sf$shape_id)
  
  if(!is.null(crs)) {
    shapes_sf <- sf::st_transform(shapes_sf, crs)
  }
  
  return(shapes_sf)
}

#' Get all trip shapes for a given route and service
#'
#' @param gtfs_sf_obj tidytransit gtfs object with sf data frames
#' @param route_ids routes to extract
#' @param service_ids service_ids to extract
#' @return an sf dataframe for gtfs routes with a row/linestring for each trip
#' @importFrom dplyr inner_join distinct
#' @importFrom sf st_cast
#' @export
#' @examples
#' data(gtfs_duke)
#' gtfs_duke_sf <- gtfs_as_sf(gtfs_duke)
#' routes_sf <- get_route_geometry(gtfs_duke_sf)
#' plot(routes_sf[c(1,1350),])
get_route_geometry <- function(gtfs_sf_obj, route_ids = NULL, service_ids = NULL) {
  if(!"sf" %in% class(gtfs_sf_obj$shapes)) {
    stop("shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)")
  }
  trips <- gtfs_sf_obj$trips
  if(!is.null(route_ids)) {
    trips <- filter(trips, route_id %in% route_ids)
    if(nrow(trips) == 0) {
      warning("No trips with route_id ", route_ids, " found")
    }
  }
  if(!is.null(service_ids)) {
    trips <- filter(trips, service_id %in% service_ids)
    if(nrow(trips) == 0) {
      warning("No trips with service_id ", service_ids, " found")
    }
  }

  shape_ids = distinct(trips, route_id, shape_id)
  
  trip_shapes <- inner_join(gtfs_sf_obj$shapes, shape_ids, by = "shape_id")
  route_shapes <- trip_shapes %>% 
    group_by(route_id) %>% 
    summarise() %>% 
    st_cast("MULTILINESTRING")
  
  return(route_shapes)
}

#' Get all trip shapes for given trip ids
#'
#' @param gtfs_sf_obj tidytransit gtfs object with sf data frames
#' @param trip_ids trip_ids to extract shapes
#' @return an sf dataframe for gtfs routes with a row/linestring for each trip
#' @export
#' @examples
#' data(gtfs_duke)
#' gtfs_duke <- gtfs_as_sf(gtfs_duke)
#' trips_sf <- get_trip_geometry(gtfs_duke, c("t_726295_b_19493_tn_41", "t_726295_b_19493_tn_40"))
#' plot(trips_sf[1,])
get_trip_geometry <- function(gtfs_sf_obj, trip_ids) {
  if(!"sf" %in% class(gtfs_sf_obj$shapes)) {
    stop("shapes not converted to sf, use gtfs_obj <- gtfs_as_sf(gtfs_obj)")
  }
  id_diff = setdiff(trip_ids, gtfs_sf_obj$trips$trip_id)
  if(length(id_diff) > 0) {
    warning('"', paste(id_diff, collapse=", "), '" not found in trips data frame')
  }

  trips = gtfs_sf_obj$trips %>% filter(trip_id %in% trip_ids)
  trips_shapes = dplyr::inner_join(gtfs_sf_obj$shapes, trips, by = "shape_id")
  return(trips_shapes)
}

#' return an sf linestring with lat and long from gtfs
#' @param df dataframe from the gtfs shapes split() on shape_id
#' @noRd
#' @return st_linestring (sfr) object
shape_as_sf_linestring <- function(df) {
  # as suggested by www.github.com/mdsumner

  m <- as.matrix(df[order(df$shape_pt_sequence),
                    c("shape_pt_lon", "shape_pt_lat")])

  return(sf::st_linestring(m))
}

#' Transform or convert coordinates of a gtfs feed
#' @param gtfs_obj tidygtfs object
#' @param crs target coordinate reference system, used by sf::st_transform
#' @importFrom sf st_transform
#' @export
gtfs_transform = function(gtfs_obj, crs) {
  if(!inherits(gtfs_obj$stops, "sf")) {
    gtfs_obj <- gtfs_as_sf(gtfs_obj)
  }
  gtfs_obj$stops <- st_transform(gtfs_obj$stops, crs)
  if(feed_contains(gtfs_obj, "shapes")) gtfs_obj$shapes <- st_transform(gtfs_obj$shapes, crs)
  gtfs_obj
}
