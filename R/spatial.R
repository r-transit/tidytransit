#' Convert stops and shapes to Simple Features#' 
#' 
#' Stops are converted to POINT sf data frames. Shapes are created as
#' LINESTRING data frame. Note that this function replaces stops and shapes
#' tables in gtfs_obj.
#'
#' @param gtfs_obj a standard tidytransit gtfs object
#' @param skip_shapes if TRUE, shapes are not converted. Default FALSE.
#' @param quiet boolean whether to print status messages
#' @return gtfs_obj a tidytransit gtfs object with stops and shapes as sf data frames
#' @export
gtfs_as_sf <- function(gtfs_obj, skip_shapes = FALSE, quiet = TRUE) {
  if(!quiet) message('Converting stops to simple features')
  gtfs_obj$stops <- try(get_stop_geometry(gtfs_obj$stops))
  
  if(feed_contains(gtfs_obj, "shapes") && !skip_shapes) {
    if(!quiet) message('Converting shapes to simple features')
    gtfs_obj$shapes <- try(get_shapes_geometry(gtfs_obj$shapes))
  } else { 
    warning('No shapes available in gtfs_obj') 
  }
  
  return(gtfs_obj)
}

#' Make Stops into Simple Features Points
#'
#' @param stops a gtfs$stops dataframe
#' @return an sf dataframe for gtfs routes with a point column
#' @examples
#' data(gtfs_duke)
#' some_stops <- gtfs_duke$stops[sample(nrow(gtfs_duke$stops), 40),]
#' some_stops_sf <- get_stop_geometry(some_stops)
#' plot(some_stops_sf)
get_stop_geometry <- function(stops) {
  stops_sf <- sf::st_as_sf(stops,
                           coords = c("stop_lon", "stop_lat"),
                           crs = 4326)
  return(stops_sf)
}

#' Make shapes into Simple Features Linestrings
#'
#' @param shapes a gtfs$shapes dataframe
#' @return an sf dataframe for gtfs shapes
get_shapes_geometry <- function(shapes) {
  list_of_line_tibbles <- split(shapes, shapes$shape_id)
  list_of_linestrings <- lapply(list_of_line_tibbles, shape_as_sf_linestring)
  
  shape_linestrings <- sf::st_sfc(list_of_linestrings, crs = 4326)
  
  shapes_sf <- sf::st_sf(shape_id = unique(shapes$shape_id), geometry = shape_linestrings)
  shapes_sf$shape_id <- as.character(shapes_sf$shape_id)
  
  return(shapes_sf)
}

#' Get all trip shapes for a given route and service.
#'
#' @param gtfs_sf_obj tidytransit gtfs object with sf data frames
#' @param route_ids routes to extract
#' @param service_ids service_ids to extract
#' @return an sf dataframe for gtfs routes with a row/linestring for each trip
#' @export
#' @examples
#' data(gtfs_duke)
#' routes_sf <- get_route_geometry(gtfs_duke)
#' plot(routes_sf[1,])
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

  trip_shapes = dplyr::inner_join(trips, gtfs_sf_obj$shapes, by = "shape_id")
  return(trip_shapes)
}

#' Get all trip shapes for a given route and service.
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
  id_diff = setdiff(trip_ids, trips$trip_id)
  if(length(id_diff) > 0) warning('"', paste(id_diff, collapse=", "), '" not found in trips data frame')

  gtfs_sf_obj$trips %>% 
    filter(trip_id %in% trip_ids) %>% 
    dplyr::inner_join(gtfs_sf_obj$shapes, by = "shape_id")
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

#' return an sf multilinestring with lat and long from gtfs for a route
#' TODO not used
#' 
#' @param df the shapes dataframe from a gtfs object
#' @keywords internal
#' @return a multilinestring simple feature geometry (sfg) for the routes
shapes_as_sfg <- function(df) {
  # as suggested by www.github.com/mdsumner
  l_dfs <- split(df, df$shape_id)

  l_linestrings <- lapply(l_dfs,
                          shape_as_sf_linestring)

  return(sf::st_multilinestring(l_linestrings))
}

#' Buffer using common urban planner distances
#' TODO not used
#'
#' merges gtfs objects
#' @param df_sf1 a simple features data frame
#' @param dist default "h" - for half mile buffers. can also pass "q".
#' @param crs default epsg 26910. can be any other epsg
#' @return a simple features data frame with planner buffers
#' @keywords internal
planner_buffer <- function(df_sf1,dist="h",crs=26910) {
  distance <- 804.672
  if(dist=="q"){distance <- 402.336}
  df2 <- sf::st_transform(df_sf1,crs)
  df3 <- sf::st_buffer(df2,dist=distance)
  return(df3)
}