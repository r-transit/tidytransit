#' Add Simple Features for Stops and Routes to GTFS Object
#'
#' @param gtfs_obj a standard gtfsr object
#' @param quiet boolean whether to print status messages
#' @return gtfs_obj a gtfsr object with a bunch of simple features tables
#' @export
gtfs_as_sf <- function(gtfs_obj, quiet) {
  if(!quiet) message('Converting stops to simple features ')
  gtfs_obj$stops_sf <- try(get_stop_geometry(gtfs_obj$stops_df))
  if(!quiet) message('Converting routes to simple features ')
  gtfs_obj$routes_sf <- try(get_route_geometry(gtfs_obj))
  return(gtfs_obj)
}

#' Make Routes into Simple Features Lines
#'
#' @param gtfs_obj gtfsr object
#' @param route_ids select routes to convert to simple features
#' @param service_ids select service_ids to convert to simple features
#' @export
#' @return an sf dataframe for gtfs routes with a multilinestring column
#' @examples
#' data(gtfs_obj)
#' routes_sf <- get_route_geometry(gtfs_obj)
#' plot(routes_sf[1,])
get_route_geometry <- function(gtfs_obj, route_ids = NULL, service_ids = NULL) {
  shape_route_service_df <- shape_route_service(gtfs_obj, route_ids = route_ids, service_ids = service_ids)
  routes_latlong_df <- dplyr::inner_join(gtfs_obj$shapes_df,
                                         shape_route_service_df,
                                         by="shape_id")

  lines_df <- dplyr::distinct(routes_latlong_df, .data$route_id)
  lines_df <- lines_df[order(lines_df$route_id),]
  list_of_line_tibbles <- split(routes_latlong_df, routes_latlong_df$route_id)
  list_of_multilinestrings <- lapply(list_of_line_tibbles, shapes_df_as_sfg)

  lines_df$geometry <- sf::st_sfc(list_of_multilinestrings, crs = 4326)

  lines_sf <- sf::st_as_sf(lines_df)
  lines_sf$geometry <- sf::st_as_sfc(sf::st_as_text(lines_sf$geometry), crs=4326)
  return(lines_sf)
}

#' Make Stops into Simple Features Points
#'
#' @param stops_df a gtfsr$stops_df dataframe
#' @export
#' @return an sf dataframe for gtfs routes with a point column
#' @examples
#' data(gtfs_obj)
#' some_stops <- gtfs_obj$stops_df[sample(nrow(gtfs_obj$stops_df), 40),]
#' some_stops_sf <- get_stop_geometry(some_stops)
#' plot(some_stops_sf)
get_stop_geometry <- function(stops_df) {
  stops_sf <- sf::st_as_sf(stops_df,
                           coords = c("stop_lon", "stop_lat"),
                           crs = 4326)
  return(stops_sf)
}

#' return an sf linestring with lat and long from gtfs
#' @param df dataframe from the gtfsr shapes_df split() on shape_id
#' @noRd
#' @return st_linestring (sfr) object
shape_as_sf_linestring <- function(df) {
  # as suggested by www.github.com/mdsumner

  m <- as.matrix(df[order(df$shape_pt_sequence),
                    c("shape_pt_lon", "shape_pt_lat")])

  return(sf::st_linestring(m))
}

#' return an sf multilinestring with lat and long from gtfs for a route
#' @param df the shapes_df dataframe from a gtfsr object
#' @keywords internal
#' @return a multilinestring simple feature geometry (sfg) for the routes
shapes_df_as_sfg <- function(df) {
  # as suggested by www.github.com/mdsumner
  l_dfs <- split(df, df$shape_id)

  l_linestrings <- lapply(l_dfs,
                          shape_as_sf_linestring)

  return(sf::st_multilinestring(l_linestrings))
}

#' Buffer using common urban planner distances
#'
#' merges gtfsr objects
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

#' This function is deprecated. Please use get_stop_geometry
#' Make Stops into Simple Features Points
#'
#' @param stops_df a gtfsr$stops_df dataframe
#' @export
#' @return an sf dataframe for gtfs routes with a point column
#' @examples
#' data(gtfs_obj)
#' some_stops <- gtfs_obj$stops_df[sample(nrow(gtfs_obj$stops_df), 40),]
#' some_stops_sf <- stops_df_as_sf(some_stops)
#' plot(some_stops_sf)
stops_df_as_sf <- function(stops_df) {
  .Deprecated("stops_df_as_sf") #include a package argument, too
  get_stop_geometry(stops_df)
}

#' This function is deprecated. Please use get_route_geometry
#' Make Routes into Simple Features Lines
#'
#' @param gtfs_obj gtfsr object
#' @param route_ids select routes to convert to simple features
#' @param service_ids select service_ids to convert to simple features
#' @export
#' @return an sf dataframe for gtfs routes with a multilinestring column
#' @examples
#' data(gtfs_obj)
#' routes_sf <- routes_df_as_sf(gtfs_obj)
#' plot(routes_sf[1,])
routes_df_as_sf <- function(gtfs_obj, route_ids = NULL, service_ids = NULL) {
  .Deprecated("routes_df_as_sf") #include a package argument, too
  get_route_geometry(gtfs_obj, route_ids = NULL, service_ids = NULL)
}
