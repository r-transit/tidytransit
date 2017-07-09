wgs84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#' return an sf linestring with lat and long from gtfs
#' @param df dataframe from the gtfsr shapes_df split() on shape_id
#' @return st_linestring (sfr) object
gtfs_shape_to_sf_linestring <- function(df) {
  # as suggested by www.github.com/mdsumner

  m <- as.matrix(df[order(df$shape_pt_sequence), c("shape_pt_lon", "shape_pt_lat")])

  return(sf::st_linestring(m))
}

#' return an sf multilinestring with lat and long from gtfs for a route
#' @param df dataframe with the shapes for a given route
#' @return multilinestring (sfr) object
gtfs_route_to_sf_multilinestring <- function(df) {
  # as suggested by www.github.com/mdsumner
  l_dfs <- split(df, df$shape_id)

  l_linestrings <- lapply(l_dfs, gtfs_shape_to_sf_linestring)

  return(sf::st_multilinestring(l_linestrings))
}

#' return an sf dataframe with from a gtfs object
#' @param gtfs_obj gtfsr object
#' @return an sf dataframe for gtfs routes with a multilinestring column
gtfs_routes_as_sf <- function(gtfs_obj) {

  srs_id_df <- join_shape_route_service_ids(gtfs_obj)
  srs_id_shapes_df <- dplyr::inner_join(gtfs_obj$shapes_df,srs_id_df,by="shape_id")

  sf_lines <- dplyr::distinct(srs_id_shapes_df, route_id)

  l_dfs <- split(srs_id_shapes_df, srs_id_shapes_df$route_id)

  l_mlinestrings <- lapply(l_dfs, gtfs_route_to_sf_multilinestring)

  sf_lines[["geometry"]] <- sf::st_sfc(l_mlinestrings, crs = wgs84_crs)

  sf_lines <- sf::st_as_sf(sf_lines)

  return(sf_lines)
}