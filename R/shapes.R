#' Get a SpatialLinesDataFrame for the shapes_df in a GTFSr object
#' @param gtfs_obj is a gtfsr list of gtfs dataframes
#' @return SpatialLinesDataFrame for the shapes_df 
gtfs_shapes_as_sp_lines <- function(gtfs_obj, shape_id_filter=NULL) {
  #as suggested by mdsumner here: https://github.com/ropensci/gtfsr/issues/24
  library(spbabel)
  sp_line <- sp(transmute(gtfs_obj$shapes_df, object_ = shape_id, branch_ = shape_id, order_ = shape_pt_sequence, x_ = shape_pt_lon, y_ = shape_pt_lat), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sp_line$shape_id <- unique(gtfs_obj$shapes_df$shape_id)
  sp_line$rownumber_ <- NULL
  return(sp_line)
}

#' turn a list of tibbles with lat and long from gtfs into st_linestrings
#' @param a list of tibbles/dataframes from the gtfsr shapes_df split() on shape_id
#' @returns st_linestring (sfr) objects by shape_id
make_gtfs_linestring_list <- function(l1) {
  m1 <- as.matrix(x[order(x$shape_pt_sequence), 
                  c("shape_pt_lon", "shape_pt_lat")])
  st_linestring(m)
}


#' make a simple features object from gtfs shapes
#' @param a gtfs shapes_df
#' @return an sf (simple features) object for the gtfs shapes
gtfs_shapes_as_sf_lines <-function(gtfs_obj, shape_id_filter=NULL) {
  #as suggested by mdsumner here: https://github.com/ropensci/gtfsr/issues/24
  library(sf)
  library(dplyr)
  sf_line <- distinct(gtfs_obj$shapes_df, shape_id)
  l_dfs <- split(gtfs_obj$shapes_df, 
                 gtfs_obj$shapes_df$shape_id)
  l_linestrings <- lapply(l_dfs, make_gtfs_linestring_list)
  wgs84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  sf_line[["geometry"]] <- st_sfc(l_linestrings, crs = wgs84_crs)
  sf_line <- st_as_sf(sf_line)
}


gtfs_route_to_sf_multiline <- function(shapes_df_sl,df1,service_id,route_id) {
  shpids <- df1[df1$service_id == service_id & 
                 df1$route_id==route_id,]$shape_id
  sfc1 <- shapes_df_sl[shapes_df_sl$shape_id %in% shpids,]
  g1 <- st_geometry(sfc1)
  st_mls1 <- st_multilinestring(g1)
  return(st_mls1)
}
