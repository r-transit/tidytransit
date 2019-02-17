#' Plot GTFS object routes and their frequencies
#'
#' @param x a gtfs_obj as read by read_gtfs()
#' @export
#' @param ... further specifications
#' @importFrom graphics plot
#' @importFrom dplyr select
#' @examples \donttest{
#' local_gtfs_path <- system.file("extdata", 
#'                               "google_transit_nyc_subway.zip", 
#'                               package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path, 
#'                 local=TRUE)
#' plot(nyc)
#' }
plot.gtfs <- function(x, ...) {
  dots = list(...)
  if(exists("x$routes_sf") & exists("x$routes_frequency_df")){
    routes_sf_frequencies <- x$routes_sf %>% 
      dplyr::inner_join(x$routes_frequency_df, by = "route_id") %>% 
      dplyr::select(median_headways, 
                     mean_headways, 
                     st_dev_headways, 
                     stop_count)
  } else {
    message('Calculating headways and spatial features. This may take a while')
      x$routes_sf <- get_route_geometry(x)
      x <- get_route_frequency(x)
      routes_sf_frequencies <- x$routes_sf %>% 
        dplyr::inner_join(x$routes_frequency_df, by = "route_id") %>% 
        dplyr::select(median_headways, 
                      mean_headways, 
                      st_dev_headways, 
                      stop_count)
  }
  plot(routes_sf_frequencies)
}

