#' Plot GTFS stops and trips
#'
#' @param x a gtfs_obj as read by read_gtfs()
#' @param ... further specifications
#' @return plot
#'
#' @examples \donttest{
#' local_gtfs_path <- system.file("extdata",
#'                               "google_transit_nyc_subway.zip",
#'                               package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path)
#' plot(nyc)
#' }
#' 
#' @importFrom graphics plot
#' @importFrom dplyr select
#' @export
plot.tidygtfs <- function(x, ...) {
  dots <- list(...)
  if(!feed_contains(x, "stops")) {
    stop("Feed doesn't contain a stops table")
  }
  x_stops <- x$stops 
  if(!"sf" %in% class(x$stops))  x_stops <- stops_as_sf(x$stops)

  if("sf" %in% class(x$shapes)) {
    plot(x$shapes["shape_id"], reset = FALSE, main = agency_info(x))
    plot(x_stops[,"stop_id"], add = TRUE)
  } else {
    plot(x_stops[,"stop_id"], main = agency_info(x))
  }
}
