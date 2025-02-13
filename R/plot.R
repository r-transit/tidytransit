#' Plot GTFS stops and trips
#'
#' @param x a tidygtfs object as read by [read_gtfs()]
#' @param ... ignored for tidygtfs
#' @return plot
#'
#' @examples \donttest{
#' local_gtfs_path <- system.file("extdata",
#'                               "nyc_subway.zip",
#'                               package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path)
#' plot(nyc)
#' }
#' 
#' @importFrom graphics plot
#' @export
plot.tidygtfs <- function(x, ...) {
  dots <- list(...)
  if(!feed_contains(x, "stops")) {
    stop("Feed doesn't contain a stops table")
  }
  x_stops <- x$stops 
  if(!inherits(x$stops, "sf"))  x_stops <- stops_as_sf(x$stops)

  if(inherits(x$shapes, "sf")) {
    plot(x$shapes["shape_id"], reset = FALSE, main = agency_info(x))
    plot(x_stops[,"stop_id"], add = TRUE)
  } else {
    plot(x_stops[,"stop_id"], main = agency_info(x))
  }
}
