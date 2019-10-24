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
  dots <- list(...)
  if("sf" %in% class(x$stops)) {
    plot(x$stops)
  } else {
    x_stops <- stops_as_sf(x$stops)
    plot(x_stops)
  }
}

