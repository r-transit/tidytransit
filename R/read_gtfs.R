# TODO
#' Read and validate GTFS files
#'
#' Reads GTFS text files from either a local \code{.zip} file or an URL and
#' validates them against GTFS specifications.
#'
#' @param path The path to a GTFS \code{.zip} file.
#' @param files A character vector containing the text files to be read from the
#'   GTFS (without the \code{.txt} extension). If \code{NULL} (the default) all
#'   existing files are read.
#' @param quiet Whether to hide log messages and progress bars (defaults to TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#' @param parse_dates date columns in calendar.txt, calendar_dates.txt and feed_info.txt
#'   are converted to a Date object.
#' @return A GTFS object: a list of data.tables in which each index represents a
#'   GTFS text file.
#'
#' @seealso \code{\link{validate_gtfs}}
#'
#' @examples
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' gtfs <- read_gtfs(local_gtfs_path)
#' names(gtfs)
#'
#' gtfs <- read_gtfs(local_gtfs_path, files = c("trips", "stop_times"))
#' names(gtfs)
#' @importFrom gtfsio import_gtfs
#' @export
read_gtfs <- function(path, files = NULL, quiet = TRUE) {
  g = gtfsio::import_gtfs(path, files = NULL, quiet = quiet)
  g <- lapply(g, dplyr::as_tibble)
  g <- set_dates(g)
  g <- set_date_service_table(g)
  g <- set_hms_times(g)

  gtfsio::new_gtfs(g)
}
