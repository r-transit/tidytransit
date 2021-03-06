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
#' @return A tidygtfs object: a list of tibbles in which each entry represents a
#'   GTFS text file. Additional tables are stored in the \code{.} sublist.
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
#' @importFrom gtfsio import_gtfs new_gtfs
#' @export
read_gtfs <- function(path, files = NULL, quiet = TRUE) {
  g = gtfsio::import_gtfs(path, files = NULL, quiet = quiet)
  
  # validate
  validation_result <- validate_gtfs(g)
  
  # prep tidygtfs columns
  g$. <- list()
  g <- convert_times_to_hms(g)
  g <- convert_dates(g)
  g <- set_dates_services(g)
  
  # convert to tibble
  g[names(g) != "."] <- lapply(g[names(g) != "."], dplyr::as_tibble)
  g <- gtfsio::new_gtfs(g)
  class(g) <- c("tidygtfs", "gtfs")
  attributes(g)$validation_result <- validation_result
    
  g
}
