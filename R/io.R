#' Read and validate GTFS files
#'
#' Reads a GTFS feed from either a local `.zip` file or an URL and validates them against 
#' GTFS specifications.
#'
#' @param path The path to a GTFS \code{.zip} file.
#' @param files A character vector containing the text files to be validated against the GTFS
#'   specification without the file extension (`txt` or `geojson`). If `NULL` (the default),
#'   all existing files are read.
#' @param quiet Whether to hide log messages and progress bars (defaults to TRUE).
#' @param ... Can be used to pass on arguments to [gtfsio::import_gtfs()]. The parameters
#'   `files` and `quiet` are passed on by default.
#'
#' @return A tidygtfs object: a list of tibbles in which each entry represents a GTFS text
#'   file. Additional tables are stored in the \code{.} sublist.
#'
#' @seealso [validate_gtfs()], [write_gtfs()]
#'
#' @examples \dontrun{
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' gtfs <- read_gtfs(local_gtfs_path)
#' summary(gtfs)
#'
#' gtfs <- read_gtfs(local_gtfs_path, files = c("trips", "stop_times"))
#' names(gtfs)
#' }
#' @importFrom gtfsio import_gtfs new_gtfs
#' @export
read_gtfs <- function(path, files = NULL, quiet = TRUE, ...) {
  g = gtfsio::import_gtfs(path, files = files, quiet = quiet, ...)
  
  tidygtfs = gtfs_to_tidygtfs(g, files = files)
  
  return(tidygtfs)
}

#' Write a tidygtfs object to a zip file
#'
#' @note Auxiliary tidytransit tables (e.g. \code{dates_services}) are not exported. Calls
#'   [gtfsio::export_gtfs()] after preparing the data.
#'
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param zipfile path to the zip file the feed should be written to. The file is overwritten
#'   if it already exists.
#' @param compression_level a number between 1 and 9, defaults to 9 (best compression).
#' @param as_dir if `TRUE`, the feed is not zipped and zipfile is used as a directory path. 
#'   The directory will be overwritten if it already exists.
#' @return Invisibly returns `gtfs_obj`
#'
#' @seealso [read_gtfs()]
#'
#' @importFrom gtfsio export_gtfs
#' @export
write_gtfs <- function(gtfs_obj, zipfile, compression_level = 9, as_dir = FALSE) {
  stopifnot(inherits(gtfs_obj, "tidygtfs"))

  gtfs_out = tidygtfs_to_gtfs(gtfs_obj)
  
  # export with gtfsio
  gtfsio::export_gtfs(gtfs_out, zipfile, 
                      standard_only = FALSE,
                      compression_level = compression_level, 
                      as_dir = as_dir, overwrite = TRUE)
  invisible(gtfs_obj)
}
