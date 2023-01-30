as_tidygtfs = function(x, ...) {
  UseMethod("as_tidygtfs")
}

as_tidygtfs.gtfs = function(x, ...) {
  gtfsio_to_tidygtfs(x, ...)
}

as_tidygtfs.gtfs_dt = function(x, ...) {
  # convert from gtfs
}

as_tidygtfs.tidygtfs = function(x, ...) {
  # convert from tidygtfs
}

gtfsio_to_tidygtfs = function(gtfs_list, files = NULL) {
  # validate
  validation_result <- validate_gtfs(gtfs_list, files = files)
  
  # prep tidygtfs columns
  x = gtfs_list
  x$. <- list()
  x <- convert_times_to_hms(x)
  x <- convert_dates(x)
  x <- set_dates_services(x)
  
  # convert to tibble
  x[names(x) != "."] <- lapply(x[names(x) != "."], dplyr::as_tibble)
  x <- gtfsio::new_gtfs(x)
  class(x) <- c("tidygtfs", "gtfs")
  attributes(x)$validation_result <- validation_result
  
  return(x)
}