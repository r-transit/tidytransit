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
  
  # check unique ids
  tbl_with_duplicated_ids = duplicated_unique_ids(gtfs_list)
  if(any(tbl_with_duplicated_ids)) {
    warning("Duplicated ids found in: ", 
            paste0(names(tbl_with_duplicated_ids[tbl_with_duplicated_ids]), collapse = ", "), "\n",
            "The returned object is not a tidygtfs object, you can use as_tidygtfs() after fixing the issue.")
    return(gtfs_list)
  }
  
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

duplicated_unique_ids = function(gtfs_list) {
  vapply(names(gtfs_list), function(tbl_name) {
    if(tbl_name %in% names(gtfs_meta)) {
      id_field = gtfs_meta[[tbl_name]]$required_unique_id
      if(!is.na(id_field)) {
        return(any(duplicated(gtfs_list[[tbl_name]][[id_field]])))
      }
    }
    return(FALSE)
  }, logical(1))
}
