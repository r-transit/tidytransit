#' Convert another gtfs like object to a tidygtfs object
#' @param x gtfs object
#' @param ... ignored
#' @export
as_tidygtfs = function(x, ...) {
  UseMethod("as_tidygtfs")
}

#' @export
as_tidygtfs.list = function(x, ...) {
  gtfsio_to_tidygtfs(x, ...)
}

#' @export
as_tidygtfs.gtfs = function(x, ...) {
  gtfsio_to_tidygtfs(x, ...)
}

#' @export
as_tidygtfs.dt_gtfs = function(x, ...) {
  x <- prepare_tidygtfs_fields(x, convert_dates = FALSE)
  x <- prepare_tidygtfs_tables(x)
  x <- convert_list_tables_to_tibbles(x)
  return(x)
}

#' @export
as_tidygtfs.tidygtfs = function(x, ...) {
  x <- set_dates_services(x)
  attributes(x)$validation_result <- validate_gtfs(x)
  class(x) <- c("tidygtfs", "gtfs")
  return(x)
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
  x = prepare_tidygtfs_fields(gtfs_list)
  
  # add tidygtfs tables
  x <- prepare_tidygtfs_tables(x)
  
  # convert to tibbles
  x <- convert_list_tables_to_tibbles(x)
  x <- gtfsio::new_gtfs(x)
  class(x) <- c("tidygtfs", "gtfs")
  attributes(x)$validation_result <- validation_result
  
  return(x)
}

prepare_tidygtfs_fields = function(gtfs_obj, convert_times = TRUE, convert_dates = TRUE) {
  if(convert_times) gtfs_obj <- convert_times_to_hms(gtfs_obj)
  if(convert_dates) gtfs_obj <- convert_dates(gtfs_obj)
  return(gtfs_obj)
}

prepare_tidygtfs_tables = function(gtfs_obj) {
  gtfs_obj$. <- list()
  gtfs_obj <- set_dates_services(gtfs_obj)
  return(gtfs_obj)
}


convert_list_tables_to_tibbles = function(gtfs_list) {
  gtfs_list[names(gtfs_list) != "."] <- lapply(gtfs_list[names(gtfs_list) != "."], dplyr::as_tibble)
  return(gtfs_list)
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
