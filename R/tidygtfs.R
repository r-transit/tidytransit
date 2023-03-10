#' Convert another gtfs like object to a tidygtfs object
#' @param x gtfs object
#' @param ... ignored
#' @return a tidygtfs object
#' @export
as_tidygtfs = function(x, ...) {
  UseMethod("as_tidygtfs")
}

#' @export
as_tidygtfs.list = function(x, ...) {
  x <- convert_list_tables_to_data.tables(x, ...)
  gtfs_to_tidygtfs(x, ...)
}

#' @export
as_tidygtfs.gtfs = function(x, ...) {
  gtfs_to_tidygtfs(x, ...)
}

#' @export
as_tidygtfs.dt_gtfs = function(x, ...) {
  gtfs_to_tidygtfs(x)
}

#' @export
as_tidygtfs.tidygtfs = function(x, ...) {
  x <- convert_list_tables_to_data.tables(x, ...)
  gtfs_to_tidygtfs(x, ...)
}

#' Convert an object created by gtfsio::import_gtfs to a tidygtfs object
#' 
#' Some basic validation is done to ensure the feed works in tidytransit
#' 
#' @param gtfs_list list of tables
#' @param files subset of files to validate
gtfs_to_tidygtfs = function(gtfs_list, files = NULL) {
  # validate files and fields
  validation_result = validate_gtfs(gtfs_list, files = files)
  
  # check unique ids
  tbl_with_duplicated_ids = duplicated_primary_keys(gtfs_list)
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
  
  # gtfs class base structure
  x <- gtfsio::new_gtfs(x)
  class(x) <- c("tidygtfs", "gtfs", "list")
  attributes(x)$validation_result <- validation_result
  
  return(x)
}

prepare_tidygtfs_fields = function(gtfs_obj) {
  gtfs_obj <- convert_times_to_hms(gtfs_obj)
  gtfs_obj <- convert_dates(gtfs_obj)
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

convert_list_tables_to_data.tables = function(gtfs_list) {
  gtfs_list$. <- NULL
  gtfs_list <- lapply(gtfs_list, data.table::as.data.table)
  return(gtfs_list)
}
