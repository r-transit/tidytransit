#' Convert a tidygtfs object to a gtfs object (for gtfsio)
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @return gtfs list
#' @keywords internal
tidygtfs_to_gtfs = function(gtfs_obj) {
  # convert sf tables
  gtfs_obj <- sf_as_tbl(gtfs_obj)
  gtfs_obj <- sf_as_json(gtfs_obj)
  
  # convert NA to empty strings
  gtfs_obj <- na_to_empty_strings(gtfs_obj)
  
  # data.tables
  gtfs_obj <- gtfs_obj[names(gtfs_obj) != "."]
  gtfs_obj <- convert_list_tables_to_data.tables(gtfs_obj)
  class(gtfs_obj) <- list("gtfs")
  
  # convert dates/times to strings
  gtfs_obj <- convert_date_to_char(gtfs_obj)
  gtfs_obj <- convert_hms_to_char(gtfs_obj)
  
  return(gtfs_obj)
}
