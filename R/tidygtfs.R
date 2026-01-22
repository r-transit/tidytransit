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
  gtfs_to_tidygtfs(x, ...)
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
#' @keywords internal
gtfs_to_tidygtfs = function(gtfs_list, files = NULL) {
  # validate files and fields
  validation_result = validate_gtfs(gtfs_list, files = files)
  
  # check unique ids
  tbl_with_duplicated_ids = duplicated_primary_keys(gtfs_list)
  if(any(tbl_with_duplicated_ids)) {
    warning("Duplicated ids found in: ", 
            paste(names(tbl_with_duplicated_ids[tbl_with_duplicated_ids]), collapse = ", "), "\n",
            "The returned object is not a tidygtfs object, you can use as_tidygtfs() after fixing the issue.")
    return(gtfs_list)
  }
  
  # prep tidygtfs columns
  x = prepare_tidygtfs_fields(gtfs_list)
  
  # add tidygtfs tables
  x <- prepare_tidygtfs_tables(x)
  
  # convert to tibbles/geojson
  x <- convert_list_tables_to_tibbles(x)
  x <- convert_list_json_to_sf(x)
  
  # convert empty strings "" to NA
  x <- empty_strings_to_na(x)
  
  # gtfs class base structure
  x <- gtfsio::new_gtfs(x)
  class(x) <- c("tidygtfs", "gtfs", "list")
  attributes(x)$validation_result <- validation_result
  
  return(x)
}

prepare_tidygtfs_fields = function(gtfs_obj) {
  gtfs_obj <- convert_char_to_hms(gtfs_obj)
  gtfs_obj <- convert_char_to_date(gtfs_obj)
  return(gtfs_obj)
}

prepare_tidygtfs_tables = function(gtfs_obj) {
  gtfs_obj$. <- list()
  gtfs_obj <- set_dates_services(gtfs_obj)
  return(gtfs_obj)
}

# convert tables ####
convert_list_tables_to_tibbles = function(gtfs_list) {
  table_index = .is_table(gtfs_list)
  gtfs_list[table_index] <- lapply(gtfs_list[table_index], dplyr::as_tibble)
  return(gtfs_list)
}

convert_list_tables_to_data.tables = function(gtfs_list) {
  table_index = .is_table(gtfs_list)
  gtfs_list[table_index] <- lapply(gtfs_list[table_index], data.table::as.data.table)
  return(gtfs_list)
}

.is_table = function(gtfs_list) {
  stopifnot(inherits(gtfs_list, "list"))
  unlist(lapply(gtfs_list, is.data.frame)) & !unlist(lapply(gtfs_list, inherits, "sf"))
}

convert_list_json_to_sf = function(gtfs_list) {
  stopifnot(inherits(gtfs_list, "list"))
  json_index = is_geojson(names(gtfs_list))
  gtfs_list[json_index] <- lapply(gtfs_list[json_index], json_to_sf)
  return(gtfs_list)
}

is_geojson = function(gtfs_table_name) {
  stopifnot(is.character(gtfs_table_name))
  isjson = (gtfs_reference_filetype[gtfs_table_name] == "geojson") %in% TRUE
  names(isjson) <- gtfs_table_name
  return(isjson)
}
