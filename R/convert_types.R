#' Convert columns between gtfsio types to tidytransit types according to GTFS reference
#' 
#' @param gtfs_list gtfs object
#' @param conversion_table data.frame containing a column `file` and `Field_Name`, generally 
#'                         from internal `gtfs_reference_types` dataset
#' @param conversion_function function to convert columns
#'
#' @return gtfs_list with converted (overwritten) columns in tables
#' @keywords internal
convert_types <- function(gtfs_list, conversion_table, conversion_function) {
  for(i in seq_len(nrow(conversion_table))) {
    file = conversion_table$file[i]
    field_name = conversion_table$Field_Name[i]
    if(feed_contains(gtfs_list, file)) {
      if(!is.null(gtfs_list[[file]][[field_name]])) {
        stopifnot(inherits(gtfs_list[[file]], "data.table"))
        gtfs_list[[file]][, c(field_name) := conversion_function(get(field_name))]
      }
    }
  }
  return(gtfs_list)
}

convert_char_to_date <- function(gtfs_list) {
  convert_types(gtfs_list, gtfs_reference_types$Date, .parse_gtfsio_date)
}

convert_date_to_char <- function(gtfs_obj) {
  convert_types(gtfs_obj, gtfs_reference_types$Date, .date_as_gtfsio_char)
}

convert_char_to_hms <- function(gtfs_list) {
  convert_types(gtfs_list, gtfs_reference_types$Time, hhmmss_to_hms)
}

convert_hms_to_char <- function(gtfs_obj) {
  convert_types(gtfs_obj, gtfs_reference_types$Time, hms_to_hhmmss)
}
