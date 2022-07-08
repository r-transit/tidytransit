#' Returns TRUE if the given gtfs_obj contains the table. Used to check for
#' tidytransit's calculated tables in sublist
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param table_name name as string of the table to look for
feed_contains <- function(gtfs_obj, table_name) {
  exists(table_name, where = gtfs_obj) ||
    (exists(".", where = gtfs_obj) && exists(table_name, where = gtfs_obj$.))
}

#' Convert empty strings ("") to NA values in gtfs tables
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#'  
#' @return a gtfs_obj where all empty strings in tables have been replaced with NA
#' 
#' @export
empty_strings_to_na = function(gtfs_obj) {
  tbl_names = names(gtfs_obj)
  tbl_names <- tbl_names[tbl_names != "."]
  for(tbl in tbl_names) {
    if(inherits(gtfs_obj[[tbl]], "data.frame")) {
      gtfs_obj[[tbl]][gtfs_obj[[tbl]] == ""] <- NA
    }
  }
  gtfs_obj
}

#' Convert NA values to empty strings ("")
#'  
#' @param gtfs_obj gtfs feed (tidygtfs object)
na_to_empty_strings = function(gtfs_obj) {
  lapply(gtfs_obj, function(df) {
    if(inherits(df, "data.frame")) {
      df2 = lapply(df, function(.col) {
        if(is.character(.col)) {
          .col[is.na(.col)] <- ""
        }
        .col
      })
      attributes(df2) <- attributes(df)
      df <- df2
    } else if(inherits(df, "list")) {
      df <- na_to_empty_strings(df)
    }
    df
  })
}
