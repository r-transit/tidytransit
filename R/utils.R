#' Returns TRUE if the given gtfs_obj contains the table in tidytransit's "calculated 
#' tables sublist" (\code{gtfs_obj$.})
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @param table_name name of the table to look for, as string
#' @keywords internal
feed_contains. <- function(gtfs_obj, table_name) {
    (exists(".", where = gtfs_obj) && exists(table_name, where = gtfs_obj$.))
}

feed_contains <- function(gtfs_obj, table_name) {
  exists(table_name, where = gtfs_obj)
}

feed_has_non_empty_table <- function(gtfs_obj, table_name) {
  if(exists(table_name, where = gtfs_obj)) {
    if(is.data.frame(gtfs_obj[[table_name]]) && nrow(gtfs_obj[[table_name]]) > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Convert empty strings ("") to NA values in all gtfs tables
#' 
#' [read_gtfs()] converts all empty strings to `NA` values
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#'  
#' @return a gtfs_obj where all empty strings in tables have been replaced with NA
#' @seealso [na_to_empty_strings()]
#' @export
empty_strings_to_na = function(gtfs_obj) {
  tbl_names = names(gtfs_obj)
  non_txt = names(gtfs_reference_filetype[gtfs_reference_filetype != "txt"])
  tbl_names <- setdiff(tbl_names, c(".", non_txt))
  for(tbl in tbl_names) {
    if(inherits(gtfs_obj[[tbl]], "data.frame")) {
      gtfs_obj[[tbl]][gtfs_obj[[tbl]] == ""] <- NA
    }
  }
  gtfs_obj
}

#' Convert NA values to empty strings ("")
#' 
#' [write_gtfs()] converts `NA` to empty strings
#'  
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @return a gtfs_obj where all NA strings in tables have been replaced with ""
#' @seealso [empty_strings_to_na()]
#' @export
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

# 10x faster than tidyr::gather for special usage in interpolate_stop_times
gather_dt = function(df_wide, new_key_colname, new_val_colname,
                     value_colnames) {
  dt = as.data.table(df_wide)
  dt_melted = data.table::melt(dt, measure.vars = value_colnames,
                               variable.name = new_key_colname, value.name = new_val_colname)
  
  return(dt_melted)
}

# 5x faster than tidyr::spread for special usage in interpolate_stop_times
spread_dt = function(df_long, key_colname, value_colname) {
  dt = data.table::as.data.table(df_long)
  setnames(dt, key_colname, "....key....")
  dcast(dt, ... ~ ....key...., value.var = value_colname)
}
