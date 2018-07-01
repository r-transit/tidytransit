#' Merge gtfsr data frames across gtfsr objects
#' 
#' merges gtfsr objects
#' @param gtfs_obj_list a list of standard gtfsr objects
#' @param dfname dataframe to return
#' @return one gtfsr dataframe object
#' @export
join_gtfsr_dfs <- function(gtfs_obj_list,dfname) {
  l_dfs <- lapply(gtfs_obj_list, 
                            FUN=function(x) {
                              try(add_agency_columns_to_df(x,dfname))}
  )
  is.df.obj <- function(x) inherits(x, "data.frame")
  processed_success <- vapply(l_dfs, is.df.obj, logical(1))
  df_bound <- do.call("rbind", l_dfs[processed_success])
  return(df_bound)
}

#' Adds columns to a gtfsr data frame with the agency id and name
#' 
#' adds agency details to a gtfsr dataframe
#' @param gtfs_obj a list of standard gtfsr objects
#' @param dfname dataframe to return
#' @keywords internal
#' @return select data frame with the agency id and name
join_agency_columns_to_df <- function(gtfs_obj,dfname) {
  agency_id <- gtfs_obj$agency_df$agency_id
  agency_name <- gtfs_obj$agency_df$agency_name
  df1 <- gtfs_obj[[dfname]]
  if(has_service(df1)){
    df1$agency_id <- agency_id
    df1$agency_name <- agency_name
  }
  return(df1)
}