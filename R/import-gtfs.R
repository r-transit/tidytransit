#' Get a Dataframes of GTFS data.
#'
#' @param urls Character. url link to zip file
#' @param quiet Boolean. Whether to see file download progress and files extract. FALSE by default.
#'
#' @return Dataframes of GTFS data.
#'
#' @export

import_gtfs <- function(urls, quiet = FALSE) {

  feed_flow <- function(url) {

    path <- get_feed(url = url, quiet = quiet)

    if(is.null(path)) return(NULL) else {
      zip_dir <- unzip_gtfs_files(zipfile = path, quiet = quiet)
    }

    read_gtfs(zip_dir, quiet = quiet)
  }

  # check if single column of data was inputed. if so, convert to vector; error otherwise.
  if(!is.null(dim(urls))) {
    if(dim(urls)[2] == 1) {
      urls <- unlist(urls, use.names = FALSE)
    } else {
      stop('Please input a vector or single column of data.')
    }
  }

  data_list <- urls %>% lapply(. %>% feed_flow)
  return(data_list)

}