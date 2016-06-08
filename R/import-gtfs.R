#' Get a Dataframes of GTFS data.
#'
#' @param path Character. url link to zip file
#'
#' @return Dataframes of GTFS data.
#'
#' @export

import_gtfs <- function(urls, timeout=10) {

  feed_flow <- function(url) {
    path <- get_feed(url = url)

    zip_dir <- unzip_gtfs(path)

    read_gtfs(zip_dir)

  }


  old_timeout <- options()$timeout
  options(timeout=timeout)

  # check if single column of data was inputed. if so, convert to vector; error otherwise.
  if(!is.null(dim(urls))) {
    if(dim(urls)[2] == 1) {
      urls <- unlist(urls, use.names = FALSE)
    } else {
      stop('Please input a vector or single column of data.')
    }
  }

  data_list <- urls %>% lapply(. %>% feed_flow)
  options(timeout=old_timeout)
  return(data_list)

}