#' Get a Dataframes of GTFS data.
#'
#' @param paths Character. url links to zip files OR paths to local zip files. if to local path, then option `local` must be set to TRUE.
#' @param local Boolean. If the paths are searching locally or not. Default is FALSE (that is, urls).
#' @param quiet Boolean. Whether to see file download progress and files extract. FALSE by default.
#'
#' @return Dataframes of GTFS data.
#'
#' @export

import_gtfs <- function(paths, local = FALSE, quiet = FALSE) {

  feed_flow <- function(url) {

    path <- get_feed(url = url, quiet = quiet)

    # check path
    check <- try(normalizePath(path), silent = TRUE)
    if(assertthat::is.error(check)) {
      warn <- 'Invalid file path. NULL is returned.'
      if(!quiet) warning(warn)
      return(NULL)
    }

    zip_dir <- unzip_gtfs_files(zipfile = path, quiet = quiet)

    try(read_gtfs(zip_dir, quiet = quiet))
  }

  # check if single column of data was inputed. if so, convert to vector; error otherwise.
  if(!is.null(dim(paths))) {
    if(dim(paths)[2] == 1) {
      paths <- unlist(paths, use.names = FALSE)
    } else {
      stop('Please input a vector or single column of data.')
    }
  }

  if(local) {
    paths <- paths %>% sapply(. %>% normalizePath)
    data_list <- paths %>% lapply(. %>% unzip_gtfs_files(quiet=quiet) %>% read_gtfs(quiet=quiet))
  } else {
    data_list <- paths %>% lapply(. %>% feed_flow)
  }

  # show note, which is suppressed in read_gtfs
  if(!quiet) {
    message("\n")
    message('NOTE: Parsing errors and warnings while importing data can be extracted from any given dataframe with `attr(df, "problems")`.')
    message("\n")
  }

  if(length(data_list) > 1) return(data_list) else data_list[[1]]

}