#' Get and validate dataframes of General Transit Feed Specification (GTFS) data.
#' 
#' This function reads GTFS text files from a local or remote zip file. 
#' It also validates the files against the GTFS specification by file, requirement status, and column name
#' The data are returned as a list of dataframes and a validation object, 
#' which contains details on whether all required files were found, 
#' and which required and optional columns are present. 
#' 
#'
#' @param path Character. url link to zip file OR path to local zip file. if to local path, then option `local` must be set to TRUE.
#' @param local Boolean. If the paths are searching locally or not. Default is FALSE (that is, urls).
#' @param quiet Boolean. Whether to see file download progress and files extract. FALSE by default.
#'
#' @return A GTFS object. That is, a list of dataframes of GTFS data.
#'
#' @export
#' @importFrom dplyr %>% arrange summarise group_by inner_join
#' @examples \donttest{
#' library(dplyr)
#' u1 <- "https://github.com/r-transit/tidytransit/raw/master/inst/extdata/sample-feed-fixed.zip"
#' sample_gtfs <- read_gtfs(u1)
#' attach(sample_gtfs)
#' #list routes by the number of stops they have
#' routes %>% inner_join(trips, by="route_id") %>%
#'   inner_join(stop_times) %>% 
#'     inner_join(stops, by="stop_id") %>% 
#'       group_by(route_long_name) %>%
#'         summarise(stop_count=n_distinct(stop_id)) %>%
#'           arrange(desc(stop_count))
#' }
read_gtfs <- function(path, local = FALSE, 
                      quiet = TRUE) {
  # download zip file
  if (!local && valid_url(path)) {
    path <- download_from_url(url = path, quiet = quiet)
    if (is.null(path)) { 
      return() 
    }
  }
  # extract zip file
  tmpdirpath <- unzip_file(path, quiet=quiet)
  file_list_df <- zip::zip_list(path)
  if (!exists("file_list_df")) {
    stop(sprintf("No files found in zip"))
  }
  gtfs_obj <- create_gtfs_object(tmpdirpath, 
                                 file_list_df$filename, 
                                 quiet = quiet)
  return(gtfs_obj) 
}

#' Download a zipped GTFS feed file from a url
#'
#' @param url Character URL of GTFS feed.
#' @param path Character. Folder into which to put zipped file. If NULL, then save a tempfile
#' @param quiet Boolean. Whether to see file download progress. FALSE by default.
#'
#' @return File path
#' @importFrom dplyr %>%
#'
#' @keywords internal

download_from_url <- function(url,
                              path=tempfile(fileext = ".zip"),
                              quiet=FALSE) {
  
  stopifnot(length(url) == 1)
  
  # check if single element of dataframe 
  # was inputed. if so, convert to single value; error otherwise.
  if(!is.null(dim(url))) {
    if(all(dim(url) == c(1,1))) {
      url <- unlist(url, use.names = FALSE)
    } else {
      stop('Please input a single url.')
    }
  }

  r <- httr::GET(url)
  
  # Get gtfs zip if url can be reach
  if(httr::status_code(r) == 200) {
    check <- try(utils::download.file(url, path, quiet = quiet), silent=TRUE)
    if(check %>% assertthat::is.error()) {
      warn <- sprintf("Link '%s' failed to download. NULL was returned.", url)
      warning(warn)
      return(NULL)
    }
  } else {
    warn <- sprintf("Link '%s' cannot be reached. NULL was returned.", url)
    warning(warn)
    return(NULL)
  }
  
  # check path
  check <- try(normalizePath(path), silent = TRUE)
  if(assertthat::is.error(check)) {
    warn <- 'Invalid file path. NULL is returned.'
    warning(warn)
    return(NULL)
  }
  return(path)
}

#' Unzip a file and delete zip
#'
#' @param zipfile path to zipped file
#' @param tmpdirpath path to unzip file to-default tempdir()
#' @param quiet Boolean. Whether to output files found in folder.
#' @importFrom tools file_ext
#' 
#' @return file path to directory with gtfs .txt files
#' @keywords internal
#' 

unzip_file <- function(zipfile, 
                       tmpdirpath=tempdir(), 
                       quiet = FALSE) {
  f <- zipfile
  
  # check path
  if(try(path.expand(f), silent = TRUE) %>% assertthat::is.error()) {
    warn <- 'Invalid file path. NULL is returned.'
    warning(warn)
    return(NULL)
  }

  if(!file.exists(f) && !dir.exists(f)) {
    stop(paste0('"', f, '": No such file or directory'))
  }
  
  f <- normalizePath(f)

  if(tools::file_ext(f) != "zip") {
    if(!quiet) message('No zip file found, reading files from path.')
    return(f)
  }
  
  # create extraction folder
  utils::unzip(f, exdir=tmpdirpath)


  if(length(list.files(tmpdirpath)) == 0) {
    warn <- "No files found after decompressing. NULL is returned."
    return(NULL)
  }

  if(!quiet) {
    message(sprintf("Unzipped the following files to directory '%s'...", tmpdirpath))
    list.files(tmpdirpath) %>% print
  }

  return(tmpdirpath)
}

#' Function to read all files into dataframes
#'
#' @param file_path Character file path
#' @param assign_envir Environment Object. Option of where to assign dataframes.
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @noRd
#' @keywords internal

create_gtfs_object <- function(tmpdirpath, file_paths, quiet = FALSE) {
  prefixes <- vapply(file_paths,get_file_shortname,FUN.VALUE = "")
  df_names <- prefixes
  if(!quiet) message('Reading files in feed...\n')
  gtfs_obj <- lapply(file_paths, 
                   function(x) read_gtfs_file(x, 
                                              tmpdirpath, 
                                              quiet = quiet))
  names(gtfs_obj) <- unname(df_names)
  gtfs_obj[sapply(gtfs_obj, is.null)] <- NULL
  class(gtfs_obj) <- "gtfs"
  if(!quiet) message('Reading files in feed... done.\n')
  
    
  gtfs_obj <- gtfs_validate(gtfs_obj, quiet = quiet)
  
  stopifnot(is_gtfs_obj(gtfs_obj))
  
  if(!quiet) message("Reading gtfs feed completed.\n\n")
  
  return(gtfs_obj)
}


#' Function to read all files into dataframes
#'
#' @param file_path Character file path
#' @param tmpdirpath path for the tmpdir files
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @noRd
#' @keywords internal

read_gtfs_file <- function(file_path, tmpdirpath, quiet = FALSE) {
  prefix <- get_file_shortname(file_path)

  if(!quiet) message(paste0('Reading ', prefix))

  full_file_path <- paste0(tmpdirpath,"/",file_path)
  new_df <- parse_gtfs_file(prefix, full_file_path, quiet = quiet)

  return(new_df)
}

#' Function to get the gtfs table name from the file string
#'
#' @param file_path Character file path
#' @return df_name a character vector of the df_name for the file
#' @noRd
#' @keywords internal
#' 
get_file_shortname <- function(file_path) {
  split_path <- strsplit(file_path, '/')
  file_name <- split_path[[1]][length(split_path[[1]])]

  prefix <- gsub(".txt|-new", "", file_name) 
  # suffix ".*-new.txt" comes from trillium data
  prefix <- gsub("\\-|\\.", "_", prefix)
  return(prefix)
}

#' Parses one gtfs file
#'
#' @param prefix Character. gtfs file prefix (e.g. "agency", "stop_times", etc.)
#' @param file_path Character. file path
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @return Dataframe of parsed GTFS file.
#' @noRd
#' @keywords internal
#' @importFrom data.table fread

parse_gtfs_file <- function(prefix, file_path, quiet = FALSE) {

  # only parse if file has any data, NULL o/w
  stopifnot(!is.na(file.size(file_path)))
  if(file.size(file_path) > 1) {

    ## get correct meta data using file prefix (e.g. "agency", "stop_times")
    meta <- get_gtfs_meta()[[prefix]]

    # check if a file is empty. If so, return NULL.
    L <- suppressWarnings(
      length(
        scan(
          file_path, 
          what = "", 
          quiet = TRUE, 
          sep = "\n")
        )
      )
    if(L < 1) {
      s <- sprintf("   File '%s' is empty.", basename(file_path))
      if(!quiet) message(s)
      return(NULL)
    }

    #if no meta data is found for a file 
    #type but file is not empty, read as is.
    if(is.null(meta)) {
      s <- sprintf("File %s not recognized, 
                   trying to read file as csv.", 
                   basename(file_path))
      if(!quiet) message(s)

      tryCatch({
        df <- suppressMessages(
          data.table::fread(file = file_path, 
                            sep=","))
        }, 
        error = function(error_condition) {
          s <- sprintf("   File could not be read as csv.", basename(file_path))
          if(!quiet) message(s)
          return(NULL)
        })
      return(df)
    }

    # get a small df to find how many cols are needed
    small_df <- suppressWarnings(
      readr::read_csv(file_path, n_max = 1, col_types = readr::cols(.default = "c")))
    
    # get correct coltype, if possible
    # create "c" as coltype defaults
    coltypes_character <- rep("c", 
                              dim(small_df)[2]) 

    names(coltypes_character) <- 
      names(small_df) %>% tolower()
    # indx from valid cols in meta$field. NAs will return for invalid cols
    indx <- match(names(coltypes_character), meta$field)  

    #!is.na(indx) = valid col in 'coltype' found in meta$field
    #indx[!is.na(indx)] = location in 'meta$coltype' 
    #where corresponding type is found
    #valid cols found in small_df
    coltypes_character[!is.na(indx)] <- 
      meta$coltype[indx[!is.na(indx)]] 

    # use col_*() notation for column types
    coltypes <-
      sapply(
        coltypes_character,
        switch,
        "c" = readr::col_character(),
        "i" = readr::col_integer(),
        "d" = readr::col_double(),
        "D" = readr::col_date(format = "%Y%m%d")
      )
    
    df <- suppressWarnings(
      readr::read_csv(file = file_path, 
                      col_types = coltypes
      )
    )
    probs <- readr::problems(df)
    
    if(dim(probs)[1] > 0) {
      attributes(df) <- append(attributes(df), list(problems = probs))
      warning(paste0("Parsing failures while reading ", prefix))
      print(probs)
    }
    
    return(df)
  } else return(NULL)
}
