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
#' @param geometry Boolean. Whether to add simple feature dataframes of routes and stops to the gtfs object
#' @param frequency Boolean. Whether to add frequency/headway calculations to the gtfs object
#'
#' @return A GTFS object. That is, a list of dataframes of GTFS data.
#'
#' @export
#' @importFrom dplyr %>% arrange summarise group_by inner_join
#' @examples \donttest{
#' library(dplyr)
#' u1 <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
#' sample_gtfs <- read_gtfs(u1)
#' attach(sample_gtfs)
#' #list routes by the number of stops they have
#' routes_df %>% inner_join(trips_df, by="route_id") %>%
#'   inner_join(stop_times_df) %>% 
#'     inner_join(stops_df, by="stop_id") %>% 
#'       group_by(route_long_name) %>%
#'         summarise(stop_count=n_distinct(stop_id)) %>%
#'           arrange(desc(stop_count))
#' }
read_gtfs <- function(path, local = FALSE, 
                      quiet = TRUE, 
                      geometry=FALSE,
                      frequency=FALSE) {
  # download zip file
  if(!local) {
    path <- download_from_url(url = path, quiet = quiet)
  }
  
  # extract zip file
  tmpdirpath <- unzip_file(path, quiet=quiet)
  
  file_list_df <- zip::zip_list(path)
  if(!exists("file_list_df")) {
    stop(sprintf("No files found in zip"))
  }
  
  gtfs_obj <- create_gtfs_object(tmpdirpath, file_list_df$filename, quiet = quiet)
  
  if(geometry) {
    gtfs_obj <- gtfs_as_sf(gtfs_obj,quiet=quiet)
  }
  
  if(frequency) {
    gtfs_obj <- get_route_frequency(gtfs_obj) 
  }
  
  return(gtfs_obj) 
}

#' This function is deprecated. Please use read_gtfs
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
#' @return Dataframes of GTFS data.
#'
#' @export
#' @importFrom dplyr %>% arrange summarise group_by inner_join
#' @examples \donttest{
#' library(dplyr)
#' u1 <- "https://developers.google.com/transit/gtfs/examples/sample-feed.zip"
#' sample_gtfs <- import_gtfs(u1)
#' attach(sample_gtfs)
#' #list routes by the number of stops they have
#' routes_df %>% inner_join(trips_df, by="route_id") %>%
#'   inner_join(stop_times_df) %>% 
#'     inner_join(stops_df, by="stop_id") %>% 
#'       group_by(route_long_name) %>%
#'         summarise(stop_count=n_distinct(stop_id)) %>%
#'           arrange(desc(stop_count))
#' }
import_gtfs <- function(path, local = FALSE, quiet = FALSE) {
  .Deprecated("read_gtfs") #include a package argument, too
  read_gtfs(path, local = FALSE, quiet = FALSE)
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

download_from_url <- function(url, path=tempfile(fileext = ".zip"), quiet=FALSE) {
  
  stopifnot(length(url) == 1)
  
  # check if single element of dataframe was inputed. if so, convert to single value; error otherwise.
  if(!is.null(dim(url))) {
    if(all(dim(url) == c(1,1))) {
      url <- unlist(url, use.names = FALSE)
    } else {
      stop('Please input a single url.')
    }
  }
  
  # check if url links to a zip file
  valid <- valid_url(url)
  if(!valid) {
    if(!quiet) {
      stop1 <- sprintf("Link '%s' is invalid; failed to connect. NULL was returned.", url)
      stop(stop1)
    }
    return(NULL)
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
    if(!quiet) warning(warn)
    return(NULL)
  }
  return(path)
}

#' Checks UTF-8-BOM encoding.
#' 
#' Special thanks to @patperu for finding the issue and 
#' to @hrbrmstr for the code to help deal with the issue.
#' 
#' @param path the path the the text file
#' @param encoding can be one of \code{UTF-8}, \code{UTF-16} or \code{UTF-16BE}.
#'        Although a BOM could be used with UTF-32 and other encodings, such
#'        encodings are rarely used for data transmission and the three supported
#'        encodings are the most likely ones folks in R will be working with from
#'        web APIs.\cr\cr
#'        This function defaults to looking for \code{UTF-8} BOM, but you can
#'        override it.
#' @return \code{TRUE} if response contains a BOM, \code{NA} if an unsupported encoding
#'         was passed (along with a message)
#' @references \href{http://www.unicode.org/faq/utf_bom.html}{UTF-8, UTF-16, UTF-32 & BOM}
#' @noRd
#' @keywords internal
#' @author @@hrbrmstr

has_bom <- function(path, encoding="UTF-8") {

  B <- readBin(path, "raw", 4, 1)
  switch(encoding,
       `UTF-8`=B[1]==as.raw(0xef) & B[2]==as.raw(0xbb) & B[3]==as.raw(0xbf),
       `UTF-16`=B[1]==as.raw(0xff) & B[2]==as.raw(0xfe),
       `UTF-16BE`=B[1]==as.raw(0xfe) & B[2]==as.raw(0xff),
       { message("Unsupported encoding") ; return(NA) }
  )
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
  df_names <- paste(prefixes,"_df",sep="")
  if(!quiet) message('Reading files in feed...\n')
  gtfs_obj <- lapply(file_paths, 
                   function(x) read_gtfs_file(x, 
                                              tmpdirpath, 
                                              quiet = quiet))
  names(gtfs_obj) <- unname(df_names)
  gtfs_obj[sapply(gtfs_obj, is.null)] <- NULL
  class(gtfs_obj) <- "gtfs"
  if(!quiet) message('Reading files in feed... done.\n\n')
  
    
  gtfs_obj <- validate_gtfs(gtfs_obj, quiet = quiet)
  
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

  prefix <- gsub('.txt|-new', '', file_name) 
  # suffix '.*-new.txt' comes from trillium data
  prefix <- gsub('\\-|\\.', '_', prefix)
  return(prefix)
}

#' Parses one gtfs file
#'
#' @param prefix Character. gtfs file prefix (e.g. 'agency', 'stop_times', etc.)
#' @param file_path Character. file path
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @return Dataframe of parsed GTFS file.
#' @noRd
#' @keywords internal

parse_gtfs_file <- function(prefix, file_path, quiet = FALSE) {

  # only parse if file has any data, NULL o/w
  stopifnot(!is.na(file.size(file_path)))
  if(file.size(file_path) > 1) {

    ## get correct meta data using file prefix (e.g. 'agency', 'stop_times')
    meta <- get_gtfs_meta()[[prefix]]

    # check if a file is empty. If so, return NULL.
    L <- suppressWarnings(length(scan(file_path, what = "", quiet = TRUE, sep = '\n')))
    if(L < 1) {
      s <- sprintf("   File '%s' is empty.", basename(file_path))
      if(!quiet) message(s)
      return(NULL)
    }

    # if no meta data is found for a file type but file is not empty, read as is.
    if(is.null(meta)) {
      s <- sprintf("   File %s not recognized, trying to read file as csv.", basename(file_path))
      if(!quiet) message(s)

      tryCatch({
        df <- suppressMessages(readr::read_csv(file = file_path))
      }, error = function(error_condition) {
        s <- sprintf("   File could not be read as csv.", basename(file_path))
        if(!quiet) message(s)
        return(NULL)
      })
      return(df)
    }

    ## read.csv supports UTF-8-BOM. use this to get field names.
    small_df <- suppressWarnings(utils::read.csv(file_path, nrows = 10, stringsAsFactors = FALSE)) # get a small df to find how many cols are needed

    ## get correct coltype, if possible
    coltypes <- rep('c', dim(small_df)[2]) # create 'c' as coltype defaults
    names(coltypes) <- names(small_df) %>% tolower()
    indx <- match(names(coltypes), meta$field)  # indx from valid cols in meta$field. NAs will return for invalid cols

    colnms <- meta$field[indx] # get expected/required names for columns. these are imposed.

    ## !is.na(indx) = valid col in 'coltype' found in meta$field
    ## indx[!is.na(indx)] = location in 'meta$coltype' where corresponding type is found
    coltypes[!is.na(indx)] <- meta$coltype[indx[!is.na(indx)]] # valid cols found in small_df

    ## get colclasses for use in read.csv (useful when UTF-8-BOM encoding is found)
    colclasses <- sapply(coltypes, switch, c = "character", i = "integer", d = "double", "D" = "Date")

    ## collapse coltypes for use in read_csv
    coltypes <- coltypes %>% paste(collapse = "")

    ## switch function for when BOMs exist
    converttype <- function(x, y) {
      switch(x, character = as.character(y), integer = as.integer(y), double = as.double(y), Date = ymd(y))
    }

    if (has_bom(file_path)) { # check for BOM. if yes, use read.csv()
      csv <- quote(utils::read.csv(file_path, col.names = colnms, stringsAsFactors= FALSE))
      df <- try(suppressWarnings(eval(csv)) %>%
          mapply(converttype, x = colclasses, y = ., SIMPLIFY = FALSE) %>% # ensure proper column types
          tibble::as_tibble())

      if(any(class(df) %in% "try-error")) {
        probs <- "Error during import. Likely encoding error. Note that utils::read.csv() was used, not readr::read_csv()."
        attributes(df) <- append(attributes(df), list(problems = probs))
      }

    } else {
      df <- readr::read_csv(file = file_path, 
                            col_types = coltypes
        )
      probs <- readr::problems(df)
      
      if(dim(probs)[1] > 0) attributes(df) <- append(attributes(df), list(problems = probs))
    }

    return(df)
  } else return(NULL)

}
