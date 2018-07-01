#' Download a zipped GTFS feed file from a url
#'
#' @param url Character URL of GTFS feed.
#' @param path Character. Folder into which to put zipped file. If NULL, then save a tempfile
#' @param quiet Boolean. Whether to see file download progress. FALSE by default.
#'
#' @return File path
#'
#' @export

get_feed <- function(url, path=NULL, quiet=FALSE) {

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
      warn1 <- sprintf("Link '%s' is invalid; failed to connect. NULL was returned.", url)
      warning(warn1)
    }
    return(NULL)
  }

  # generate a temporary file path if no path is specified
  if(is.null(path)) temp <- tempfile(fileext = ".zip") else temp <- file.path(path, 'gtfs_zip.zip')

  r <- httr::GET(url)

  # Get gtfs zip if url can be reach
  if(httr::status_code(r) == 200) {
    check <- try(utils::download.file(url, temp, quiet = quiet), silent=TRUE)
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

  # return the temp path - for unzipping
  return(temp)

}


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
    paths <- paths %>% 
      sapply(. %>% normalizePath)
    data_list <- paths %>% 
      lapply(. %>% 
            unzip_gtfs_files(quiet=quiet) %>% 
               read_gtfs(quiet=quiet))
  } else {
    data_list <- paths %>% 
      lapply(. %>% feed_flow)
  }

  # show note, which is suppressed in read_gtfs
  if(!quiet) {
    message("\n")
    message('NOTE: Parsing errors and warnings while importing data can be extracted from any given dataframe with `attr(df, "problems")`.')
    message("\n")
  }

  if(length(data_list) > 1) 
    return(data_list) 
  else 
    return(data_list[[1]])
}

#' Checks UTF-8-BOM encoding. Special thanks to @patperu for finding the issue and to @hrbrmstr for the code to help deal with the issue.
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

# Purpose -----------------------------------------------------------------

# Functions to read in GTFS data
# These should collectively allow a user to
# - unzip GTFS data from a local file
# - validate that the content of the zipped file is a GTFS feed
# - read the data into an object

#' Unzip GTFS file and delete zip
#'
#' @param zipfile path to zipped file
#' @param delete_zip Boolean. whether to delete the zipped file after extraction.  Deletes by default.
#' @param move_path Character. full file path to desire new location
#' @param quiet Boolean. Whether to output files found in folder.
#'
#' @return file path to directory with gtfs .txt files

unzip_gtfs_files <- function(zipfile, delete_zip = FALSE, move_path = NULL, quiet = FALSE) {

  # set file path based on options
  if(is.null(move_path)) f <- zipfile else f <- move_path

  # check path
  if(try(path.expand(f), silent = TRUE) %>% assertthat::is.error()) {
    warn <- 'Invalid file path. NULL is returned.'
    warning(warn)
    return(NULL)
  }

  f <- normalizePath(f)

  if(!is.null(move_path)) {
    if(dir.exists(move_path)) {
      stop(sprintf('%s must be a full file path, not a directory path (e.g. "full/path/to/filename.zip")', move_path))
    }
    file.copy(zipfile, move_path, overwrite=TRUE)
  }

  # create extraction folder
  ex_dir <- file.path(dirname(f), strsplit(basename(f), "\\.")[[1]][1])
  if(!dir.exists(ex_dir)) dir.create(ex_dir) else {
    warning('Extraction folder already exists. Overwriting.')
    rmfolder(ex_dir)
    dir.create(ex_dir)
  }

  utils::unzip(f, exdir = ex_dir)

  if(delete_zip) file.remove(f)

  if(length(list.files(ex_dir)) == 0) {
    warn <- "No files found after decompressing. NULL is returned."
    return(NULL)
  }

  if(!quiet) {
    message(sprintf("Unzipped the following files to directory '%s'...", ex_dir))
    list.files(ex_dir) %>% print
  }

  return(ex_dir)

}


# Read in feed ------------------------------------------------------------

#' Put GTFS text file contents into objects in memory and delete files
#'
#' @param exdir Character. Path to folder into which files were extracted.
#' @param delete_files Logical, whether to delete the files after extraction.  Does not delete by default.
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @noRd
#' @export
read_gtfs <- function(exdir, delete_files = FALSE, quiet = FALSE) {

  # check path
  check <- try(normalizePath(exdir), silent=TRUE)
  if(assertthat::is.error(check)) {
    warn <- 'Invalid file path. NULL is returned.'
    if(!quiet) warning(warn)
    return(NULL)
  }

  all_files <- list.files(exdir, full.names = TRUE)
  is_txt <- grepl(pattern = '.txt', x = all_files)
  all_txt <- all_files[is_txt]

  exec_env <- environment()

  lapply(all_files, function(x) read_sub_gtfs(x, assign_envir = exec_env, quiet = quiet))

  ls_envir <- ls(envir = exec_env)

  df_list <- ls_envir[grepl(pattern = '_df', x = ls_envir)]

  gtfs_list <- mget(df_list, envir = exec_env)

  if (delete_files) {
    file.remove(all_files)
    file.remove(exdir)
  }

  if(!quiet) message('...done.\n\n')

  # check if valid 'gtfs'
  check <- validate_gtfs_structure(gtfs_list, return_gtfs_obj = FALSE, quiet = TRUE)
  valid <- all(check$all_req_files, check$all_req_fields_in_req_files)

  if(!quiet) message("Testing data structure...")
  if(valid) {
    class(gtfs_list) <- 'gtfs'
    if(!quiet) message("...passed. Valid GTFS object.\n")
  } else {
    if(!quiet) message("...failed. Invalid data structure.\n")
  }

  return(gtfs_list)

}

#' Function to read all files into dataframes
#'
#' @param file_path Character file path
#' @param assign_envir Environment Object. Option of where to assign dataframes.
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @noRd

read_sub_gtfs <- function(file_path, assign_envir, quiet = FALSE) {

  split_path <- strsplit(file_path, '/')
  file_name <- split_path[[1]][length(split_path[[1]])]

  prefix <- gsub('.txt|-new', '', file_name) # suffix '.*-new.txt' comes from trillium data
  prefix <- gsub('\\-|\\.', '_', prefix)
  df_name <- paste0(prefix, '_df')

  if(!quiet) message(paste0('Reading ', file_name))

  new_df <- parse_gtfs(prefix, file_path, quiet = quiet) # will have warning even though we fix problem

  assign(df_name, new_df, envir = assign_envir)

}

#' Function to better read in GTFS txt files
#'
#' @param prefix Character. gtfs file prefix (e.g. 'agency', 'stop_times', etc.)
#' @param file_path Character. file path
#' @param quiet Boolean. Whether to output messages and files found in folder.
#' @return Dataframe of parsed GTFS file.
#' @noRd

parse_gtfs <- function(prefix, file_path, quiet = FALSE) {

  # only parse if file has any data, NULL o/w
  stopifnot(!is.na(file.size(file_path)))
  if(file.size(file_path) > 1) {

    ## get correct meta data using file prefix (e.g. 'agency', 'stop_times')
    meta <- get_gtfs_meta()[[prefix]]

    # check if a file is empty. If so, return NULL.
    if(length(scan(file_path, what = "", quiet = TRUE, sep = '\n')) < 1) {
      s <- sprintf("File '%s' is empty. Returning NULL.\n", basename(file_path))
      message(s)
      return()
    }

    # if no meta data is found for a file type but file is not empty, read as is.
    if(is.null(meta)) {
      s <- sprintf("File %s not recognized. No meta data exists. Reading file as csv.\n", basename(file_path))
      message(s)
      csv <- quote(readr::read_csv(file = file_path))
      df <- suppressMessages(trigger_suppressWarnings(eval(csv), quiet))
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
    colclasses <- sapply(coltypes, switch, c = "character", i = "integer", d = "double")

    ## collapse coltypes for use in read_csv
    coltypes <- coltypes %>% paste(collapse = "")

    ## switch function for when BOMs exist
    converttype <- function(x, y) {
      switch(x, character = as.character(y), integer = as.integer(y), double = as.double(y))
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
      csv <- quote(readr::read_csv(file = file_path, col_types = coltypes, col_names = colnms, skip = 1L))
      prob <- quote(readr::problems(readr::read_csv(file = file_path, col_types = coltypes, col_names = colnms, skip = 1L)))
      df <- trigger_suppressWarnings(eval(csv), quiet)
      probs <- trigger_suppressWarnings(eval(prob), quiet)

      if(dim(probs)[1] > 0) attributes(df) <- append(attributes(df), list(problems = probs))
    }

    return(df)
  } else return(NULL)

}
