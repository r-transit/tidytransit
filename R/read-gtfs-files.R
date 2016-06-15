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
#' @export

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


  if(!quiet) {
    message(sprintf("Unzipped the following files to directory '%s'...", ex_dir))
    list.files(ex_dir) %>% print
    message('...done.\n\n')
  }

  return(ex_dir)

}


# Read in feed ------------------------------------------------------------

#' Put GTFS text file contents into objects in memory and delete files
#'
#' @param exdir Character. Path to folder into which files were extracted.
#' @param delete_files Logical, whether to delete the files after extraction.  Deletes by default.
#' @param quiet Boolean. Whether to output messages and files found in folder.
#'
#' @export

read_gtfs <- function(exdir, delete_files = TRUE, quiet = FALSE) {

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

  if(!quiet) {
    message("\n")
    message('NOTE: Parsing errors and warnings while importing data can be extracted from any given dataframe with `attr(df, "problems")`.')
    message("\n")
  }

  ls_envir <- ls(envir = exec_env)

  df_list <- ls_envir[grepl(pattern = '_df', x = ls_envir)]

  gtfs_list <- mget(df_list, envir = exec_env)

  if (delete_files) {
    file.remove(all_files)
    file.remove(exdir)
  }

  class(gtfs_list) <- 'gtfs'
  return(gtfs_list)

}

#' Function to read all files into dataframes
#'
#' @param file_path Character file path
#' @param assign_envir Environment Object. Option of where to assign dataframes.
#' @param quiet Boolean. Whether to output messages and files found in folder.

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
#'
parse_gtfs <- function(prefix, file_path, quiet = FALSE) {

  # only parse if file has any data, NULL o/w
  stopifnot(!is.na(file.size(file_path)))
  if(file.size(file_path) > 1) {

    ## get correct meta data using file prefix (e.g. 'agency', 'stop_times')
    meta <- get_gtfs_meta()[[prefix]]
    small_df <- suppressWarnings(readr::read_csv(file_path, n_max = 10)) # get a small df to find how many cols are needed
    ## get correct coltype, if possible
    coltypes <- rep('c', dim(small_df)[2]) # create 'c' as coltype defaults
    names(coltypes) <- names(small_df)
    indx <- match(names(coltypes), meta$field)  # indx from valid cols in meta$field. NAs will return for invalid cols
    ## !is.na(indx) = valid col in 'coltype' found in meta$field
    ## indx[!is.na(indx)] = location in 'meta$coltype' where corresponding type is found
    coltypes[!is.na(indx)] <- meta$coltype[indx[!is.na(indx)]] # valid cols found in small_df
    coltypes <- coltypes %>% paste(collapse = "")

    if(!quiet) {
      df <- readr::read_csv(file_path, col_types = coltypes)
      probs <- readr::problems(readr::read_csv(file_path, col_types = coltypes))
    } else {
      df <- suppressWarnings(readr::read_csv(file_path, col_types = coltypes))
      probs <- suppressWarnings(readr::problems(readr::read_csv(file_path, col_types = coltypes)))
    }

    if(dim(probs)[1] > 0) attributes(df) <- append(attributes(df), list(problems = probs))

    return(df)
  } else return(NULL)

}