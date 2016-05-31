# Purpose -----------------------------------------------------------------

# Functions to read in GTFS data
# These should collectively allow a user to
# - unzip GTFS data from a local file
# - validate that the content of the zipped file is a GTFS feed
# - read the data into an object of class gtfs

#' Unzip GTFS file and delete zip
#'
#' @param file path to zipped file
#' @param delete_zip <boolean> whether to delete the zipped file after extraction.  Deletes by default.
#' @param move_path <character> full file path to desire new location
#'
#' @return file path to directory with gtfs .txt files
#' @export

unzip_gtfs <- function(file, delete_zip = FALSE, move_path = NULL) {

  # set file path based on options
  if(is.null(move_path)) f <- file else f <- move_path
  f <- normalizePath(f)

  if(!is.null(move_path)) {
    if(dir.exists(move_path)) stop(sprintf('%s must be a full file path, not a directory path (e.g. "full/path/to/filename.zip")', move_path))
    file.copy(file, move_path, overwrite=TRUE)
  }

  # create extraction folder
  ex_dir <- file.path(dirname(f), strsplit(basename(f), "\\.")[[1]][1])
  if(!dir.exists(ex_dir)) dir.create(ex_dir) else stop('extraction folder already exists.')

  unzip(f, exdir = ex_dir)

  if(delete_zip) file.remove(f)

  message(sprintf("unzipped the following files to directory '%s'", ex_dir))
  list.files(ex_dir) %>% print

  return(ex_dir)

}


# Read in feed ------------------------------------------------------------

#' Put GTFS text file contents into objects in memory and delete files
#'
#' @param exdir path to folder into which files were extracted.
#' @param delete_files Logical, whether to delete the files after extraction.  Deletes by default.
#'
#' @export

read_gtfs <- function(exdir, delete_files = TRUE) {

  exdir <- normalizePath(exdir)
  if(!dir.exists(exdir)) stop('Not a valid directory.')

  all_files <- list.files(exdir, full.names = TRUE)
  is_txt <- grepl(pattern = '.txt', x = all_files)
  all_txt <- all_files[is_txt]

  if(!any(grepl('agency.txt', all_files))) stop("Required file 'agency.txt' not found. Abort.")

  # something_df <- dplyr::data_frame(a = 1:5)

  exec_env <- environment()

  lapply(all_files, function(x) read_sub_gtfs(x, assign_envir = exec_env))

  ls_envir <- ls(envir = exec_env)

  df_list <- ls_envir[grepl(pattern = '_df', x = ls_envir)]

  gtfs_list <- list(mget(df_list, envir = exec_env))

  # print('Everything df in my function environment')
  # print(df_list)

  if (delete_files) file.remove(all_files)

}

#' Function to read all files into dataframes
#'
#' @param file_path Character file path

read_sub_gtfs <- function(file_path, assign_envir = .GlobalEnv) {

  split_path <- strsplit(file_path, '/')
  file_name <- split_path[[1]][length(split_path[[1]])]

  df_name <- gsub('.txt', '', file_name)
  df_name <- paste0(df_name, '_df')

  print(paste0('Reading ', file_name))

  # need a better parser for stop times
  if(df_name == "stop_times_df") {
    new_df <- suppressWarnings(parse_stop_times(file_path)) # will have warning even though we fix problem
  } else new_df <- readr::read_csv(file_path)

  assign(df_name, new_df, envir = assign_envir)

}

#' Function to better read in stop_times.txt, which often fails
#'
#' @param file_path <character> file path
#' @param all_char <logical> default FALSE. temporary variable. option to import all stop time data as character.

parse_stop_times <- function(file_path, all_char = FALSE) {

  if(all_char) {
    # ALL CHARACTER
    small_df <- readr::read_csv(file_path, n_max = 10) # get a small df to find how many cols are needed
    ## types are defined by readr (see https://github.com/hadley/readr/blob/master/vignettes/column-types.Rmd)
    types_string <- rep('c', dim(small_df)[2]) %>% paste(collapse = "") # input all as character
    stop_times_df <- readr::read_csv(file_path, col_types = types_string)
    stop_times_df
  } else {
    ## define stop_times.txt variables and types
    stop_times_vars <- c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'stop_headsign', 'pickup_type', 'drop_off_type', 'shape_dist_traveled', 'timepoint')
    ## types are defined by readr (see https://github.com/hadley/readr/blob/master/vignettes/column-types.Rmd)
    stop_times_vars_type <- c('c', 'c', 'c', 'c', 'i', 'c', 'i', 'i', 'd', 'i')

    small_df <- readr::read_csv(file_path, n_max = 10) # get a small df to find how many cols are needed
    indx <- stop_times_vars %in% names(small_df)
    types_string <- paste(stop_times_vars_type[indx], collapse = "")
    stop_times_df <- readr::read_csv(file_path, col_types = types_string)
    stop_times_df
  }
}
