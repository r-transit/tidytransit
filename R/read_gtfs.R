# Purpose -----------------------------------------------------------------

# Functions to read in GTFS data
# These should collectively allow a user to 
# - unzip GTFS data from a local file
# - validate that the content of the zipped file is a GTFS feed
# - read the data into an object of class gtfs

#' Unzip GTFS file and delete zip
#' 
#' @param path File path of zipped file
#' @param delete_zip Logical, whether to delete the zipped file after extraction.  Deletes by default.
#' 
#' @export

unzip_gtfs <- function(path, delete_zip = TRUE) {
  
  #TODO: give option to move zip file to another folder instead of deleting
  
  ex_dir <- strsplit(path, '/')[[1]][1]
  
  all_files <- list.files(ex_dir, full.names = TRUE)
  if (!identical(all_files, path)) stop(paste0('No files other than zipped GTFS should be in ', ex_dir, ' directory pre-extraction.'))
  
  #TODO: if this fails, maybe don't delete the file (failure can be a warning)
  unzip(path, exdir = ex_dir)
  
  if (delete_zip) file.remove(path)
  
}

#' Function to read all files into dataframes
#' 
#' @param file_path Character file path


read_sub_gtfs <- function(file_path, prefix = 'data/gtfs/', assign_envir = .GlobalEnv) {
  
  split_path <- strsplit(file_path, '/')
  file_name <- split_path[[1]][length(split_path[[1]])]
  
  df_name <- gsub('.txt', '', file_name)
  df_name <- paste0(df_name, '_df')
  
  print(paste0('Reading ', file_name))
  
  new_df <- read_csv(file_path)
  
  #TODO: take an environment argument with global as default?
  assign(df_name, new_df, envir = assign_envir)
  
}

# Read in feed ------------------------------------------------------------

#' Put GTFS text file contents into objects in memory and delete files
#' 
#' @param path Path to folder into which files were extracted.
#' @param delete_files Logical, whether to delete the files after extraction.  Deletes by default.
#' 
#' @export

read_gtfs <- function(path, delete_files = TRUE) {
  
  #TODO: clean up path if it's to zip file instead of folder?
  
  all_files <- list.files(path, full.names = TRUE)
  is_txt <- grepl(pattern = '.txt', x = all_files)
  all_txt <- all_files[is_txt]
  
  func_envir <- environment()
  
  lapply(all_files, function(x) read_sub_gtfs(x, assign_envir = func_envir))
  
  ls_envir <- ls(envir = func_envir)
  
  # print('Everything in my function environment')
  # print(ls_envir)
  
  df_list <- ls_envir[grepl(pattern = '_df', x = ls_envir)]
  
  gtfs_list <- list(mget(df_list, envir = func_envir))
  
  # print('Everything df in my function environment')
  # print(df_list)
  
  if (delete_files) file.remove(all_files)
  
  gtfs_list
  
}