# Purpose -----------------------------------------------------------------

# Functions to read in GTFS data
# These should collectively allow a user to
# - unzip GTFS data from a local file
# - validate that the content of the zipped file is a GTFS feed
# - read the data into an object of class gtfs

#' Unzip GTFS file and delete zip
#'
#' @param file path to zipped file
#' @param delete_zip Boolean. whether to delete the zipped file after extraction.  Deletes by default.
#' @param move_path Character. full file path to desire new location
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
#' @param exdir Character. Path to folder into which files were extracted.
#' @param delete_files Logical, whether to delete the files after extraction.  Deletes by default.
#'
#' @export

read_gtfs <- function(exdir, delete_files = TRUE) {

  exdir <- normalizePath(exdir)
  if(!dir.exists(exdir)) stop('Not a valid directory.')

  all_files <- list.files(exdir, full.names = TRUE)
  is_txt <- grepl(pattern = '.txt', x = all_files)
  all_txt <- all_files[is_txt]

  if(!any(grepl('agency.txt', all_files))) {
    message("\nRequired file 'agency.txt' not found. NULL is returned.\n\n")
    return(NULL)
  }

  # something_df <- dplyr::data_frame(a = 1:5)

  exec_env <- environment()

  lapply(all_files, function(x) read_sub_gtfs(x, assign_envir = exec_env))
  message("\n")
  message('NOTE: Parsing errors and warnings while importing data can be extracted from any given dataframe with `attr(df, "problems")`.')
  message("\n")

  ls_envir <- ls(envir = exec_env)

  df_list <- ls_envir[grepl(pattern = '_df', x = ls_envir)]

  gtfs_list <- mget(df_list, envir = exec_env)

  # print('Everything df in my function environment')
  # print(df_list)

  if (delete_files) file.remove(all_files)

  gtfs_list

}

#' Function to read all files into dataframes
#'
#' @param file_path Character file path
#' @param assign_envir Environment Object. Option of where to assign dataframes.

read_sub_gtfs <- function(file_path, assign_envir) {

  split_path <- strsplit(file_path, '/')
  file_name <- split_path[[1]][length(split_path[[1]])]

  prefix <- gsub('.txt', '', file_name)
  df_name <- paste0(prefix, '_df')

  message(paste0('Reading ', file_name))

  new_df <- parse_gtfs(prefix, file_path) # will have warning even though we fix problem
  # new_df <- parse_gtfs(prefix, file_path) # will have warning even though we fix problem

  assign(df_name, new_df, envir = assign_envir)

}

#' Function to better read in GTFS txt files
#'
#' @param prefix Character. gtfs file prefix (e.g. 'agency', 'stop_times', etc.)
#' @param file_path Character. file path

parse_gtfs <- function(prefix, file_path) {

  # only parse if file has any data, NULL o/w
  if(file.size(file_path) > 0) {

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
    df <- readr::read_csv(file_path, col_types = coltypes)
    probs <- readr::problems(readr::read_csv(file_path, col_types = coltypes))
    if(dim(probs)[1] > 0) attributes(df) <- append(attributes(df), list(problems = probs))

    return(df)
  } else return(NULL)

}

#' Function that creates gtfs meta data
#' @return Environment with gtfs data.
#' @export

get_gtfs_meta <- function() {

  # agency
  assign("agency", list())
  agency$field <- c('agency_id', 'agency_name', 'agency_url', 'agency_timezone', 'agency_lang', 'agency_phone', 'agency_fare_url', 'agency_email')
  agency$spec <- c('opt', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt')
  agency$coltype <- rep('c', 8)

  # stops
  assign("stops", list())
  stops$field <- c('stop_id', 'stop_code', 'stop_name', 'stop_desc', 'stop_lat', 'stop_lon', 'zone_id', 'stop_url', 'location_type', 'parent_station', 'stop_timezone', 'wheelchair_boarding')
  stops$spec <- c('req', 'opt', 'req', 'opt', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  stops$coltype <- rep('c', length(stops$field))
  stops$coltype[which(stops$field %in% c('stop_lat', 'stop_lon'))] <- 'd' # double
  stops$coltype[which(stops$field %in% c('location_type', 'wheelchair_boarding'))] <- 'i' #integers

  # routes
  assign("routes", list())
  routes$field <- c('route_id', 'agency_id', 'route_short_name', 'route_long_name', 'route_desc', 'route_type', 'route_url', 'route_color', 'route_text_color')
  routes$spec <- c('req', 'opt', 'req', 'req', 'opt', 'req', 'opt', 'opt', 'opt')
  routes$coltype <- rep('c', length(routes$field))
  routes$coltype[routes$field %in% c('route_type')] <- 'i'

  # trips
  assign("trips", list())
  trips$field <- c('route_id', 'service_id', 'trip_id', 'trip_headsign', 'trip_short_name', 'direction_id', 'block_id', 'shape_id', 'wheelchair_accessible', 'bikes_allowed')
  trips$spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt', 'opt')
  trips$coltype <- rep('c', length(trips$field))
  trips$coltype[trips$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'

  # stop_times
  assign("stop_times", list())
  stop_times$field <- c('trip_id', 'arrival_time', 'departure_time', 'stop_id', 'stop_sequence', 'stop_headsign', 'pickup_type', 'drop_off_type', 'shape_dist_traveled', 'timepoint')
  stop_times$spec <- c('req', 'req', 'req', 'req', 'req', 'opt', 'opt', 'opt', 'opt', 'opt')
  stop_times$coltype <- rep('c', length(stop_times$field))
  stop_times$coltype[stop_times$field %in% c('stop_sequence', 'pickup_type', 'drop_off_type', 'timepoint')] <- 'i'
  stop_times$coltype[stop_times$field %in% c('shape_dist_traveled')] <- 'd'


  # calendar
  assign("calendar", list())
  calendar$field <- c('service_id', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday', 'start_date', 'end_date')
  calendar$spec <- rep('req', times = 10)
  calendar$coltype <- rep('i', length(calendar$field))
  calendar$coltype[calendar$field %in% c('service_id', 'start_date', 'end_date')] <- 'c'


  # calendar_dates
  assign("calendar_dates", list())
  calendar_dates$field <- c('service_id', 'date', 'exception_type')
  calendar_dates$spec <- c('req', 'req', 'req')
  calendar_dates$coltype <- rep('c', length(calendar_dates$field))
  calendar_dates$coltype[calendar_dates$field %in% c('exception_type')] <- 'i'


  # fare_attributes
  assign("fare_attributes", list())
  fare_attributes$field <- c('fare_id', 'price', 'currency_type', 'payment_method', 'transfers', 'transfer_duration')
  fare_attributes$spec <- c('req', 'req', 'req', 'req', 'req', 'opt')
  fare_attributes$coltype <- rep('c', length(fare_attributes$field))
  fare_attributes$coltype[fare_attributes$field %in% c('payment_method', 'transfers')] <- 'i'
  fare_attributes$coltype[fare_attributes$field %in% c('transfer_duration')] <- 'd'

  # fare_rules
  assign("fare_rules", list())
  fare_rules$field <- c('fare_id', 'route_id', 'origin_id', 'destination_id', 'contains_id')
  fare_rules$spec <- c('req', 'opt', 'opt', 'opt', 'opt')
  fare_rules$coltype <- rep('c', length(fare_rules$field))
  fare_rules$coltype[fare_rules$field %in% c('direction_id', 'wheelchair_accessible', 'bikes_allowed')] <- 'i'

  # shapes
  assign("shapes", list())
  shapes$field <- c('shape_id', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence', 'shape_dist_traveled')
  shapes$spec <- c('req', 'req', 'req', 'req', 'opt')
  shapes$coltype <- rep('d', length(shapes$field))
  shapes$coltype[shapes$field %in% c('shape_id')] <- 'c'
  shapes$coltype[shapes$field %in% c('shape_pt_sequence')] <- 'i'

  # frequencies
  assign("frequencies", list())
  frequencies$field <- c('trip_id', 'start_time', 'end_time', 'headway_sec', 'exact_times')
  frequencies$spec <- c('req', 'req', 'req', 'req', 'opt')
  frequencies$coltype <- rep('c', length(frequencies$field))
  frequencies$coltype[frequencies$field %in% c('headway_sec')] <- 'd'
  frequencies$coltype[frequencies$field %in% c('exact_times')] <- 'i'

  # transfers
  assign("transfers", list())
  transfers$field <- c('from_stop_id', 'to_stop_id', 'transfer_type', 'min_transfer_time')
  transfers$spec <- c('req', 'req', 'req', 'opt')
  transfers$coltype <- rep('c', length(transfers$field))
  transfers$coltype[transfers$field %in% c('exception_type')] <- 'i'

  # feed_info
  assign("feed_info", list())
  feed_info$field <- c('feed_publisher_name', 'feed_publisher_url', 'feed_lang', 'feed_start_date', 'feed_end_date', 'feed_version')
  feed_info$spec <- c('req', 'req', 'req', 'opt', 'opt', 'opt')
  feed_info$coltype <- rep('c', length(feed_info$field))
  feed_info$coltype[feed_info$field %in% c('exception_type')] <- 'i'

  environment()

}

