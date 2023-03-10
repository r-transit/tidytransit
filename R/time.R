#' Convert time columns to hms::hms in feed
#' 
#' Overwrites character columns in stop_times (arrival_time, departure_time) and 
#' frequencies (start_time, end_time) with times converted with [hms::hms()].
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @return gtfs_obj with hms times columns for stop_times and frequencies
#' 
#' @importFrom hms new_hms
convert_times_to_hms <- function(gtfs_obj) {
  arrival_time_hms <- departure_time_hms <- start_time_hms <- end_time_hms <-  NULL
  
  # stop_times ####
  if(feed_contains(gtfs_obj, "stop_times")) {
    stopifnot(inherits(gtfs_obj$stop_times, "data.table"))
    gtfs_obj$stop_times[, arrival_time := hhmmss_to_hms(arrival_time)]
    gtfs_obj$stop_times[, departure_time := hhmmss_to_hms(departure_time)]
  }
  
  # frequencies ####
  if(feed_contains(gtfs_obj, "frequencies") && nrow(gtfs_obj$frequencies) > 0) {
    stopifnot(inherits(gtfs_obj$frequencies, "data.table"))
    gtfs_obj$frequencies[, start_time := hhmmss_to_hms(start_time)]
    gtfs_obj$frequencies[, end_time := hhmmss_to_hms(end_time)]
  }
  
  return(gtfs_obj)
}

convert_hms_to_char <- function(gtfs_obj) {
  if(feed_contains(gtfs_obj, "stop_times")) {
    stopifnot(inherits(gtfs_obj$stop_times, "data.table"))
    gtfs_obj$stop_times[, arrival_time := hms_to_hhmmss(arrival_time)]
    gtfs_obj$stop_times[, departure_time := hms_to_hhmmss(departure_time)]
  }
  
  if(feed_contains(gtfs_obj, "frequencies") && nrow(gtfs_obj$frequencies) > 0) {
    stopifnot(inherits(gtfs_obj$frequencies, "data.table"))
    gtfs_obj$frequencies[, start_time := hms_to_hhmmss(start_time)]
    gtfs_obj$frequencies[, end_time := hms_to_hhmmss(end_time)]
  }
  
  gtfs_obj
}

# string conversion functions ####

#' convert a vector of time strings
#' empty strings are converted to NA
#' @param time_strings char vector ("HH:MM:SS")
hhmmss_to_hms = function(time_strings) {
  if(inherits(time_strings, "hms")) { return(time_strings) }
  empty_strings = nchar(time_strings) == 0
  
  time_seconds = suppressWarnings(hhmmss_to_seconds(time_strings))
  
  if(any(is.na(time_seconds) & !empty_strings)) {
    # strings are not in HH:MM:SS format
    time_seconds = hhmmss_to_sec_split(time_strings)
  }
  time_seconds[empty_strings] <- NA
  
  return(hms::new_hms(time_seconds))
}

#' Function to convert "HH:MM:SS" time strings to seconds.
#' @param hhmmss_str string
hhmmss_to_seconds <- function(hhmmss_str) {
  as.numeric(substr(hhmmss_str, 0, 2)) * 3600 +
    as.numeric(substr(hhmmss_str, 4, 5)) * 60 +
    as.numeric(substr(hhmmss_str, 7, 8))
}

#' Fallback function to convert strings like 5:02:11
#' 10x slower than [hhmmss_to_seconds()], empty strings are converted to NA
#' @param hhmmss_str string
hhmmss_to_sec_split <- function(hhmmss_str) {
  seconds = sapply(strsplit(hhmmss_str, ":"), function(Y) {
    sum(as.numeric(Y) * c(3600, 60, 1))
  })
  seconds[nchar(hhmmss_str) == 0] <- NA_real_
  return(seconds)  
}

hms_to_hhmmss = function(vec) {
  hhmmss = format(vec, format = "%H:%M:%S")
  hhmmss[is.na(vec)] <- ""
  return(hhmmss)
}

# fix function ####
replace_NA_times = function(stop_times) {
  stopifnot(!inherits(stop_times, "gtfs"))
  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]
  return(stop_times)
}
