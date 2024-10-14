#' convert a vector of time strings
#' empty strings are converted to NA
#' @param time_strings char vector ("HH:MM:SS")
hhmmss_to_hms <- function(time_strings) {
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

# interpolate stop_times ####

#' Interpolate missing stop_times linearly
#' Uses shape_dist_traveled if available
#' @param x tidygtfs object or stop_times table
#' @param use_shape_dist if available, use shape_dist_traveled column for time
#'                       interpolation. If shape_dist_traveled is missing, times
#'                       are interpolated equally between stops. 
#' @return tidygtfs or stop_times with interpolated arrival and departure times
#' @examples
#' \dontrun{
#' data(gtfs_duke)
#' print(gtfs_duke$stop_times[1:5, 1:5])
#' 
#' gtfs_duke_2 = interpolate_stop_times(gtfs_duke)
#' print(gtfs_duke_2$stop_times[1:5, 1:5])
#' 
#' gtfs_duke_3 = interpolate_stop_times(gtfs_duke, FALSE)
#' print(gtfs_duke_3$stop_times[1:5, 1:5])
#' }
#' @export
interpolate_stop_times = function(x, use_shape_dist = TRUE) {
  ....event_time <- ....shape_dist_traveled <- NULL
  if(inherits(x, "tidygtfs")) {
    stoptimes = x$stop_times
  } else {
    stoptimes = x
  }
  
  stoptimes$....rowindex <- seq_len(nrow(stoptimes))
  if(!use_shape_dist || !("shape_dist_traveled" %in% colnames(stoptimes))) {
    stoptimes$....shape_dist_traveled <- stoptimes$stop_sequence
  } else {
    stoptimes$....shape_dist_traveled <- stoptimes$shape_dist_traveled
  }

  # bring times in long format
  times_interpolated = gather_dt(stoptimes, "....event_type", "....event_time", c("arrival_time", "departure_time"))
  setorder(times_interpolated, "trip_id", "stop_sequence", "....event_type")
  
  # interpolate times
  times_interpolated[, ....event_time := ceiling(approx_NA(as.numeric(....event_time), ....shape_dist_traveled)), 
                     by = "trip_id"]

  times_wide = times_interpolated %>%
    spread_dt("....event_type", "....event_time")
  setorder(times_wide, "....rowindex")
  
  # return
  stoptimes <- dplyr::as_tibble(times_wide)[,colnames(stoptimes)]
  attributes(stoptimes)$sorted <- NULL
  stoptimes$....shape_dist_traveled <- stoptimes$....rowindex <- NULL
  if(inherits(x, "tidygtfs")) {
    x$stop_times <- stoptimes
    return(x)
  } else {
    return(stoptimes)
  }
}

approx_NA = function(values, x = NULL) {
  if(!anyNA(values)) {
    return(values)
  }
  if(is.null(x)) {
    x <- seq_len(length(values))
  }
  
  values_approx = stats::approx(x, values, x[is.na(values)], ties = "ordered")$y
  values[is.na(values)] <- values_approx
  
  return(values)
}
