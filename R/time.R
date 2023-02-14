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
    gtfs_obj$stop_times[, arrival_time := parse_hms_strings(arrival_time)]
    gtfs_obj$stop_times[, departure_time := parse_hms_strings(departure_time)]
  }
  
  # frequencies ####
  if(feed_contains(gtfs_obj, "frequencies") && nrow(gtfs_obj$frequencies) > 0) {
    stopifnot(inherits(gtfs_obj$frequencies, "data.table"))
    gtfs_obj$frequencies[, start_time := parse_hms_strings(start_time)]
    gtfs_obj$frequencies[, end_time := parse_hms_strings(end_time)]
  }
  
  return(gtfs_obj)
}

hms_to_hhmmss = function(vec) {
  format(vec, format = "%H:%M:%S")
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

# Dates ####
parse_gtfsio_date = function(gtfsio_date) {
  as.Date(as.character(gtfsio_date), format = "%Y%m%d")
}

date_as_gtfsio_char = function(date) {
  format(date, format = "%Y%m%d")
}

convert_dates <- function(gtfs_obj, parse_function = parse_gtfsio_date) {
  if(!is.null(gtfs_obj[["calendar"]])) { # $calendar matches calendar_dates
    stopifnot(inherits(gtfs_obj$calendar, "data.table"))
    gtfs_obj$calendar[,start_date := parse_function(start_date)]
    gtfs_obj$calendar[,end_date := parse_function(end_date)]
  }
  if(!is.null(gtfs_obj[["calendar_dates"]])) {
    stopifnot(inherits(gtfs_obj$calendar_dates, "data.table"))
    gtfs_obj$calendar_dates[,date := parse_function(date)]
  }
  if(!is.null(gtfs_obj[["feed_info"]])) {
    stopifnot(inherits(gtfs_obj$feed_info, "data.table"))
    if(!is.null(gtfs_obj$feed_info$feed_start_date)) {
      gtfs_obj$feed_info[,feed_start_date := parse_function(feed_start_date)]
    }
    if(!is.null(gtfs_obj$feed_info$feed_end_date)) {
      gtfs_obj$feed_info[,feed_end_date := parse_function(feed_end_date)]
    }
  }
  gtfs_obj
}

#' Returns all possible date/service_id combinations as a data frame
#' 
#' Use it to summarise service. For example, get a count of the number of 
#' services for a date. See example. 
#' 
#' @param gtfs_obj gtfs feed (tidygtfs object)
#' @return a date_service data frame
#' 
#' @keywords internal
#' @importFrom stats reshape
#' @examples 
#' library(dplyr)
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path)
#' nyc_services_by_date <- nyc$.$dates_services
#' # count the number of services running on each date
#' nyc_services_by_date %>% group_by(date) %>% count()
set_dates_services <- function(gtfs_obj) {
  has_calendar = feed_contains(gtfs_obj, "calendar") && nrow(gtfs_obj[["calendar"]]) > 0
  has_calendar_dates = feed_contains(gtfs_obj, "calendar_dates") && nrow(gtfs_obj[["calendar_dates"]]) > 0
  
  # check date validity 
  if(!has_calendar && !has_calendar_dates) {
    return(gtfs_obj)
  }
  
  feed_dates = list()
  if(has_calendar) {
    feed_dates$calendar <- c(gtfs_obj$calendar$start_date, gtfs_obj$calendar$end_date)
  }
  if(has_calendar_dates) {
    feed_dates$calendar_dates <- gtfs_obj$calendar_dates$date[which(gtfs_obj$calendar_dates$exception_type != 2)]
  }
  if(length(feed_dates$calendar) == 0 && length(feed_dates$calendar_dates) == 0) {
    warning("No valid dates defined in feed")
    return(gtfs_obj)
  }
  
  # table to connect every date to corresponding services (all dates from earliest to latest)
  if(has_calendar) {
    weekday <- function(date) {
      c("sunday", "monday", "tuesday", 
        "wednesday", "thursday", "friday", 
        "saturday")[as.POSIXlt(date)$wday + 1]
    }
    
    min_date = min(feed_dates$calendar, na.rm = T)
    max_date = max(feed_dates$calendar, na.rm = T)
    # get first and last date of a feed
    dates <- dplyr::tibble(
      date = seq(min_date, max_date, 1),
      weekday = weekday(date)
    )
    
    # gather services by weekdays
    .days = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday") 
    .cns_nondays = colnames(gtfs_obj$calendar)[which(!colnames(gtfs_obj$calendar) %in% .days)]
    service_ids_weekdays = gtfs_obj$calendar %>% 
      reshape(gc, direction = "long", idvar = .cns_nondays, varying = .days, 
              v.names = "bool", timevar = "weekday_num") %>% 
      left_join(data.frame(weekday_num = 1:7, weekday = .days), "weekday_num") %>% 
      dplyr::filter(bool == 1) %>% dplyr::select(service_id, weekday, start_date, end_date)
    
    # set services to dates according to weekdays and start/end date
    date_service_df <- 
      suppress_matches_multiple_warning(
        dplyr::full_join(dates, service_ids_weekdays, by="weekday")
      ) %>% 
      dplyr::filter(date >= start_date & date <= end_date) %>% 
      dplyr::select(-weekday, -start_date, -end_date)
    
    # addtions and exceptions
    if(has_calendar_dates) {
      # add calendar_dates additions (1)
      additions = gtfs_obj$calendar_dates %>% 
        filter(exception_type == 1) %>% 
        dplyr::select(-exception_type)
      if(nrow(additions) > 0) {
        date_service_df <- dplyr::full_join(date_service_df, 
                                            additions, 
                                            by=c("date", "service_id"))
      }
      
      # remove calendar_dates exceptions (2) 
      exceptions = gtfs_obj$calendar_dates %>% 
        dplyr::filter(exception_type == 2) %>% 
        dplyr::select(-exception_type)
      if(nrow(exceptions) > 0) {
        date_service_df <- dplyr::anti_join(date_service_df, 
                                            exceptions, 
                                            by=c("date", "service_id"))
      }
    }
  } else if(has_calendar_dates) { # only calendar_dates.txt
    date_service_df = gtfs_obj$calendar_dates[gtfs_obj$calendar_dates$exception_type != 2, c("date", "service_id")]
    date_service_df <- dplyr::as_tibble(date_service_df)
  }
  
  gtfs_obj$.$dates_services <- date_service_df
  
  return(gtfs_obj)
}

#' convert a vector of time strings
#' empty strings are converted to NA
#' @param time_strings char vector ("HH:MM:SS")
parse_hms_strings = function(time_strings) {
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
