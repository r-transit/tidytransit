#' Filter stop times by hour of the day
#' 
#' @param stop_times a gtfs_obj$stop_times dataframe with arrival_time and departure_time 
#' @return dataframe with only stop times within the hours specified, with time columns as lubridate periods
#' @keywords internal
filter_stop_times_by_hour <- function(stop_times, 
                                      start_hour, 
                                      end_hour) {
  dplyr::filter(stop_times, arrival_time > 
                  hms::hms(hours = start_hour) & 
                  departure_time < hms::hms(hours = end_hour))
}

#' Use hms::hms columns in feed
#' 
#' Overwirtes character columns in stop_times (arrival_time, departure_time) and 
#' frequencies (start_time, end_time) with times converted with [hms::hms()].
#' 
#' @param gtfs_obj a gtfs object in which hms times should be set, 
#'                the modified gtfs_obj is returned
#' @return gtfs_obj with added hms times columns for stop_times and frequencies
#' @importFrom hms new_hms
set_hms_times <- function(gtfs_obj) {
  if(is.null(gtfs_obj$stop_times)) {
    stop("stop_times.txt not provided")    
  }
  arrival_time_hms <- departure_time_hms <- start_time_hms <- end_time_hms <-  NULL
  
  if(feed_contains(gtfs_obj, "stop_times")) {
    stopifnot(inherits(gtfs_obj$stop_times, "data.table"))
    # arrival_time
    suppressWarnings(
      gtfs_obj$stop_times[, arrival_time_hms := hms::new_hms(hhmmss_to_seconds(arrival_time))]
    )
    if(any(is.na(gtfs_obj$stop_times$arrival_time_hms))) {
      # warning("malformed arrival_time strings in stop_times")
      gtfs_obj$stop_times[, arrival_time_hms := hms::new_hms(hhmmss_to_sec_split(arrival_time))]
    }
    gtfs_obj$stop_times[, arrival_time := arrival_time_hms]
    gtfs_obj$stop_times[, arrival_time_hms := NULL]

    # departure_time    
    suppressWarnings(
      gtfs_obj$stop_times[, departure_time_hms := hms::new_hms(hhmmss_to_seconds(departure_time))]
    )
    if(any(is.na(gtfs_obj$stop_times$departure_time_hms))) {
      # warning("malformed departure_time strings in stop_times")
      gtfs_obj$stop_times[, departure_time_hms := hms::new_hms(hhmmss_to_sec_split(departure_time))]
    }
    gtfs_obj$stop_times[, departure_time := departure_time_hms]
    gtfs_obj$stop_times[, departure_time_hms := NULL]
  }
  
  if(feed_contains(gtfs_obj, "frequencies") && nrow(gtfs_obj$frequencies) > 0) {
    stopifnot(inherits(gtfs_obj$frequencies, "data.table"))
    
    suppressWarnings(
      gtfs_obj$frequencies[, start_time_hms := hms::new_hms(hhmmss_to_seconds(start_time))]
    )
    if(any(is.na(gtfs_obj$frequencies$start_time_hms))) {
      # warning("malformed start_time strings in frequencies")
      gtfs_obj$frequencies[, start_time_hms := hms::new_hms(hhmmss_to_sec_split(start_time))]
    }
    gtfs_obj$frequencies[, start_time := start_time_hms]
    gtfs_obj$frequencies[, start_time_hms := NULL]
    
    suppressWarnings(
      gtfs_obj$frequencies[, end_time_hms := hms::new_hms(hhmmss_to_seconds(end_time))]
    )
    if(any(is.na(gtfs_obj$frequencies$end_time_hms))) {
      # warning("malformed end_time strings in frequencies")
      gtfs_obj$frequencies[, end_time_hms := hms::new_hms(hhmmss_to_sec_split(end_time))]
    }
    gtfs_obj$frequencies[, end_time := end_time_hms]
    gtfs_obj$frequencies[, end_time_hms := NULL]
  }
  
  gtfs_obj
}

parse_gtfsio_date = function(gtfsio_date) {
  as.Date(as.character(gtfsio_date), format = "%Y%m%d")
}

set_dates <- function(gtfs_obj) {
  if(!is.null(gtfs_obj[["calendar"]])) { # $calendar matches calendar_dates
    stopifnot(inherits(gtfs_obj$calendar, "data.table"))
    gtfs_obj$calendar[,start_date := parse_gtfsio_date(start_date)]
    gtfs_obj$calendar[,end_date := parse_gtfsio_date(end_date)]
  }
  if(!is.null(gtfs_obj[["calendar_dates"]])) {
    stopifnot(inherits(gtfs_obj$calendar_dates, "data.table"))
    gtfs_obj$calendar_dates[,date := parse_gtfsio_date(date)]
  }
  if(!is.null(gtfs_obj[["feed_info"]])) {
    stopifnot(inherits(gtfs_obj$feed_info, "data.table"))
    if(!is.null(gtfs_obj$feed_info$feed_start_date)) {
      gtfs_obj$feed_info[,feed_start_date := parse_gtfsio_date(feed_start_date)]
    }
    if(!is.null(gtfs_obj$feed_info$feed_end_date)) {
      gtfs_obj$feed_info[,feed_end_date := parse_gtfsio_date(feed_end_date)]
    }
  }
  gtfs_obj
}

#' Returns all possible date/service_id combinations as a data frame
#' 
#' Use it to summarise service. For example, get a count of the number of services for a date. See example. 
#' @return a date_service data frame
#' @param gtfs_obj a gtfs_object as read by [read_gtfs()]
#' @export
#' @examples 
#' library(dplyr)
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- read_gtfs(local_gtfs_path) %>% set_dates_services()
#' nyc_services_by_date <- nyc$.$dates_services
#' # count the number of services running on each date
#' nyc_services_by_date %>% group_by(date) %>% count()
set_dates_services <- function(gtfs_obj) {
  weekday <- function(date) {
    c("sunday", "monday", "tuesday", 
      "wednesday", "thursday", "friday", 
      "saturday")[as.POSIXlt(date)$wday + 1]
  }
  
  # get first and last date of a feed
  if(!is.null(gtfs_obj$calendar) && (
     all(is.na(gtfs_obj$calendar[["start_date"]])) || 
     all(is.na(gtfs_obj$calendar[["end_date"]])))) {
    feed_dates <- gtfs_obj$calendar_dates$date[which(gtfs_obj$exception_type != 2)]
    if(length(feed_dates) == 0) {
      warning("No valid dates defined in feed")
      return(gtfs_obj)
    }
  } else {
    feed_dates = c(gtfs_obj$calendar$start_date, gtfs_obj$calendar$end_date)
  }
  min_date = min(feed_dates, na.rm = T)
  max_date = max(feed_dates, na.rm = T)
  
  # table to connect every date to corresponding services (all dates from earliest to latest)
  dates <- dplyr::tibble(
    date = seq(min_date, max_date, 1),
    weekday = weekday(date)
  )
  
  # gather services by weekdays
  service_ids_weekdays <-
    tidyr::gather(
      gtfs_obj$calendar,
      key = "weekday",
      value = "bool",
      -c(service_id, start_date, end_date)
    ) %>%
    dplyr::filter(bool == 1) %>% dplyr::select(-bool)
  
  # set services to dates according to weekdays and start/end date
  date_service_df <- 
    dplyr::full_join(dates, service_ids_weekdays, by="weekday") %>% 
    dplyr::filter(date >= start_date & date <= end_date) %>% 
    dplyr::select(-weekday, -start_date, -end_date)
  
  if(!is.null(gtfs_obj$calendar_dates)) {
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
  
  if(nrow(date_service_df) == 0) {
    warning("No start and end dates defined in feed")
  }

  gtfs_obj$.$dates_services <- date_service_df
  
  return(gtfs_obj)
}

# Function to convert "HH:MM:SS" time strings to seconds.
hhmmss_to_seconds <- function(hhmmss_str) {
  as.numeric(substr(hhmmss_str, 0, 2)) * 3600 +
    as.numeric(substr(hhmmss_str, 4, 5)) * 60 +
    as.numeric(substr(hhmmss_str, 7, 8))
}

hhmmss_to_sec_split <- function(hhmmss_str) {
  sapply(strsplit(hhmmss_str, ":"), function(Y) {
    sum(as.numeric(Y) * c(3600, 60, 1))
  })
}
