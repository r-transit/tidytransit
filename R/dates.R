# Dates ####
parse_gtfsio_date = function(gtfsio_date) {
  if(inherits(gtfsio_date, "Date")) {
    return(gtfsio_date)
  }
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
  return(gtfs_obj)
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
    date_service_df <- dates %>%
      dplyr::inner_join(
        service_ids_weekdays, 
        by = dplyr::join_by(weekday, between(date, start_date, end_date))
      ) %>% 
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
