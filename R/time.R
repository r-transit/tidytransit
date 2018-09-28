#' Get a dataframe with lubridate dates for the gtfs stop_times_df 
#' 
#' @param stop_times_df a gtfsr$stop_times_df dataframe
#' @return an dataframe with arrival and departure time set to lubridate types
#' @keywords internal
#' @importFrom lubridate hms
gt_as_dt <- function(stop_times_df) {
  stop_times_dt <- stop_times_df %>% 
    dplyr::mutate(
      departure_time = lubridate::hms(.data$departure_time, quiet = TRUE),
      arrival_time = lubridate::hms(.data$arrival_time, quiet = TRUE)
    )
  return(stop_times_dt)
}

#' Filter stop times by hour of the day
#' 
#' @param stop_times_df a gtfsr$stop_times_df dataframe with lubridate arrival_time and departure_time
#' @return dataframe with only stop times within the hours specified, with time columns as lubridate periods
#' @keywords internal
filter_stop_times_by_hour <- function(stop_times, 
  start_hour, 
  end_hour) {
  stop_times_dt <- gt_as_dt(stop_times)
  stop_times <- stop_times[lubridate::hour(stop_times_dt$arrival_time) > start_hour &
      lubridate::hour(stop_times_dt$departure_time) < end_hour,]
  return(stop_times)
}

#' Add lubridate::hms columns to feed
#' 
#' Adds columns to stop_times (arrival_time_hms, departure_time_hms) and frequencies (start_time_hms, end_time_hms)
#' with times converted with lubridate::hms().
#' 
#' @return gtfs_obj with added hms times columns for stop_times_df and frequencies_df
#' @keywords internal
#' @importFrom lubridate hms
set_hms_times <- function(gtfs_obj) {
  stopifnot(is_gtfs_obj(gtfs_obj))
  
  str_to_seconds <- function(hhmmss_str) {
    sapply(
      strsplit(hhmmss_str, ":"), 
      function(Y) { sum(as.numeric(Y) * c(3600, 60, 1)) }
      )
  }
  
  gtfs_obj$stop_times_df$arrival_time_hms <- hms::hms(str_to_seconds(gtfs_obj$stop_times_df$arrival_time))
  gtfs_obj$stop_times_df$departure_time_hms <- hms::hms(str_to_seconds(gtfs_obj$stop_times_df$departure_time))
  
  if(!is.null(gtfs_obj$frequencies_df)) {
    gtfs_obj$frequencies_df$start_time_hms <- hms::hms(str_to_seconds(gtfs_obj$frequencies_df$start_time))
    gtfs_obj$frequencies_df$end_time_hms <- hms::hms(str_to_seconds(gtfs_obj$frequencies_df$end_time))
  }
  
  return(gtfs_obj)
}

#' Returns all possible date/service_id combinations as a data frame
#' 
#' Use it to summarise service. For example, get a count of the number of services for a date. See example. 
#' @return gtfs_obj with added date_service data frame
#' @param gtfs_obj a gtfs_object as read by read_gtfs
#' @export
#' @examples 
#' library(dplyr)
#' local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
#' nyc <- tidytransit:::read_gtfs(local_gtfs_path, local=TRUE)
#' nyc_services_by_date <- nyc %>% get_date_service_table()
#' # count the number of services running on each date
#' nyc_services_by_date %>% group_by(date) %>% count()
#'

get_date_service_table <- function(gtfs_obj) {
  stopifnot(is_gtfs_obj(gtfs_obj))
  
  if(all(is.na(gtfs_obj$calendar_df$start_date)) & all(is.na(gtfs_obj$calendar_df$start_date))) {
    # TODO validate no start_date and end_date defined in calendar.txt
    date_service_df <- dplyr::tibble(date=lubridate::ymd("19700101"), service_id="x") %>% dplyr::filter(service_id != "x")
  } else {
    # table to connect every date to corresponding services (all dates from earliest to latest)
    dates <- dplyr::tibble(
      date = seq(
        min(gtfs_obj$calendar_df$start_date, na.rm = T),
        max(gtfs_obj$calendar_df$end_date, na.rm = T),
        1
      ),
      weekday = tolower(weekdays(date))
    )
    
    # gather services by weekdays
    service_ids_weekdays <-
      tidyr::gather(
        gtfs_obj$calendar_df,
        key = "weekday",
        value = "bool",
        -c(service_id, start_date, end_date)
      ) %>%
      dplyr::filter(bool == 1) %>% dplyr::select(-bool)
    
    # set services to dates according to weekdays and start/end date
    date_service_df <- dplyr::full_join(dates, service_ids_weekdays, by="weekday") %>% 
      dplyr::filter(date > start_date & date < end_date) %>% 
      dplyr::select(-weekday, -start_date, -end_date)
  }
  
  # add calendar_dates additions (1) 
  additions = gtfs_obj$calendar_dates_df %>% filter(exception_type == 1) %>% select(-exception_type)
  if(nrow(additions) > 0) {
    date_service_df <- dplyr::full_join(date_service_df, additions, by=c("date", "service_id"))
  }
  
  # remove calendar_dates exceptions (2) 
  exceptions = gtfs_obj$calendar_dates_df %>% filter(exception_type == 2) %>% select(-exception_type)
  if(nrow(exceptions) > 0) {
    date_service_df <- dplyr::anti_join(date_service_df, exceptions, by=c("date", "service_id"))
  }
  
  if(nrow(date_service_df) == 0) {
    warning("No start and end dates defined in feed")
  }
  
  return(date_service_df)
}