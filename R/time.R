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

#' Create a m:n table showing which service runs on which date.
#' 
#' @return gtfs_obj with added date_service data frame
create_date_service <- function(gtfs_obj) {
  stopifnot(is_gtfs_obj(gtfs_obj))
  
  # table to connect every date to corresponding services (all dates from earliest to latest)
  dates <- tibble(
    date = seq(
      min(gtfs_obj$calendar_df$start_date),
      max(gtfs_obj$calendar_df$end_date),
      1
    ),
    weekday = tolower(weekdays(date))
  )
  
  # gather services by weekdays
  service_ids_weekdays <-
    gather(
      gtfs_obj$calendar_df,
      key = "weekday",
      value = "bool",
      -c(service_id, start_date, end_date)
    ) %>%
    filter(bool == 1) %>% select(-bool)
  
  # set services to dates according to weekdays and start/end date
  date_service_df <- full_join(dates, service_ids_weekdays, by="weekday") %>% 
    filter(date > start_date & date < end_date) %>% 
    select(-weekday, -start_date, -end_date)
  
  # add calendar_dates additions (1) 
  additions = gtfs_obj$calendar_dates_df %>% filter(exception_type == 1) %>% select(-exception_type)
  if(nrow(additions) > 0) {
    date_service_df <- full_join(date_service_df, additions, by=c("date", "service_id"))
  }

  # remove calendar_dates exceptions (2) 
  exceptions = gtfs_obj$calendar_dates_df %>% filter(exception_type == 2) %>% select(-exception_type)
  if(nrow(exceptions) > 0) {
    date_service_df <- anti_join(date_service_df, exceptions, by=c("date", "service_id"))
  }
  
  gtfs_obj$date_service <- date_service_df
  
  return(gtfs_obj)
}